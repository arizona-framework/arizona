// Reference native client for the Arizona "native" (JSON) render target.
//
// A minimal, faithful counterpart of the browser worker (assets/js/
// arizona-core.js + arizona-worker.js) for non-browser clients: it speaks the
// exact same WebSocket wire protocol, but reconstructs a JSON *widget tree*
// instead of HTML and applies ops against an `az -> node` registry instead of
// the DOM. This is the executable spec a real platform client (Kotlin/Compose,
// Swift/SwiftUI, ...) mirrors.
//
// Differences from the browser worker, by design:
//   - interleave JSON-encodes each dynamic value (string -> quoted, number ->
//     as-is) instead of string-concatenating;
//   - a `#slot` is a transparent fragment (its children splice into the parent);
//   - ops resolve nodes via an `az` registry, not `querySelector`.
//
// Native has no SSR page, so the client connects with `_az_reconnect=1` to make
// the live process send its `mount_and_render` output as the first OP_REPLACE.

// Op codes (mirror src/arizona.hrl).
const OP_TEXT = 0;
const OP_SET_ATTR = 1;
const OP_REM_ATTR = 2;
const OP_UPDATE = 3;
const OP_INSERT = 5;
const OP_REMOVE = 6;
const OP_ITEM_PATCH = 7;
const OP_REPLACE = 8;
const OP_MOVE = 9;
// arizona_effect op codes (mirror include/arizona_effect.hrl).
const EFFECT_PUSH_EVENT = 0;
const EFFECT_NAVIGATE = 10;

// Reconnect backoff: step delays (ms) capped at 10s, with ±20% jitter (mirrors
// assets/js/arizona-core.js).
function backoff(attempt) {
    const delays = [1000, 2000, 5000, 10000];
    const base = attempt < delays.length ? delays[attempt] : 10000;
    return Math.round(base * (0.8 + Math.random() * 0.4));
}

export class NativeClient {
    constructor(baseUrl, path) {
        // baseUrl like http://localhost:4041 -> ws://localhost:4041/ws
        this.wsUrl =
            baseUrl.replace(/^http/, 'ws') +
            '/ws?_az_path=' +
            encodeURIComponent(path) +
            '&_az_reconnect=1';
        this.fpCache = new Map(); // fingerprint -> { s, t }
        this.registry = new Map(); // az -> raw node (with #slot wrappers)
        this.root = null; // raw root node
        this.viewId = null;
        this._waiters = [];
        this._closing = false;
        this._reconnectAttempt = 0;
        this._reconnectTimer = null;
    }

    connect() {
        return new Promise((resolve, reject) => {
            this._resolveConnect = resolve;
            this._rejectConnect = reject;
            this._open();
        });
    }

    // Open (or reopen) the socket -- reused for the initial connect and each
    // reconnect. The reconnect path re-mounts via _az_reconnect=1 and re-applies
    // the OP_REPLACE the server sends.
    _open() {
        this.ws = new WebSocket(this.wsUrl);
        this.ws.onopen = () => {
            this._reconnectAttempt = 0;
            this.ws.send(JSON.stringify(['cached_fps', []]));
        };
        this.ws.onerror = (e) => {
            // Reject only the initial connect; once rendered, drops are handled
            // by onclose -> reconnect.
            if (!this.root) this._rejectConnect(new Error(`ws error: ${e.message || e}`));
        };
        this.ws.onmessage = (e) => {
            if (e.data === '1') return; // pong
            const msg = JSON.parse(e.data);
            if (msg.o) this._applyOps(msg.o);
            // Handler-returned effects (the "e" array): dispatch the portable
            // ones, skip web-only effects (set_title, dispatch_event, ...).
            if (msg.e) for (const cmd of msg.e) this._runEffect(cmd, false);
            if (this.root) this._resolveConnect(this); // first frame applied (no-op after)
            this._flushWaiters();
        };
        this.ws.onclose = (e) => {
            // No reconnect on an intentional close (1000) or a never-connected
            // initial failure; any other drop reconnects with backoff.
            if (this._closing || e.code === 1000 || !this.root) return;
            this._reconnectTimer = setTimeout(
                () => this._open(),
                backoff(this._reconnectAttempt++),
            );
        };
    }

    close() {
        this._closing = true;
        if (this._reconnectTimer) clearTimeout(this._reconnectTimer);
        if (this.ws) this.ws.close(1000);
    }

    // The user-visible widget tree: the raw tree with every #slot spliced away.
    tree() {
        return flatten(structuredClone(this.root));
    }

    // Resolve when `predicate(tree)` holds (checked after each applied frame).
    waitFor(predicate, timeoutMs = 2000) {
        return new Promise((resolve, reject) => {
            const check = () => predicate(this.tree());
            if (check()) return resolve(this.tree());
            const timer = setTimeout(() => reject(new Error('waitFor timeout')), timeoutMs);
            this._waiters.push(() => {
                if (check()) {
                    clearTimeout(timer);
                    resolve(this.tree());
                    return true;
                }
                return false;
            });
        });
    }

    // Send an event frame [ViewId, Event, Payload], as the browser's pushEvent does.
    pushEvent(event, payload = {}) {
        this.ws.send(JSON.stringify([this.viewId, event, payload]));
    }

    // SPA navigate: transition to a new view on the same socket (the server's
    // handle_navigate re-mounts and replies with OP_REPLACE). Mirrors the browser
    // sending ['navigate', {path, qs}].
    navigate(path) {
        const [p, qs = ''] = path.split('?');
        this.ws.send(JSON.stringify(['navigate', { path: p, qs }]));
    }

    // Interpret a node's tap command prop (an arizona_effect command array) and
    // fire it, like the browser delegating an az-click. Strict: an unexpected
    // command throws.
    tap(node, prop = 'on_tap') {
        this._runEffect(node[prop], true);
    }

    // Run one effect command (from a tap prop or the server's "e" array). `strict`
    // throws on an unsupported command (taps); non-strict skips it (web-only
    // effects in the "e" stream don't apply to native).
    _runEffect(cmd, strict) {
        if (Array.isArray(cmd) && cmd[0] === EFFECT_PUSH_EVENT) this.pushEvent(cmd[1]);
        else if (Array.isArray(cmd) && cmd[0] === EFFECT_NAVIGATE) this.navigate(cmd[1]);
        else if (strict) throw new Error(`unsupported command: ${JSON.stringify(cmd)}`);
    }

    _flushWaiters() {
        this._waiters = this._waiters.filter((w) => !w());
    }

    _applyOps(ops) {
        // Top-level ops address nodes as "ViewId:az" via the global registry.
        for (const op of ops) this._dispatch(op, (target) => this._resolve(target));
    }

    // Apply one op, resolving its target node via `resolve`. Top-level ops pass
    // "ViewId:az"; an OP_ITEM_PATCH's inner ops pass a bare az resolved within
    // the patched item (mirrors the browser worker's applyItemOps).
    _dispatch(op, resolve) {
        switch (op[0]) {
            case OP_REPLACE: {
                const [, viewId, payload] = op;
                this.viewId = viewId;
                this.root = JSON.parse(this._interleave(payload));
                this.registry = new Map();
                indexByAz(this.root, this.registry);
                break;
            }
            case OP_TEXT:
                // The value is usually a scalar, but a dynamic that is a nested
                // template (e.g. a conditional subtree) ships a {f,s,d} payload;
                // _decode handles both (the browser runs OP_TEXT through resolveHtml).
                resolve(op[1]).children = [this._decode(op[2])];
                break;
            case OP_UPDATE:
                // Re-render a node's content (e.g. a stream reset rebuilds the
                // whole each-list).
                resolve(op[1]).children = [this._decode(op[2])];
                break;
            case OP_SET_ATTR:
                resolve(op[1])[op[2]] = op[3];
                break;
            case OP_REM_ATTR:
                delete resolve(op[1])[op[2]];
                break;
            case OP_INSERT: {
                const items = itemList(resolve(op[1]));
                const pos = op[3];
                const item = this._decode(op[4]);
                if (pos === -1 || pos >= items.length) items.push(item);
                else items.splice(pos, 0, item);
                break;
            }
            case OP_REMOVE: {
                const items = itemList(resolve(op[1]));
                const i = items.findIndex((it) => it.az_key === op[2]);
                if (i !== -1) items.splice(i, 1);
                break;
            }
            case OP_MOVE: {
                const items = itemList(resolve(op[1]));
                const i = items.findIndex((it) => it.az_key === op[2]);
                if (i === -1) break;
                const [item] = items.splice(i, 1);
                const afterKey = op[3];
                if (afterKey === null) {
                    items.unshift(item);
                } else {
                    const r = items.findIndex((it) => it.az_key === afterKey);
                    if (r === -1) items.push(item);
                    else items.splice(r + 1, 0, item);
                }
                break;
            }
            case OP_ITEM_PATCH: {
                const items = itemList(resolve(op[1]));
                const item = items.find((it) => it.az_key === op[2]);
                if (item) this._applyInner(item, op[3]);
                break;
            }
            default:
                throw new Error(`unhandled op code: ${op[0]}`);
        }
    }

    // target is "ViewId:az"; resolve the raw node by its az.
    _resolve(target) {
        const az = target.slice(target.indexOf(':') + 1);
        const node = this.registry.get(az);
        if (!node) throw new Error(`unknown az target: ${target}`);
        return node;
    }

    // Apply an OP_ITEM_PATCH's inner ops, scoped to one keyed item: inner ops
    // carry bare az indices resolved within the item's own subtree.
    _applyInner(item, innerOps) {
        const reg = new Map();
        indexByAz(item, reg);
        for (const op of innerOps) this._dispatch(op, (az) => reg.get(az) || item);
    }

    // Decode an op payload: a {t:0} each-list -> array, a {f,s,d} template ->
    // node, a scalar -> itself.
    _decode(payload) {
        return JSON.parse(this._encodeValue(payload));
    }

    // Stitch statics + dynamics into a JSON string (the browser's zipTemplate,
    // but JSON-encoding each value). Recurses into nested {f,s,d} payloads and
    // each {t:0} item-lists. Returns a JSON string to be parsed by the caller.
    _interleave(payload) {
        const s = this._statics(payload);
        let out = s[0];
        payload.d.forEach((v, i) => {
            out += this._encodeValue(v) + s[i + 1];
        });
        return out;
    }

    _statics(payload) {
        if (payload.s) this.fpCache.set(payload.f, { s: payload.s, t: payload.t });
        const cached = this.fpCache.get(payload.f);
        if (!cached) throw new Error(`uncached fingerprint: ${payload.f}`);
        return cached.s;
    }

    _encodeValue(v) {
        if (v !== null && typeof v === 'object') {
            if (v.t === 0) {
                // an ?each expansion -> a JSON array of items the parent splices
                const s = this._statics(v);
                const items = v.d.map((itemD) => this._interleaveWith(s, itemD));
                return `[${items.join(',')}]`;
            }
            // a nested {f,s,d} template
            return this._interleave(v);
        }
        return JSON.stringify(v); // scalar: string -> quoted, number -> as-is
    }

    _interleaveWith(s, d) {
        let out = s[0];
        d.forEach((v, i) => {
            out += this._encodeValue(v) + s[i + 1];
        });
        return out;
    }
}

// Build an az -> node index over the raw tree (includes #slot nodes, which carry
// the az that OP_TEXT targets).
function indexByAz(node, reg) {
    if (node === null || typeof node !== 'object') return;
    if (Array.isArray(node)) {
        for (const c of node) indexByAz(c, reg);
        return;
    }
    if (node.az) reg.set(node.az, node);
    if (node.children) {
        for (const c of node.children) indexByAz(c, reg);
    }
}

// A stream container (#slot) holds its keyed items as the single each-array
// among its children; the keyed-list ops (insert/remove/move/patch) address it.
function itemList(container) {
    const items = container.children.find(Array.isArray);
    if (!items) throw new Error(`container ${container.az} has no item list`);
    return items;
}

// Splice every #slot's children into its parent, and flatten nested arrays
// (each expansions), producing the user-visible widget tree.
function flatten(node) {
    if (node === null || typeof node !== 'object' || !node.children) return node;
    node.children = node.children.flatMap(flattenChild);
    return node;
}

function flattenChild(child) {
    if (child !== null && typeof child === 'object' && child.type === '#slot') {
        return child.children.flatMap(flattenChild);
    }
    if (Array.isArray(child)) return child.flatMap(flattenChild);
    if (child !== null && typeof child === 'object' && child.children) return [flatten(child)];
    return [child];
}
