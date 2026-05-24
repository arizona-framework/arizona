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

// Op codes (mirror include/arizona.hrl).
const OP_TEXT = 0;
const OP_SET_ATTR = 1;
const OP_REPLACE = 8;
// arizona_effect op codes (mirror include/arizona_effect.hrl).
const EFFECT_PUSH_EVENT = 0;

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
    }

    connect() {
        return new Promise((resolve, reject) => {
            this.ws = new WebSocket(this.wsUrl);
            this.ws.onopen = () => this.ws.send(JSON.stringify(['cached_fps', []]));
            this.ws.onerror = (e) => reject(new Error(`ws error: ${e.message || e}`));
            this.ws.onmessage = (e) => {
                if (e.data === '1') return; // pong
                const msg = JSON.parse(e.data);
                if (msg.o) this._applyOps(msg.o);
                if (this.root) resolve(this); // first frame applied
                this._flushWaiters();
            };
        });
    }

    close() {
        if (this.ws) this.ws.close();
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

    // Interpret a node's tap command prop (an arizona_effect command array) and
    // fire it, like the browser delegating an az-click.
    tap(node, prop = 'on_tap') {
        const cmd = node[prop];
        if (Array.isArray(cmd) && cmd[0] === EFFECT_PUSH_EVENT) {
            this.pushEvent(cmd[1]);
        } else {
            throw new Error(`unsupported tap command: ${JSON.stringify(cmd)}`);
        }
    }

    _flushWaiters() {
        this._waiters = this._waiters.filter((w) => !w());
    }

    _applyOps(ops) {
        for (const op of ops) {
            switch (op[0]) {
                case OP_REPLACE: {
                    const [, viewId, payload] = op;
                    this.viewId = viewId;
                    this.root = JSON.parse(this._interleave(payload));
                    this.registry = new Map();
                    indexByAz(this.root, this.registry);
                    break;
                }
                case OP_TEXT: {
                    const node = this._resolve(op[1]);
                    node.children = [op[2]];
                    break;
                }
                case OP_SET_ATTR: {
                    const node = this._resolve(op[1]);
                    node[op[2]] = op[3];
                    break;
                }
                default:
                    throw new Error(`unhandled op code: ${op[0]}`);
            }
        }
    }

    // target is "ViewId:az"; resolve the raw node by its az.
    _resolve(target) {
        const az = target.slice(target.indexOf(':') + 1);
        const node = this.registry.get(az);
        if (!node) throw new Error(`unknown az target: ${target}`);
        return node;
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
