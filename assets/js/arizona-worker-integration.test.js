/**
 * Integration tests for arizona-worker.js. Mocks WebSocket + postMessage;
 * indexedDB is stubbed to a no-op (the worker only uses it for fire-and-forget
 * cache persistence, which these tests don't assert on).
 */
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

function installIndexedDBStub({ seed = [] } = {}) {
    const puts = [];
    const orig = globalThis.indexedDB;
    globalThis.indexedDB = {
        open() {
            const req = { result: null, onsuccess: null, onupgradeneeded: null, onerror: null };
            queueMicrotask(() => {
                req.result = {
                    createObjectStore() {},
                    transaction() {
                        return {
                            objectStore() {
                                return {
                                    openCursor() {
                                        const c = { result: null, onsuccess: null };
                                        let i = 0;
                                        const step = () => {
                                            if (i < seed.length) {
                                                const [k, v] = seed[i++];
                                                c.result = {
                                                    key: k,
                                                    value: v,
                                                    continue() {
                                                        queueMicrotask(step);
                                                    },
                                                };
                                            } else {
                                                c.result = null;
                                            }
                                            if (c.onsuccess) c.onsuccess();
                                        };
                                        queueMicrotask(step);
                                        return c;
                                    },
                                    put(value, key) {
                                        puts.push([key, value]);
                                    },
                                };
                            },
                        };
                    },
                };
                if (req.onupgradeneeded) req.onupgradeneeded();
                if (req.onsuccess) req.onsuccess();
            });
            return req;
        },
    };
    return {
        puts,
        restore() {
            globalThis.indexedDB = orig;
        },
    };
}

function installWebSocketStub() {
    const instances = [];
    class MockWS {
        constructor(url) {
            this.url = url;
            this.readyState = 0;
            this.sent = [];
            this.onopen = null;
            this.onmessage = null;
            this.onclose = null;
            this.onerror = null;
            instances.push(this);
        }
        send(data) {
            this.sent.push(data);
        }
        close(code = 1000) {
            this.readyState = 3;
            if (this.onclose) this.onclose({ code });
        }
        simulateOpen() {
            this.readyState = 1;
            if (this.onopen) this.onopen();
        }
        simulateMessage(data) {
            if (this.onmessage) this.onmessage({ data });
        }
    }
    const orig = globalThis.WebSocket;
    globalThis.WebSocket = MockWS;
    return {
        instances,
        latest() {
            return instances[instances.length - 1];
        },
        restore() {
            globalThis.WebSocket = orig;
        },
    };
}

function installPostMessageStub() {
    const posted = [];
    const origOnmessage = self.onmessage;
    const origPostMessage = self.postMessage;
    self.postMessage = (data) => posted.push(data);
    return {
        posted,
        send(msg) {
            self.onmessage({ data: msg });
        },
        restore() {
            self.onmessage = origOnmessage;
            self.postMessage = origPostMessage;
        },
    };
}

async function loadWorker() {
    vi.resetModules();
    return await import('./arizona-worker.js');
}

describe('arizona-worker', () => {
    let idb, ws, slf;

    beforeEach(async () => {
        idb = installIndexedDBStub();
        ws = installWebSocketStub();
        slf = installPostMessageStub();
        vi.useFakeTimers({
            toFake: ['setTimeout', 'clearTimeout', 'setInterval', 'clearInterval'],
        });
        await loadWorker();
    });

    afterEach(() => {
        vi.useRealTimers();
        slf.restore();
        ws.restore();
        idb.restore();
    });

    it('connect message opens a WebSocket rebuilt from self.location', () => {
        slf.send([0, 'ws://ignored-host/ws?_az_path=%2F']);
        expect(ws.instances).toHaveLength(1);
        expect(ws.latest().url).toContain('/ws?_az_path=%2F');
    });

    it('WS open posts [1, false] to the main thread', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        expect(slf.posted).toContainEqual([1, false]);
    });

    it('pong messages are silently dropped, no postMessage', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        ws.latest().simulateMessage('1');
        expect(slf.posted).toHaveLength(0);
    });

    it('decoded ops + effects are posted to main thread', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        ws.latest().simulateMessage(JSON.stringify({ o: [[0, 'v:0', 'hi']], e: [['reload']] }));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved).toBeDefined();
        // The worker tags each OP_TEXT with its isHtml flag (op[3]); a bare-string value
        // is text, so the flag is false -- the client text-nodes it (safe, no injection).
        expect(resolved[1]).toEqual([[0, 'v:0', 'hi', false]]);
        expect(resolved[2]).toEqual([['reload']]);
        expect(resolved[3]).toBe(false);
    });

    it('an HTML-fragment OP_TEXT payload (object) is resolved and flagged isHtml', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        // A `?raw` value arrives as a `{raw}` object (an HTML fragment); the worker
        // unwraps it to a string AND marks op[3]=true so the main thread innerHTMLs it.
        ws.latest().simulateMessage(JSON.stringify({ o: [[0, 'v:0', { raw: '<b>x</b>' }]] }));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[1]).toEqual([[0, 'v:0', '<b>x</b>', true]]);
    });

    it('send message [1, json] forwards to WebSocket when ready', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.send([1, '["navigate",{"path":"/x","qs":""}]']);
        expect(ws.latest().sent).toContain('["navigate",{"path":"/x","qs":""}]');
    });

    it('send message is dropped before socket is open', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        slf.send([1, 'dropped']);
        expect(ws.latest().sent).toHaveLength(0);
    });

    it('close message [2, code] closes the WebSocket', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.send([2, 1000]);
        expect(ws.latest().readyState).toBe(3);
    });

    it('close with 1000 does not schedule reconnect', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.send([2, 1000]);
        vi.advanceTimersByTime(10000);
        expect(ws.instances).toHaveLength(1);
    });

    it('close with non-1000 schedules reconnect with _az_reconnect=1', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        expect(ws.instances.length).toBeGreaterThanOrEqual(2);
        expect(ws.latest().url).toContain('_az_reconnect=1');
    });

    it('W_UPDATE_PATH replaces the user qs on reconnect, keeping framework params', () => {
        // Connected on /search?q=a (a page param) with a framework caps param.
        slf.send([0, 'ws://host/ws?_az_path=%2Fsearch&q=a&_az_caps=%7B%7D']);
        ws.latest().simulateOpen();
        // An SPA navigation to /items?page=2 updates path + qs.
        slf.send([3, { path: '/items', qs: 'page=2' }]);
        // A blip forces a reconnect, whose URL must reflect the navigation.
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        const url = ws.latest().url;
        expect(url).toContain('_az_path=%2Fitems'); // navigated path
        expect(url).toContain('page=2'); // navigated qs is present
        expect(url).not.toContain('q=a'); // the stale page param is dropped
        expect(url).toContain('_az_caps='); // framework param preserved
    });

    it('W_UPDATE_PATH lays down connect() extras carried in the posted qs', () => {
        // arizona.js merges connect() extras into the qs it posts, so a constant
        // connection param survives while the page qs is replaced.
        slf.send([0, 'ws://host/ws?_az_path=%2Fsearch&q=a&token=abc']);
        ws.latest().simulateOpen();
        slf.send([3, { path: '/items', qs: 'page=2&token=abc' }]);
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        const url = ws.latest().url;
        expect(url).toContain('token=abc'); // connect extra preserved
        expect(url).toContain('page=2');
        expect(url).not.toContain('q=a');
    });

    it('does not reset the backoff on socket open without a frame', () => {
        // backoff(0) in [800,1200), backoff(1) in [1600,2400) -- disjoint, so the
        // fire time distinguishes the attempt index. A socket that opens but never
        // frames must NOT reset the counter, so the second drop backs off further.
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1006); // attempt 0 -> backoff(0)
        vi.advanceTimersByTime(1200);
        expect(ws.instances).toHaveLength(2); // reconnected

        ws.latest().simulateOpen(); // open again, still no frame
        ws.latest().close(1006); // attempt 1 (NOT reset on open) -> backoff(1)
        vi.advanceTimersByTime(1300); // < 1600: too soon for backoff(1)
        expect(ws.instances).toHaveLength(2);
        vi.advanceTimersByTime(1200); // now past backoff(1)
        expect(ws.instances).toHaveLength(3);
    });

    it('resets the backoff once a real frame arrives', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1006); // attempt 0 -> backoff(0)
        vi.advanceTimersByTime(1200);
        expect(ws.instances).toHaveLength(2);

        ws.latest().simulateOpen();
        ws.latest().simulateMessage(JSON.stringify({ o: [] })); // a real frame resets the counter
        ws.latest().close(1006); // attempt back to 0 -> backoff(0)
        vi.advanceTimersByTime(1200); // backoff(0) < 1200 fires
        expect(ws.instances).toHaveLength(3);
    });

    it('update-path message [3, {path, qs}] rewrites the cached URL', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2Fold']);
        slf.send([3, { path: '/new', qs: '' }]);
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        expect(ws.latest().url).toContain('_az_path=%2Fnew');
    });

    it('update-path targets _az_path by exact key and replaces a like-named user param', () => {
        slf.send([0, 'ws://host/ws?upload_path=%2Ffoo&_az_path=%2Fold']);
        // Navigate to a page that also carries an `upload_path` param: `_az_path`
        // is updated by its exact key (never confused with the user param), and
        // the stale page value is replaced by the navigated-to one.
        slf.send([3, { path: '/new', qs: 'upload_path=%2Fbar' }]);
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        const url = ws.latest().url;
        expect(url).toContain('_az_path=%2Fnew');
        expect(url).toContain('upload_path=%2Fbar');
        expect(url).not.toContain('upload_path=%2Ffoo');
    });

    it('heartbeat sends ping after 30s, closes if pong not received before next tick', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        vi.advanceTimersByTime(30000);
        expect(ws.latest().sent).toContain('0');
        vi.advanceTimersByTime(30000);
        expect(ws.latest().readyState).toBe(3);
    });

    it('ITEM_PATCH op recursively resolves inner ops including nested ITEM_PATCH', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        const msg = {
            o: [
                [
                    7,
                    'v:0',
                    'k1',
                    [
                        [0, 'v:0', 'text-payload'],
                        [3, 'v:0', 'update-payload'],
                        [5, 'v:0', 'anchor', 'before', 'insert-payload'],
                        [7, 'v:0', 'k2', [[0, 'v:0', 'nested-text']]],
                    ],
                ],
            ],
        };
        ws.latest().simulateMessage(JSON.stringify(msg));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[1][0][3][0][2]).toBe('text-payload');
        expect(resolved[1][0][3][1][2]).toBe('update-payload');
        expect(resolved[1][0][3][2][4]).toBe('insert-payload');
        expect(resolved[1][0][3][3][3][0][2]).toBe('nested-text');
    });

    it('LIST_PATCH op resolves each sub-op payload (ITEM_PATCH inner ops + INSERT html)', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        const msg = {
            o: [
                [
                    10,
                    'v:0',
                    [
                        [7, 0, [[0, 'v:0', 'patch-text']]],
                        // INSERT carries item HTML at sub[2] (vs [4] for the keyed
                        // stream form). A {raw} object proves resolveHtml ran.
                        [5, 2, { raw: '<li>new</li>' }],
                        [6, 1],
                    ],
                ],
            ],
        };
        ws.latest().simulateMessage(JSON.stringify(msg));
        const resolved = slf.posted.find((m) => m[0] === 0);
        const subOps = resolved[1][0][2];
        // ITEM_PATCH inner OP_TEXT resolved + isHtml-flagged (bare string -> false).
        expect(subOps[0][2][0]).toEqual([0, 'v:0', 'patch-text', false]);
        // INSERT object payload resolved to a string in place at sub[2].
        expect(subOps[1][2]).toBe('<li>new</li>');
        // REMOVE sub-op has no payload to resolve and is left intact.
        expect(subOps[2]).toEqual([6, 1]);
    });

    it('OP_REPLACE op is resolved', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        ws.latest().simulateMessage(JSON.stringify({ o: [[8, 'v:0', 'replace-html']] }));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[1][0][2]).toBe('replace-html');
    });

    it('reconnect: after successful reopen, next message flags firstAfterReconnect=true', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        slf.posted.length = 0;
        ws.latest().simulateOpen();
        ws.latest().simulateMessage(JSON.stringify({ o: [] }));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[3]).toBe(true);
    });

    it('close posts [2, code] to main thread', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        ws.latest().close(1006);
        expect(slf.posted).toContainEqual([2, 1006]);
    });

    it('fingerprinted template writes fpId + statics to IndexedDB via onPersist', async () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().simulateMessage(
            JSON.stringify({
                o: [[0, 'v:0', { f: 'fp_new', s: ['<p>', '</p>'], d: ['hi'] }]],
            }),
        );
        // idbPut runs via setOnPersist callback; its getDB() chain has to resolve
        vi.useRealTimers();
        await new Promise((r) => setTimeout(r, 0));
        expect(idb.puts.some(([k]) => k === 'fp_new')).toBe(true);
    });

    it('hydrates fp cache from IndexedDB cursor entries on boot', async () => {
        // Re-install stubs with a pre-seeded cursor row so idbLoadAll iterates.
        idb.restore();
        idb = installIndexedDBStub({
            seed: [['pre_seeded', { s: ['<em>', '</em>'] }]],
        });
        await loadWorker();

        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        // Flush microtasks (IDB cursor + loadFpEntries + sendCachedFps).
        vi.useRealTimers();
        await new Promise((r) => setTimeout(r, 0));
        const sentFps = ws.latest().sent.find((s) => s.startsWith('["cached_fps"'));
        expect(sentFps).toContain('pre_seeded');
    });

    it('sends cached_fps over WS once fp cache is non-empty after hydration', async () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        // Seed the cache via a resolved fingerprinted message so sendCachedFps has something to ship
        ws.latest().simulateMessage(
            JSON.stringify({ o: [[0, 'v:0', { f: 'fp_z', s: ['<i>', '</i>'], d: ['x'] }]] }),
        );
        // Reconnect resets _fpsSent; next open should re-ship cached fps
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        ws.latest().simulateOpen();
        const sentFps = ws.latest().sent.find((s) => s.startsWith('["cached_fps"'));
        expect(sentFps).toContain('fp_z');
    });
});
