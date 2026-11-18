/**
 * Integration tests for arizona-worker.js. Mocks WebSocket + postMessage;
 * indexedDB is stubbed with an in-memory store that records puts and deletes,
 * so the cache hydration + eviction path is assertable.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { FP_CACHE_MAX } from './arizona-core.js';

function installIndexedDBStub({ seed = [] } = {}) {
    const puts = [];
    const deletes = [];
    const orig = globalThis.indexedDB;
    globalThis.indexedDB = {
        open() {
            const req = { result: null, onsuccess: null, onupgradeneeded: null, onerror: null };
            queueMicrotask(() => {
                req.result = {
                    objectStoreNames: { contains: () => false },
                    createObjectStore() {},
                    deleteObjectStore() {},
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
                                    delete(key) {
                                        deletes.push(key);
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
        deletes,
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
            // A dead peer never answers the close handshake: the browser keeps the
            // socket CLOSING and fires `close` only once it gives up (20-60s).
            if (MockWS.deadPeer) {
                this.readyState = 2;
                return;
            }
            this.readyState = 3;
            if (this.onclose) this.onclose({ code });
        }
        /** The browser gives up on a dead peer's close handshake. */
        giveUp(code = 1006) {
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
    MockWS.deadPeer = false;
    const orig = globalThis.WebSocket;
    globalThis.WebSocket = MockWS;
    return {
        instances,
        latest() {
            return instances[instances.length - 1];
        },
        /** @param {boolean} dead */
        deadPeer(dead) {
            MockWS.deadPeer = dead;
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

    it("forwards a frame's az-* delta beside the ops that render the markup", () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        // Newly proven names ride the frame itself (key `a`), deduped per socket by
        // the server: the client learns a DOM event type from the very frame that
        // can first produce the markup declaring it. The worker only relays -- the
        // main thread decides which names are DOM events, so that triage lives in
        // one place.
        ws.latest().simulateMessage(
            JSON.stringify({
                o: [[0, 'v:0', { f: 'fpA', s: ['<b az-toggle="x">', '</b>'], d: ['1'] }]],
                a: ['az-toggle'],
            }),
        );
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[1]).toEqual([[0, 'v:0', '<b az-toggle="x">1</b>', true]]);
        expect(resolved[4]).toEqual(['az-toggle']);
    });

    it('adds no names of its own: a frame without a delta forwards null', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        const frame = JSON.stringify({
            o: [[0, 'v:0', { f: 'fpB', s: ['<b az-close="x">', '</b>'], d: ['1'] }]],
            a: ['az-close'],
        });
        ws.latest().simulateMessage(frame);
        slf.posted.length = 0;
        // The server already sent az-close on the frame above and dedupes per
        // socket, so the follow-up frame carries no `a` -- and the worker must not
        // re-derive names from the payload bytes it resolves.
        ws.latest().simulateMessage(JSON.stringify({ o: [[0, 'v:0', { f: 'fpB', d: ['2'] }]] }));
        expect(slf.posted.find((m) => m[0] === 0)[4]).toBeNull();
    });

    it('sends null in field 4 on a frame carrying no names', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        // Every frame after connect. The field must be falsy so the main thread's
        // `if (msg[4])` skips it rather than re-running the triage on every patch.
        ws.latest().simulateMessage(JSON.stringify({ o: [[0, 'v:0', 'hi']] }));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[4]).toBeNull();
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

    it('gives the reconnect flag to the frame that carries ops, not the first one', () => {
        // the connect message's third element marks a reconnect (a bfcache restore)
        slf.send([0, 'ws://host/ws?_az_path=%2F', true]);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        // A reconnect opens with the declared az-* names and no ops. If that frame
        // claimed the flag, the main thread would restore form state against a DOM
        // the following OP_REPLACE discards, and `_savedForms.clear()` at the end of
        // the restore means the user's typed input is gone rather than merely late.
        ws.latest().simulateMessage(JSON.stringify({ a: ['az-click'] }));
        const namesFrame = slf.posted.find((m) => m[0] === 0);
        expect(namesFrame[1]).toBeNull();
        expect(namesFrame[3]).toBe(false);

        slf.posted.length = 0;
        ws.latest().simulateMessage(JSON.stringify({ o: [[0, 'v:0', 'hi']] }));
        const opsFrame = slf.posted.find((m) => m[0] === 0);
        expect(opsFrame[3]).toBe(true);
    });

    it('send message [1, json] forwards to WebSocket when ready', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.send([1, '["navigate",{"path":"/x","qs":""}]']);
        expect(ws.latest().sent).toContain('["navigate",{"path":"/x","qs":""}]');
    });

    it('send message is dropped before socket is open, with a warning', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        slf.send([1, '["v1","inc",{"password":"hunter2"}]']);
        expect(ws.latest().sent).toHaveLength(0);
        // The drop is announced -- silence here reads as a handler that never fired.
        expect(warn).toHaveBeenCalledOnce();
        const line = warn.mock.calls[0][0];
        expect(line).toContain('socket not open');
        // ... but the frame's own bytes never reach the console.
        expect(line).not.toContain('hunter2');
        expect(line).not.toContain('inc');
        warn.mockRestore();
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

    it('connect flagged as reconnect opens with _az_reconnect=1 and reports it', () => {
        // A bfcache restore respawns the worker against an already-rendered
        // page: the main thread flags the connect so the server sends the
        // full-page resync, exactly like an in-worker backoff reconnect.
        slf.send([0, 'ws://host/ws?_az_path=%2Fnext', true]);
        expect(ws.latest().url).toContain('_az_reconnect=1');
        ws.latest().simulateOpen();
        expect(slf.posted).toContainEqual([1, true]);
    });

    it('close with non-1000 schedules reconnect with _az_reconnect=1', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        expect(ws.instances.length).toBeGreaterThanOrEqual(2);
        expect(ws.latest().url).toContain('_az_reconnect=1');
    });

    /** Send a navigate frame tagged with the page it asks for (`qs` as main merges it). */
    function sendNav(ref, path, qs) {
        slf.send([
            1,
            JSON.stringify(['navigate', { path, qs }, ref]),
            { ref, path, qs, closes: 0 },
        ]);
    }

    /** Close the open socket abnormally and return the URL the reconnect dials. */
    function redialUrl() {
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        return ws.latest().url;
    }

    it('a navigate reply that put its page on screen moves the reconnect URL, keeping framework params', () => {
        // Connected on /search?q=a (a page param) with a framework caps param.
        slf.send([0, 'ws://host/ws?_az_path=%2Fsearch&q=a&_az_caps=%7B%7D']);
        ws.latest().simulateOpen();
        sendNav(7, '/items', 'page=2');
        slf.posted.length = 0;
        ws.latest().simulateMessage(JSON.stringify({ o: [], r: 7 }));
        expect(slf.posted).toEqual([[0, [], null, false, null, 7, true]]);
        const url = redialUrl();
        expect(url).toContain('_az_path=%2Fitems'); // navigated path
        expect(url).toContain('page=2'); // navigated qs is present
        expect(url).not.toContain('q=a'); // the stale page param is dropped
        expect(url).toContain('_az_caps='); // framework param preserved
    });

    it('a navigate whose reply has not come leaves the reconnect URL on the page on screen', () => {
        // The reply is lost with the socket: the DOM still holds /search, which is
        // the view the reconnect must resync.
        slf.send([0, 'ws://host/ws?_az_path=%2Fsearch&q=a']);
        ws.latest().simulateOpen();
        sendNav(7, '/items', 'page=2');
        const url = redialUrl();
        expect(url).toContain('_az_path=%2Fsearch');
        expect(url).toContain('q=a');
        expect(url).not.toContain('page=2');
    });

    it('a reply that navigates again moves nothing', () => {
        // A redirect, or a full load the server degraded the frame to: the page it
        // asked for never went on screen.
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        ws.latest().simulateOpen();
        sendNav(7, '/protected', '');
        slf.posted.length = 0;
        ws.latest().simulateMessage(
            JSON.stringify({ e: [[10, '/login', { replace: true }]], r: 7 }),
        );
        sendNav(8, '/wrapped', '');
        ws.latest().simulateMessage(
            JSON.stringify({ e: [[20, {}, [[24, '/elsewhere', {}]]]], r: 8 }),
        );
        expect(slf.posted.map((m) => m[6])).toEqual([false, false]);
        expect(redialUrl()).toContain('_az_path=%2Fstart');
    });

    it('a reply whose navigate only names a fragment of the page it put on screen moves the URL', () => {
        // A patch whose handle_update/3 returns navigate("#bottom"): the page is on
        // screen, the effect only scrolls it.
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        ws.latest().simulateOpen();
        slf.send([
            1,
            JSON.stringify(['patch', { path: '/hash', qs: 's=two' }, 7]),
            { ref: 7, path: '/hash', qs: 's=two', href: 'http://host/hash?s=two', closes: 0 },
        ]);
        slf.posted.length = 0;
        ws.latest().simulateMessage(
            JSON.stringify({
                o: [],
                e: [
                    [10, '#bottom', {}],
                    [20, {}, [24, '?s=two#x', {}]],
                ],
                r: 7,
            }),
        );
        expect(slf.posted.map((m) => m[6])).toEqual([true]);
        const url = redialUrl();
        expect(url).toContain('_az_path=%2Fhash');
        expect(url).toContain('s=two');
    });

    it('resolves a reply navigate against the document base, as the main thread does', () => {
        // Under <base href="/">, on /docs/intro, "docs/intro#sec" names a fragment of
        // the page; resolved against the page itself it would be /docs/docs/intro.
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        ws.latest().simulateOpen();
        slf.send([
            1,
            JSON.stringify(['patch', { path: '/docs/intro', qs: '' }, 7]),
            {
                ref: 7,
                closes: 0,
                path: '/docs/intro',
                qs: '',
                href: 'http://host/docs/intro',
                base: 'http://host/',
            },
        ]);
        slf.posted.length = 0;
        ws.latest().simulateMessage(
            JSON.stringify({ o: [], e: [[10, 'docs/intro#sec', {}]], r: 7 }),
        );
        expect(slf.posted.map((m) => m[6])).toEqual([true]);
        expect(redialUrl()).toContain('_az_path=%2Fdocs%2Fintro');
    });

    it('a fragment of another page, or a full load of the same one, still navigates', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        ws.latest().simulateOpen();
        const tag = (ref) => ({ ref, path: '/hash', qs: '', href: 'http://host/hash', closes: 0 });
        slf.posted.length = 0;
        slf.send([1, JSON.stringify(['patch', { path: '/hash', qs: '' }, 7]), tag(7)]);
        ws.latest().simulateMessage(JSON.stringify({ e: [[10, '/other#x', {}]], r: 7 }));
        slf.send([1, JSON.stringify(['patch', { path: '/hash', qs: '' }, 8]), tag(8)]);
        ws.latest().simulateMessage(JSON.stringify({ e: [[10, '?#x', {}]], r: 8 }));
        slf.send([1, JSON.stringify(['patch', { path: '/hash', qs: '' }, 9]), tag(9)]);
        ws.latest().simulateMessage(JSON.stringify({ e: [[10, '#x', { full: true }]], r: 9 }));
        expect(slf.posted.map((m) => m[6])).toEqual([false, false, false]);
        expect(redialUrl()).toContain('_az_path=%2Fstart');
    });

    it('reports every connect attempt it starts, and whether the backoff was at its cap', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        expect(slf.posted).toEqual([[4, false]]);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        // Never opening, the attempts walk the backoff: 1 s, 2 s, 5 s, then 10 s.
        ws.latest().close(1006);
        const capped = [];
        for (let n = 0; n < 5; n++) {
            vi.advanceTimersByTime(12000);
            capped.push(slf.posted.filter((m) => m[0] === 4).at(-1)[1]);
            ws.latest().close(1006);
        }
        expect(capped).toEqual([false, false, false, true, true]);
    });

    it('a crash (4500) reports its code alone and dials the page on screen', () => {
        // A navigation still owed a reply at a close is not one the server failed (it
        // answers one it could not complete): main sends it again once the page on screen is
        // live. The reconnect does not dial it, even to the page on screen's own path.
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart&q=a']);
        ws.latest().simulateOpen();
        sendNav(7, '/items', 'page=2');
        sendNav(8, '/start', 'x=1');
        slf.posted.length = 0;
        ws.latest().close(4500);
        expect(slf.posted).toEqual([[2, 4500]]);
        vi.advanceTimersByTime(10000);
        expect(ws.latest().url).toContain('_az_path=%2Fstart');
        expect(ws.latest().url).toContain('q=a');
        expect(ws.latest().url).not.toContain('x=1');
    });

    for (const code of [1006, 4501]) {
        it(`a ${code} close with a navigation to the same path owed dials nothing`, () => {
            // The reconnect after any close resyncs the page on screen, and main
            // sends the navigation again once it is live.
            slf.send([0, 'ws://host/ws?_az_path=%2Fstart&q=a']);
            ws.latest().simulateOpen();
            sendNav(8, '/start', 'x=1');
            ws.latest().close(code);
            vi.advanceTimersByTime(10000);
            expect(ws.latest().url).toContain('q=a');
            expect(ws.latest().url).not.toContain('x=1');
        });
    }

    it('drops a navigation main posted before it saw a close the worker reported', () => {
        // Main is busy while the socket closes and reopens: its navigation, posted
        // for the dead socket, would reach the new one, and main then sends it
        // again as that close says. The tag names the closes main had seen.
        vi.spyOn(console, 'warn').mockImplementation(() => {});
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart', false, 2]);
        ws.latest().simulateOpen();
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        ws.latest().simulateOpen();
        const nav = (closes) => [
            1,
            JSON.stringify(['navigate', { path: '/next', qs: '' }, 5]),
            { ref: 5, path: '/next', qs: '', closes },
        ];
        slf.send(nav(2));
        expect(ws.latest().sent).toEqual([]);
        slf.send(nav(3));
        expect(ws.latest().sent).toEqual([
            JSON.stringify(['navigate', { path: '/next', qs: '' }, 5]),
        ]);
    });

    it('any other close reports no page and keeps the reconnect on the page on screen', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        ws.latest().simulateOpen();
        sendNav(7, '/items', '');
        slf.posted.length = 0;
        ws.latest().close(4501);
        expect(slf.posted).toEqual([[2, 4501]]);
        vi.advanceTimersByTime(10000);
        expect(ws.latest().url).toContain('_az_path=%2Fstart');
    });

    it('a navigate sent while the socket is down is not tracked, nor one a close lost', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2Fstart']);
        sendNav(6, '/never-sent', '');
        ws.latest().simulateOpen();
        sendNav(7, '/lost', '');
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        // Frames carrying those refs on the new socket answer nothing it sent.
        ws.latest().simulateMessage(JSON.stringify({ o: [], r: 6 }));
        ws.latest().simulateMessage(JSON.stringify({ o: [], r: 7 }));
        expect(slf.posted.map((m) => m[6])).toEqual([false, false]);
        expect(redialUrl()).toContain('_az_path=%2Fstart');
    });

    it('lays down connect() extras carried in the tagged qs', () => {
        // arizona.js merges connect() extras into the qs it tags, so a constant
        // connection param survives while the page qs is replaced.
        slf.send([0, 'ws://host/ws?_az_path=%2Fsearch&q=a&token=abc']);
        ws.latest().simulateOpen();
        sendNav(7, '/items', 'page=2&token=abc');
        ws.latest().simulateMessage(JSON.stringify({ o: [], r: 7 }));
        const url = redialUrl();
        expect(url).toContain('token=abc'); // connect extra preserved
        expect(url).toContain('page=2');
        expect(url).not.toContain('q=a');
    });

    // backoff(0) in [800,1200), backoff(1) in [1600,2400): the wait tells the
    // attempt index apart.
    /** Close the open socket with `code`; return how long the worker waits to reopen. */
    function reopenDelay(code) {
        const n = ws.instances.length;
        ws.latest().close(code);
        let waited = 0;
        while (ws.instances.length === n && waited < 60000) {
            vi.advanceTimersByTime(50);
            waited += 50;
        }
        return waited;
    }
    const FIRST_STEP = [800, 1200];
    const SECOND_STEP = [1600, 2400];
    const inStep = (ms, [lo, hi]) => ms >= lo && ms <= hi;

    /** Open a connection that already backed off once (attempt 1 is next). */
    function openAfterOneDrop() {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        const first = reopenDelay(1006); // never mounted -> backoff(0)
        ws.latest().simulateOpen();
        return first;
    }
    /** The main thread's word that the open socket's view mounted. */
    const mounted = () => slf.send([5]);

    it('does not reset the backoff on a socket that never mounted', () => {
        expect(inStep(openAfterOneDrop(), FIRST_STEP)).toBe(true);
        expect(inStep(reopenDelay(1006), SECOND_STEP)).toBe(true);
    });

    it('frames alone do not reset the backoff: only the mount signal does', () => {
        // A flagged reconnect frames its az-* names before the deferred mount.
        openAfterOneDrop();
        ws.latest().simulateMessage(JSON.stringify({ a: ['az-click'] }));
        ws.latest().simulateMessage(JSON.stringify({ o: [] }));
        expect(inStep(reopenDelay(1006), SECOND_STEP)).toBe(true);
    });

    it('a drop after a mount resets the backoff', () => {
        openAfterOneDrop();
        mounted();
        expect(inStep(reopenDelay(1006), FIRST_STEP)).toBe(true);
    });

    it('a mount failure (4501) on a long-lived session keeps backing off', () => {
        const now = vi.spyOn(Date, 'now').mockReturnValue(1_000_000);
        try {
            openAfterOneDrop();
            mounted();
            now.mockReturnValue(1_000_000 + 60_000);
            expect(inStep(reopenDelay(4501), SECOND_STEP)).toBe(true);
        } finally {
            now.mockRestore();
        }
    });

    it('a crash (4500) right after the mount keeps backing off', () => {
        const now = vi.spyOn(Date, 'now').mockReturnValue(1_000_000);
        try {
            openAfterOneDrop();
            mounted();
            now.mockReturnValue(1_000_000 + 9_999);
            expect(inStep(reopenDelay(4500), SECOND_STEP)).toBe(true);
        } finally {
            now.mockRestore();
        }
    });

    it('a crash (4500) long after the mount resets the backoff', () => {
        const now = vi.spyOn(Date, 'now').mockReturnValue(1_000_000);
        try {
            openAfterOneDrop();
            mounted();
            now.mockReturnValue(1_000_000 + 10_000);
            expect(inStep(reopenDelay(4500), FIRST_STEP)).toBe(true);
        } finally {
            now.mockRestore();
        }
    });

    it('the stable window restarts at each mount, not at a path update', () => {
        // A navigate's mount restarts it; a patch (a path update, no mount) must not.
        const now = vi.spyOn(Date, 'now').mockReturnValue(1_000_000);
        try {
            openAfterOneDrop();
            mounted();
            now.mockReturnValue(1_000_000 + 60_000);
            slf.send([3, { path: '/patched', qs: '' }]);
            now.mockReturnValue(1_000_000 + 61_000);
            expect(inStep(reopenDelay(4500), FIRST_STEP)).toBe(true);

            ws.latest().simulateOpen();
            mounted();
            now.mockReturnValue(1_000_000 + 120_000);
            mounted(); // a navigate mounted a new view
            now.mockReturnValue(1_000_000 + 121_000);
            expect(inStep(reopenDelay(4500), SECOND_STEP)).toBe(true);
        } finally {
            now.mockRestore();
        }
    });

    it('a mount signal for a socket that already closed is dropped', () => {
        openAfterOneDrop();
        ws.latest().close(1006);
        mounted(); // late: no socket is open
        vi.advanceTimersByTime(2400);
        ws.latest().simulateOpen();
        expect(inStep(reopenDelay(1006), [4000, 6000])).toBe(true);
    });

    it('a navigate reply targets _az_path by exact key and replaces a like-named user param', () => {
        slf.send([0, 'ws://host/ws?upload_path=%2Ffoo&_az_path=%2Fold']);
        ws.latest().simulateOpen();
        // Navigate to a page that also carries an `upload_path` param: `_az_path`
        // is updated by its exact key (never confused with the user param), and
        // the stale page value is replaced by the navigated-to one.
        slf.send([
            1,
            JSON.stringify(['navigate', { path: '/new', qs: 'upload_path=%2Fbar' }, 7]),
            { ref: 7, path: '/new', qs: 'upload_path=%2Fbar', closes: 0 },
        ]);
        ws.latest().simulateMessage(JSON.stringify({ o: [], r: 7 }));
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        const url = ws.latest().url;
        expect(url).toContain('_az_path=%2Fnew');
        expect(url).toContain('upload_path=%2Fbar');
        expect(url).not.toContain('upload_path=%2Ffoo');
    });

    it('acknowledges [4] with a ping and reports its pong as [3]', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.send([1, '["page","changed",{}]']);
        slf.send([4]);
        // The ping goes out right after the frame it acknowledges.
        expect(ws.latest().sent.slice(-2)).toEqual(['["page","changed",{}]', '0']);
        slf.posted.length = 0;
        ws.latest().simulateMessage('1');
        expect(slf.posted).toEqual([[3]]);
    });

    it("a heartbeat's pong answers the heartbeat, not a later acknowledgement", () => {
        // Pongs come back in ping order: a heartbeat ping sent before [4] gets the
        // first pong, which must not be taken for the acknowledgement.
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        vi.advanceTimersByTime(30000); // heartbeat ping
        slf.send([4]); // acknowledgement ping
        slf.posted.length = 0;
        ws.latest().simulateMessage('1'); // the heartbeat's pong
        expect(slf.posted).toEqual([]);
        ws.latest().simulateMessage('1'); // the acknowledgement's pong
        expect(slf.posted).toEqual([[3]]);
    });

    it("a new socket forgets the old one's outstanding pings", () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.send([4]);
        ws.latest().close(1006);
        vi.advanceTimersByTime(1200);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        ws.latest().simulateMessage('1'); // a pong on the new socket
        expect(slf.posted).toEqual([]);
    });

    // The heartbeat gives up on a peer that stopped answering: the close it starts
    // is never answered either, and the browser fires `close` only when it gives up
    // on the handshake. The main thread hears the socket is down when the worker
    // closes it, and reconnects; the late `close` reports and schedules nothing.
    it('reports a socket it closes on a dead peer at once, and once', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        const dead = ws.latest();
        dead.simulateOpen();
        ws.deadPeer(true);
        slf.posted.length = 0;
        vi.advanceTimersByTime(60000);
        expect(dead.readyState).toBe(2);
        expect(slf.posted).toEqual([[2, 1006]]);
        ws.deadPeer(false);
        vi.advanceTimersByTime(1200);
        expect(ws.instances).toHaveLength(2);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        dead.giveUp();
        vi.advanceTimersByTime(30000);
        expect(slf.posted.filter((m) => m[0] === 2)).toEqual([]);
        expect(ws.instances).toHaveLength(2);
        // A frame from the abandoned socket is no frame of this connection.
        dead.simulateMessage(JSON.stringify({ o: [] }));
        expect(slf.posted.filter((m) => m[0] === 0)).toEqual([]);
    });

    it('reports a close the main thread asks for at once, on a dead peer too', () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        const dead = ws.latest();
        dead.simulateOpen();
        ws.deadPeer(true);
        slf.posted.length = 0;
        slf.send([2, 1000]);
        expect(slf.posted).toEqual([[2, 1000]]);
        dead.giveUp(1000);
        vi.advanceTimersByTime(60000);
        expect(slf.posted).toEqual([[2, 1000]]);
        expect(ws.instances).toHaveLength(1);
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
                        [5, 'v:0', 'anchor', 'before', 'insert-payload'],
                        [7, 'v:0', 'k2', [[0, 'v:0', 'nested-text']]],
                    ],
                ],
            ],
        };
        ws.latest().simulateMessage(JSON.stringify(msg));
        const resolved = slf.posted.find((m) => m[0] === 0);
        expect(resolved[1][0][3][0][2]).toBe('text-payload');
        expect(resolved[1][0][3][1][4]).toBe('insert-payload');
        expect(resolved[1][0][3][2][3][0][2]).toBe('nested-text');
    });

    it('ITEM_PATCH inner ops resolve through a child-view wrapper', () => {
        // `arizona_diff:make_ops/5`'s child-view clause emits `[ChildViewId, ChildOps]`
        // INSIDE an item's inner ops when a `?stateful` in a stream item re-renders --
        // its head is the view id, a string, not an op code. Unresolved, the `{f,s,d}`
        // payload below reaches the main thread as an object and a text write renders
        // it as "[object Object]".
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        slf.posted.length = 0;
        const msg = {
            o: [
                [
                    7,
                    'sic:0',
                    'k1',
                    [
                        [
                            'badge-1',
                            [[0, 'v:0', { f: 'wrapped_fp', s: ['<em>', '</em>'], d: ['hi'] }]],
                        ],
                    ],
                ],
            ],
        };
        ws.latest().simulateMessage(JSON.stringify(msg));
        const resolved = slf.posted.find((m) => m[0] === 0);
        const childOp = resolved[1][0][3][0][1][0];
        expect(childOp[2]).toBe('<em>hi</em>');
        // and flagged as HTML, so the main thread innerHTMLs it rather than text-noding
        expect(childOp[3]).toBe(true);
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

    it('a reconnect open flags _az_fps_follow=1; a first connect does not', () => {
        // First connect: no flag (SSR delivered the page, nothing to defer)
        // and an empty cache announces nothing -- no extra frame.
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        expect(ws.latest().url).not.toContain('_az_fps_follow');
        ws.latest().simulateOpen();
        expect(ws.latest().sent.find((s) => s.startsWith('["cached_fps"'))).toBeUndefined();
        // Backoff reconnect: the flag rides beside _az_reconnect, promising
        // the cached_fps frame the server holds its resync for.
        ws.latest().close(1006);
        vi.advanceTimersByTime(10000);
        expect(ws.latest().url).toContain('_az_reconnect=1');
        expect(ws.latest().url).toContain('_az_fps_follow=1');
    });

    it('a flagged reconnect announces even an empty cache', async () => {
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1006);
        // The async advance flushes the (empty) hydration microtasks and fires
        // the backoff timer, so the reconnect opens with hydration settled.
        await vi.advanceTimersByTimeAsync(10000);
        ws.latest().simulateOpen();
        // The server defers its resync for this frame, so it MUST go out even
        // with nothing cached -- pre-fix the empty-suppression starved it.
        expect(ws.latest().sent).toContain(JSON.stringify(['cached_fps', []]));
    });

    it('a flagged fresh-worker connect waits for IDB hydration before announcing', async () => {
        // A bfcache respawn: fresh worker (cold in-memory cache), flagged
        // connect. The announcement must carry the REAL (IDB) cache, so it
        // waits for hydration to settle instead of claiming an empty cache.
        idb.restore();
        idb = installIndexedDBStub({ seed: [['fp_seed', { s: ['<p>', '</p>'], u: 1 }]] });
        slf.send([0, 'ws://host/ws?_az_path=%2F', true]);
        ws.latest().simulateOpen();
        // Hydration microtasks have not run yet: nothing announced.
        expect(ws.latest().sent.find((s) => s.startsWith('["cached_fps"'))).toBeUndefined();
        vi.useRealTimers();
        await new Promise((r) => setTimeout(r, 0));
        const sentFps = ws.latest().sent.find((s) => s.startsWith('["cached_fps"'));
        expect(sentFps).toContain('fp_seed');
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

    it('every failed reconnect attempt reports its own close', () => {
        // The main thread's `arizona:disconnected` is once per outage BECAUSE
        // each attempt that never opens still posts a close of its own.
        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        ws.latest().close(1001);
        vi.advanceTimersByTime(10000);
        ws.latest().close(1006); // the handshake failed: no open, no frames
        vi.advanceTimersByTime(10000);
        ws.latest().close(1006);
        expect(slf.posted.filter((m) => m[0] === 2)).toEqual([
            [2, 1001],
            [2, 1006],
            [2, 1006],
        ]);
        expect(slf.posted.filter((m) => m[0] === 1)).toEqual([[1, false]]);
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
        // Re-install stubs with a pre-seeded cursor row so idbLoadPruned iterates.
        idb.restore();
        idb = installIndexedDBStub({
            seed: [['pre_seeded', { s: ['<em>', '</em>'], u: 1 }]],
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
        // Reconnect resets _fpsSent; next open should re-ship cached fps. The
        // async advance also settles hydration, which a flagged reconnect's
        // announcement waits for.
        ws.latest().close(1006);
        await vi.advanceTimersByTimeAsync(10000);
        ws.latest().simulateOpen();
        const sentFps = ws.latest().sent.find((s) => s.startsWith('["cached_fps"'));
        expect(sentFps).toContain('fp_z');
    });

    it('hydration evicts all but the FP_CACHE_MAX most-recently-used entries', async () => {
        // A store one generation past the cap: `u` ranks them, oldest first.
        const seed = [];
        for (let i = 0; i < FP_CACHE_MAX + 3; i++) {
            seed.push([`fp${i}`, { s: ['<p>', '</p>'], u: i }]);
        }
        idb.restore();
        idb = installIndexedDBStub({ seed });
        await loadWorker();

        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        vi.useRealTimers();
        await new Promise((r) => setTimeout(r, 0));

        // The three stalest entries are gone from the store, not just unloaded.
        expect([...idb.deletes].sort()).toEqual(['fp0', 'fp1', 'fp2']);
        // ... and the announcement carries the survivors only, so it cannot grow
        // past the cap however long the origin has been storing fingerprints.
        const sentFps = ws.latest().sent.find((s) => s.startsWith('["cached_fps"'));
        expect(JSON.parse(sentFps)[1]).toHaveLength(FP_CACHE_MAX);
        expect(sentFps).not.toContain('"fp0"');
        expect(sentFps).toContain(`"fp${FP_CACHE_MAX + 2}"`);
    });

    it('hydration keeps a store at the cap intact', async () => {
        const seed = [];
        for (let i = 0; i < FP_CACHE_MAX; i++) {
            seed.push([`fp${i}`, { s: ['<p>', '</p>'], u: i }]);
        }
        idb.restore();
        idb = installIndexedDBStub({ seed });
        await loadWorker();

        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        vi.useRealTimers();
        await new Promise((r) => setTimeout(r, 0));

        expect(idb.deletes).toEqual([]);
        const sentFps = ws.latest().sent.find((s) => s.startsWith('["cached_fps"'));
        expect(JSON.parse(sentFps)[1]).toHaveLength(FP_CACHE_MAX);
    });

    it('a resolve re-stamps a hydrated entry so the next hydration ranks it as used', async () => {
        // Its stored stamp is stale, so eviction would rank it last -- even though
        // the app still renders it. Resolving it writes the fresh stamp back.
        idb.restore();
        idb = installIndexedDBStub({ seed: [['fp_used', { s: ['<p>', '</p>'], u: 1 }]] });
        await loadWorker();

        slf.send([0, 'ws://host/ws?_az_path=%2F']);
        ws.latest().simulateOpen();
        vi.useRealTimers();
        await new Promise((r) => setTimeout(r, 0));
        // Statics omitted: the server trusts the announcement, so this is a hit.
        ws.latest().simulateMessage(
            JSON.stringify({ o: [[0, 'v:0', { f: 'fp_used', d: ['hi'] }]] }),
        );
        await new Promise((r) => setTimeout(r, 0));

        const written = idb.puts.find(([k]) => k === 'fp_used');
        expect(written).toBeDefined();
        expect(written[1].u).toBeGreaterThan(Date.now() - 5000);
    });
});
