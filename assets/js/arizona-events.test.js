import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// The delegated event surface is open: an `az-<event>` attribute binds
// `addEventListener(<event>)` for ANY type. The types come from the server as a
// set of `az-*` attribute names the parse transform collected at compile time,
// delivered on the connect frame. These cover what a fixed seven bubble-phase
// listeners could not do, and what the delivery has to get right.

/**
 * A module instance with no delegation history. `_eventTypes` and the
 * prevent-default flag are module state and MONOTONIC, so a shared import would
 * make each test's outcome depend on which types earlier tests declared.
 */
async function fresh() {
    vi.resetModules();
    return await import('./arizona.js');
}

/** Drive a module instance through a mocked Worker, exposing what it sent. */
function mockWorker(mod) {
    const posted = [];
    let onmsg = null;
    const inst = {
        postMessage: (d) => posted.push(d),
        set onmessage(fn) {
            onmsg = fn;
        },
        get onmessage() {
            return onmsg;
        },
        terminate: vi.fn(),
    };
    const Orig = globalThis.Worker;
    globalThis.Worker = function () {
        return inst;
    };
    const disconnect = mod.connect('/ws');
    return {
        open: () => inst.onmessage({ data: [1, false] }),
        /** The connect frame: the app's whole `az-*` vocabulary, ops optional. */
        names: (azAttrs, ops = null) => inst.onmessage({ data: [0, ops, null, false, azAttrs] }),
        ops: (ops) => inst.onmessage({ data: [0, ops, null, false, null] }),
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        restore: () => {
            disconnect();
            globalThis.Worker = Orig;
        },
    };
}

/**
 * Dispatch an event exactly as the platform does: `bubbles` decides the phase.
 * Returns the event, so a test can read `defaultPrevented` back off it.
 */
function fire(el, type, bubbles, cancelable = false) {
    const ev = new Event(type, { bubbles, cancelable });
    el.dispatchEvent(ev);
    return ev;
}

/** Dispatch a drop carrying a dataTransfer, which the `Event` constructor has no slot for. */
function fireDrop(el, key) {
    const ev = new Event('drop', { bubbles: true, cancelable: true });
    Object.defineProperty(ev, 'dataTransfer', { value: { getData: () => key } });
    el.dispatchEvent(ev);
    return ev;
}

beforeEach(() => {
    document.body.innerHTML = '';
});

describe('open event delegation', () => {
    let w;
    /** @type {{mockRestore: () => void} | null} */
    let spy = null;
    afterEach(() => {
        // In the test body a failed assertion skips it, leaving addEventListener
        // mocked and blowing the stack in every later test.
        spy?.mockRestore();
        spy = null;
        w?.restore();
        w = null;
    });

    /** Record the options every document listener is registered with. */
    function recordListeners() {
        const calls = [];
        const orig = document.addEventListener.bind(document);
        spy = vi.spyOn(document, 'addEventListener').mockImplementation((t, fn, opts) => {
            calls.push([t, opts]);
            return orig(t, fn, opts);
        });
        return calls;
    }

    it('delivers a non-bubbling event the server declared', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view>` +
            `<details id="d" az-toggle='[0,"opened"]'><summary>s</summary></details>` +
            `<video id="m" az-play='[0,"played"]'></video>` +
            `</div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-toggle', 'az-play']);

        // Neither event bubbles, so bubble-phase delegation could never see them.
        fire(document.getElementById('d'), 'toggle', false);
        fire(document.getElementById('m'), 'play', false);

        expect(w.sent()).toEqual([
            ['v', 'opened', {}],
            ['v', 'played', {}],
        ]);
    });

    it('delegates the common types before the connect frame arrives', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><button id="b" az-click='[0,"clicked"]'>go</button></div>`;
        w = mockWorker(mod);
        w.open();

        // The socket is open, so `_connected` is true and the user can click -- but
        // the name set is a server frame and arrives a round trip later. Without a
        // bootstrap set the document has no listeners at all in that window and the
        // click is lost with no symptom.
        document.getElementById('b').click();

        expect(w.sent()).toEqual([['v', 'clicked', {}]]);
    });

    it("delegates a custom element's own event name", async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><sl-select id="s" az-sl-change='[0,"picked"]'></sl-select></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-sl-change']);

        // The attribute suffix is used verbatim as the addEventListener type, so a
        // vocabulary Arizona has never heard of needs no registration anywhere.
        fire(document.getElementById('s'), 'sl-change', true);

        expect(w.sent()).toEqual([['v', 'picked', {}]]);
    });

    it("delegates the frame's names before that frame's ops can dispatch", async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view az="0"><!--az:0--><!--/az--></div>`;
        w = mockWorker(mod);
        w.open();
        // The names ride the same frame as ops inserting elements that declare them.
        // A hook's `mounted()` runs INSIDE applyOps, on the element the op just
        // inserted, and may dispatch on it straight away (a media element told to
        // play, a dialog opened on arrival). So "before the ops" is not cosmetic
        // ordering -- the listener has to exist before the op runs, not merely
        // before the next frame.
        mod.hooks.opener = {
            mounted() {
                this.el.dispatchEvent(new Event('toggle', { bubbles: false }));
            },
        };

        w.names(
            ['az-toggle'],
            [
                [
                    0,
                    'v:0',
                    `<details id="d" az-hook="opener" az-toggle='[0,"opened"]'></details>`,
                    true,
                ],
            ],
        );

        expect(w.sent()).toEqual([['v', 'opened', {}]]);
        delete mod.hooks.opener;
    });

    it('lowercases a name a runtime attribute write introduces', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view az="0"><dialog id="dlg" az="1"></dialog></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);

        // `setAttribute` in an HTML document lowercases the name, but the string
        // handed to the triage is whatever the caller wrote. addEventListener types
        // are case-sensitive, so an unlowered `az-Close` binds a dead `Close` while
        // the DOM holds `az-close` -- the attribute lands and its event never fires.
        w.ops([[1, 'v:1', 'az-Close', '[0,"closed"]']]);
        fire(document.getElementById('dlg'), 'close', false);

        expect(w.sent()).toEqual([['v', 'closed', {}]]);
    });

    it('delegates an event type an attribute write introduces, with no markup', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view az="0"><dialog id="dlg" az="1"></dialog></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);

        // A runtime attribute write names a type no template contains, so the
        // compile-time set cannot hold it. Every such write funnels through
        // applySetAttrOp: the op, an item-patch inner op, set_attr/toggle_attr
        // effects, and a ?local attribute slot.
        w.ops([[1, 'v:1', 'az-close', '[0,"closed"]']]);
        fire(document.getElementById('dlg'), 'close', false);
        expect(w.sent()).toEqual([['v', 'closed', {}]]);

        // ...and via an effect, which never reaches the worker at all.
        mod.applyEffects([[7, '#dlg', 'az-cancel', '[0,"cancelled"]']]);
        fire(document.getElementById('dlg'), 'cancel', false);
        expect(w.sent()).toEqual([
            ['v', 'closed', {}],
            ['v', 'cancelled', {}],
        ]);
    });

    it('delegates host-inserted markup through the exported mountHooks', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);

        // Markup Arizona did not render is the one source of `az-*` names the
        // compile-time set cannot cover, so the exported mountHooks scans for them.
        document.getElementById('v').innerHTML =
            `<details id="d" az-toggle='[0,"opened"]'><summary>s</summary></details>`;
        mod.mountHooks(document.getElementById('v'));

        fire(document.getElementById('d'), 'toggle', false);
        expect(w.sent()).toEqual([['v', 'opened', {}]]);
    });

    it('fires a non-bubbling event only for the element that declares it', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><div id="outer" az-mouseenter='[0,"entered"]'>` +
            `<span id="inner">x</span></div></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-mouseenter']);

        // The platform dispatches mouseenter separately to each nesting level. A
        // closest() resolver would run the ancestor's command for the inner one
        // too, so entering a child would report entering the parent twice.
        fire(document.getElementById('inner'), 'mouseenter', false);
        expect(w.sent()).toEqual([]);

        fire(document.getElementById('outer'), 'mouseenter', false);
        expect(w.sent()).toEqual([['v', 'entered', {}]]);
    });

    it('lets an inner stopPropagation still suppress a bubbling ancestor command', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><div id="outer" az-click='[0,"clicked"]'>` +
            `<button id="inner">go</button></div></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);

        // Capture phase would run before this listener and defeat it; a bubbling
        // event must therefore stay on the bubble path.
        document.getElementById('inner').addEventListener('click', (e) => e.stopPropagation());
        document.getElementById('inner').click();

        expect(w.sent()).toEqual([]);
    });

    it('ignores a non-bubbling dispatch of a type that bubbles natively', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><input id="i" az-change='[0,"changed"]' /></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-change']);

        // `new Event('change')` defaults to bubbles:false, the idiom a masked-input
        // or autofill shim uses. These types were bubble-only before the surface
        // opened, and delegating them in capture too would round-trip on views that
        // opted into nothing.
        fire(document.getElementById('i'), 'change', false);
        expect(w.sent()).toEqual([]);
        fire(document.getElementById('i'), 'change', true);
        expect(w.sent()).toEqual([['v', 'changed', { value: '' }]]);
    });

    it('cancels a non-bubbling event when the element declares prevent-default', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view>` +
            `<dialog id="dlg" az-cancel='[0,"cancelled"]' az-prevent-default></dialog></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-cancel', 'az-prevent-default']);

        // `cancel` (Esc on a <dialog>) and `beforetoggle` (a popover) are cancelable
        // and do NOT bubble, so the capture listener is the only one that ever sees
        // them: keeping the dialog open is impossible unless preventDefault runs on
        // that path too. The bubble path can't stand in -- the event never gets there.
        const ev = fire(document.getElementById('dlg'), 'cancel', false, true);

        expect(ev.defaultPrevented).toBe(true);
        expect(w.sent()).toEqual([['v', 'cancelled', {}]]);
    });

    it('registers a forced-passive type passive when nothing declares prevent-default', async () => {
        const mod = await fresh();
        const calls = recordListeners();
        document.body.innerHTML = `<div id="v" az-view><div id="p" az-wheel='[0,"w"]'></div></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-wheel']);

        // Chrome forces these four passive on a Document, so opting out is the only
        // way preventDefault works there -- but opting out unconditionally costs the
        // whole page its scroll fast path, and observing wheel should not do that.
        // One registration, not two: wheel bubbles natively, so no capture listener.
        expect(calls.filter(([t]) => t === 'wheel').map(([, o]) => o.passive)).toEqual([true]);
        // Types the platform does not force are left alone; non-passive is already
        // their default there, so passing the flag at all would be a no-op.
        expect(calls.filter(([t]) => t === 'click').every(([, o]) => o.passive === undefined)).toBe(
            true,
        );

        fire(document.getElementById('p'), 'wheel', true);
        expect(w.sent()).toEqual([['v', 'w', {}]]);
    });

    it('registers a forced-passive type non-passive when the set declares prevent-default', async () => {
        const mod = await fresh();
        const calls = recordListeners();
        document.body.innerHTML = `<div id="v" az-view><div id="p" az-wheel='[0,"w"]' az-prevent-default></div></div>`;
        w = mockWorker(mod);
        w.open();
        // One frame carries the whole vocabulary, and `az-prevent-default` is read
        // out of it before any type is bound -- `passive` is fixed at registration,
        // so a set processed in order would bind wheel passive and never recover.
        w.names(['az-wheel', 'az-prevent-default']);

        // `false`, not absent: Chrome forces passive when the flag is UNSPECIFIED,
        // so an omitted flag here silently re-forces it and preventDefault is a
        // no-op on exactly the four types that needed it.
        expect(calls.filter(([t]) => t === 'wheel').map(([, o]) => o.passive)).toEqual([false]);
        // Exactly one live listener, so the command runs once.
        fire(document.getElementById('p'), 'wheel', true);
        expect(w.sent()).toEqual([['v', 'w', {}]]);
    });

    it('binds a type whose name is not a valid CSS identifier', async () => {
        const mod = await fresh();
        // The HTML parser keeps `.` and `:` in an attribute name, so the type reaches
        // the selector unescaped and would throw per dispatch without CSS.escape.
        // A dot rather than a colon only because jsdom's selector engine will not
        // match an escaped colon; Chromium matches both.
        document.body.innerHTML = `<div id="v" az-view><b id="d" az-my.evt='[0,"loaded"]'>x</b></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-my.evt']);

        expect(() => fire(document.getElementById('d'), 'my.evt', true)).not.toThrow();
        expect(w.sent()).toEqual([['v', 'loaded', {}]]);
    });

    it('does not double-run az-submit, which has its own listener', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><form id="f" az-submit='[0,"saved"]'></form></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-submit']);

        document
            .getElementById('f')
            .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));

        // `submit` is a real DOM event name AND a dedicated listener, so generic
        // delegation would run the command a second time.
        expect(w.sent()).toEqual([['v', 'saved', {}]]);
    });

    it('does not double-run az-drop, which has its own listener', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><ul id="list" az-drop='[0,"reorder"]'>` +
            `<li id="item" az-key="a">A</li></ul></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-drop', 'az-key']);

        // `drop` is submit's twin: a real DOM event name AND a dedicated listener
        // (it carries the drag bookkeeping), so generic delegation would run the
        // command a second time with the same payload.
        fireDrop(document.getElementById('item'), 'a');
        expect(w.sent()).toEqual([['v', 'reorder', { data_transfer: 'a', drop_index: 0 }]]);
    });

    it('survives an event whose target is the Document, not an Element', async () => {
        const mod = await fresh();
        // Declaring az-scroll is what gets `scroll` delegated at all; the viewport
        // then dispatches it at the Document, which has no closest().
        document.body.innerHTML = `<div id="v" az-view><div id="pane" az-scroll='[0,"scrolled"]'></div></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-scroll']);

        const errors = [];
        const onErr = (e) => errors.push(e.error || e.message);
        window.addEventListener('error', onErr);
        fire(document, 'scroll', false);
        // dispatch_event fires on the Document too, under a delegated event name.
        mod.applyEffects([[9, 'click', {}]]);
        window.removeEventListener('error', onErr);

        expect(errors).toEqual([]);
        // The document scrolled, not the pane, so the pane's command must not run.
        expect(w.sent()).toEqual([]);
    });

    it('contains a declared attribute that does not hold a command list', async () => {
        const mod = await fresh();
        // The residual case: the transform records a name whose value it cannot
        // fold (`{az_keydown, ?get(handler)}`), so what it holds is unknown until it
        // arrives. Throwing here is once per dispatch, out of a document listener,
        // for the life of the page -- and keydown fires per keystroke.
        document.body.innerHTML =
            `<div id="v" az-view><input id="i" az-keydown="not a command" />` +
            `<input id="j" az-keydown='[0,"typed"]' /></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-keydown']);
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});

        const i = document.getElementById('i');
        expect(() => fire(i, 'keydown', true)).not.toThrow();
        fire(i, 'keydown', true);
        expect(w.sent()).toEqual([]);
        // Once per attribute name, not per dispatch.
        expect(warn.mock.calls.filter(([m]) => m.includes('az-keydown')).length).toBe(1);

        // ...and a real command of the SAME type is unaffected.
        fire(document.getElementById('j'), 'keydown', true);
        expect(w.sent()).toEqual([['v', 'typed', { value: '' }]]);
        warn.mockRestore();
    });

    it('contains a value that parses but crashes the interpreter', async () => {
        const mod = await fresh();
        // `[1,2,3]` is a structurally valid command list: opcode 1 (toggle) with
        // selector 2 -- querySelectorAll('2') throws. Execution must be inside the
        // containment, or every dispatch throws out of a document listener.
        document.body.innerHTML = `<div id="v" az-view><img id="i" az-error="[1,2,3]" /></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-error']);
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});

        const i = document.getElementById('i');
        expect(() => fire(i, 'error', false)).not.toThrow();
        fire(i, 'error', false);
        // Once per failure message, not per dispatch.
        expect(warn.mock.calls.filter(([m]) => m.includes('az-error')).length).toBe(1);
        warn.mockRestore();
    });

    it('prevents the default for an empty az-event value', async () => {
        const mod = await fresh();
        // `az-<event>=""` declares no command but still asks for suppression:
        // the prevent-default check must run before the empty-value return.
        document.body.innerHTML = `<div id="v" az-view><a id="l" href="#" az-click="" az-prevent-default>x</a></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-prevent-default']);

        const ev = fire(document.getElementById('l'), 'click', true, true);
        expect(ev.defaultPrevented).toBe(true);
        expect(w.sent()).toEqual([]);
    });

    it('rebinds a forced-passive type when prevent-default arrives late', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><div id="s" az-wheel='[0,"w"]'></div></div>`;
        w = mockWorker(mod);
        w.open();
        const calls = recordListeners();
        // Frame 1 declares only the wheel: bound passive (the scroll fast path).
        w.names(['az-wheel']);
        expect(calls.filter(([t]) => t === 'wheel').map(([, o]) => o.passive)).toEqual([true]);
        // Frame 2 (a delta, or a runtime attribute write) declares the directive:
        // passive is fixed at registration, so the wheel listener must be
        // re-registered non-passive -- or the page's preventDefault stays a no-op
        // forever.
        w.names(['az-prevent-default']);
        expect(calls.filter(([t]) => t === 'wheel').map(([, o]) => o.passive)).toEqual([
            true,
            false,
        ]);
        // The old registration is gone: one dispatch runs the command once.
        fire(document.getElementById('s'), 'wheel', true, true);
        expect(w.sent()).toEqual([['v', 'w', {}]]);
    });
});
