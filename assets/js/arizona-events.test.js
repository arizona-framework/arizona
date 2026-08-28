import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// The delegated event surface is open: an `az-<event>` attribute binds
// `addEventListener(<event>)` for ANY type, discovered from the markup itself.
// These cover the parts that a bubble-phase-only delegation could not do.

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
        ops: (ops, azAttrs = null) => inst.onmessage({ data: [0, ops, null, false, azAttrs] }),
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        restore: () => {
            disconnect();
            globalThis.Worker = Orig;
        },
    };
}

/** Dispatch an event exactly as the platform does: `bubbles` decides the phase. */
function fire(el, type, bubbles) {
    el.dispatchEvent(new Event(type, { bubbles }));
}

beforeEach(() => {
    document.body.innerHTML = '';
});

describe('open event delegation', () => {
    let w;
    afterEach(() => w?.restore());

    it('delivers a non-bubbling event declared in the SSR markup', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view>` +
            `<details id="d" az-toggle='[0,"opened"]'><summary>s</summary></details>` +
            `<video id="m" az-play='[0,"played"]'></video>` +
            `</div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

        // Neither event bubbles, so bubble-phase delegation could never see them.
        fire(document.getElementById('d'), 'toggle', false);
        fire(document.getElementById('m'), 'play', false);

        expect(w.sent()).toEqual([
            ['v', 'opened', {}],
            ['v', 'played', {}],
        ]);
    });

    it("delegates a custom element's own event name", async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><sl-select id="s" az-sl-change='[0,"picked"]'></sl-select></div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

        // The attribute suffix is used verbatim as the addEventListener type, so a
        // vocabulary Arizona has never heard of needs no registration anywhere.
        fire(document.getElementById('s'), 'sl-change', true);

        expect(w.sent()).toEqual([['v', 'picked', {}]]);
    });

    it('delegates an event type the worker reports from a later patch', async () => {
        const mod = await fresh();
        // The SSR marker shape a conditional content slot really has.
        document.body.innerHTML = `<div id="v" az-view az="0"><!--az:0-->old<!--/az--></div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

        // OP_TEXT swaps in markup declaring a type nothing had declared at connect.
        // The worker scans the markup it resolved and names it on the frame (proved
        // in arizona-worker-integration.test.js); the main thread must delegate it.
        w.ops([[0, 'v:0', `<dialog id="dlg" az-close='[0,"closed"]'></dialog>`, true]], ['close']);
        fire(document.getElementById('dlg'), 'close', false);

        expect(w.sent()).toEqual([['v', 'closed', {}]]);
    });

    it('delegates an event type an attribute write introduces, with no markup', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view az="0"><dialog id="dlg" az="1"></dialog></div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

        // No markup arrives, so the worker has nothing to scan -- the attribute is
        // written onto an element already in the DOM. Every such write funnels
        // through applySetAttrOp: the op, an item-patch inner op, set_attr/
        // toggle_attr effects, and a ?local attribute slot.
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

    it('fires a non-bubbling event only for the element that declares it', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><div id="outer" az-mouseenter='[0,"entered"]'>` +
            `<span id="inner">x</span></div></div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

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
        mod.mountHooks(document);
        w.open();

        // Capture phase would run before this listener and defeat it; a bubbling
        // event must therefore stay on the bubble path.
        document.getElementById('inner').addEventListener('click', (e) => e.stopPropagation());
        document.getElementById('inner').click();

        expect(w.sent()).toEqual([]);
    });

    it('does not double-run az-submit, which has its own listener', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><form id="f" az-submit='[0,"saved"]'></form></div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

        document
            .getElementById('f')
            .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));

        // `submit` is a real DOM event name AND a dedicated listener, so generic
        // delegation would run the command a second time.
        expect(w.sent()).toEqual([['v', 'saved', {}]]);
    });

    it('survives an event whose target is the Document, not an Element', async () => {
        const mod = await fresh();
        // Declaring az-scroll is what gets `scroll` delegated at all; the viewport
        // then dispatches it at the Document, which has no closest().
        document.body.innerHTML = `<div id="v" az-view><div id="pane" az-scroll='[0,"scrolled"]'></div></div>`;
        w = mockWorker(mod);
        mod.mountHooks(document);
        w.open();

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
});
