#!/usr/bin/env node
/**
 * Client-side benchmark for the connect/hydration path, run in a real Chromium
 * via Playwright. Complements `bench_client.mjs` (which times `applyOps` on an
 * already-connected page) by measuring what happens BEFORE that: the real
 * `connect()` -- delegation binding, worker spawn, IndexedDB hydration, the
 * connect frame's az-attrs delegation, `mountHooks` -- and the bfcache
 * reconnect resync (worker respawn, `cached_fps` handshake, full-page
 * OP_REPLACE, form restore).
 *
 * Run with `make bench-client-connect`. Never wired into `make ci`.
 *
 * Everything is the production path except the network: fixtures carry the SSR
 * page and the exact frames a real `arizona_socket` emitted for it
 * (`client_fixture.escript`), and the only substitution is a stubbed
 * `WebSocket` in the worker that replays those frames with zero latency -- so
 * the numbers isolate client-side cost. The reconnect leg is triggered through
 * the client's own bfcache handlers (synthetic `pagehide`/`pageshow`), not by
 * calling internals.
 *
 * Guards, mirroring `bench_client.mjs`:
 *  1. Frames come from a real socket, never hand-written.
 *  2. The run REFUSES to report unless `az-connected` appeared, the resync
 *     visibly replaced the view root, and the console stayed silent.
 *
 * Each run uses a fresh browser context, so the worker's IndexedDB fingerprint
 * cache is cold every time -- the numbers are the deterministic cold-connect
 * cost, not a mix of cold and warm.
 *
 * Usage: bench_client_connect.mjs FIXTURE_DIR [--only LABEL] [--runs N]
 */
import { readFileSync, readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

const [, , fixtureDir, ...rest] = process.argv;
if (!fixtureDir) {
    console.error('usage: bench_client_connect.mjs FIXTURE_DIR [--only LABEL] [--runs N]');
    process.exit(1);
}
const only = [];
let runs = 25;
for (let i = 0; i < rest.length; i++) {
    if (rest[i] === '--only') only.push(rest[++i]);
    else if (rest[i] === '--runs') runs = Number(rest[++i]);
}

const CLIENT = readFileSync(join(ROOT, 'assets/js/arizona.js'), 'utf8');
const CORE = readFileSync(join(ROOT, 'assets/js/arizona-core.js'), 'utf8');
const WORKER = readFileSync(join(ROOT, 'assets/js/arizona-worker.js'), 'utf8');
// The committed production bundle: one minified ESM file with the same named
// exports, referencing its sibling min worker. Timed as its own pass so the
// headline numbers exist in the exact shape production ships -- the source
// pass pays two module fetches the bundle does not.
const MIN_CLIENT = readFileSync(join(ROOT, 'priv/static/assets/js/arizona.min.js'), 'utf8');
const MIN_WORKER = readFileSync(join(ROOT, 'priv/static/assets/js/arizona-worker.min.js'), 'utf8');

/** Connect-path internals worth a per-call breakdown; missing names are skipped. */
const INTERNALS = [
    'bindDocumentEvents', 'handleEvent', 'noteAzAttrs', 'mountHooks', 'restoreFormState',
    'saveFormState', 'applyOps', 'applyEffects', 'parseFragmentIn', 'notifyUpdated',
];

/** Wrap `function` declarations so internal call sites see the timing shim. */
function instrumented(src) {
    const present = INTERNALS.filter((n) => src.includes(`function ${n}(`));
    let block = '\nwindow.__stats = {};\n';
    for (const n of present) {
        block +=
            `{const _o=${n};window.__stats['${n}']={n:0,ms:0};` +
            `${n}=function(...a){const s=performance.now();try{return _o.apply(this,a);}` +
            `finally{const r=window.__stats['${n}'];r.n++;r.ms+=performance.now()-s;}};}\n`;
    }
    return src + block;
}

/**
 * A zero-latency WebSocket replaying the fixture's real frames: the connect
 * frame on open (the reconnect variant when the URL carries `_az_reconnect=1`),
 * and the resync frame in answer to the worker's `cached_fps` announcement --
 * the same handshake a deferred reconnect performs against a real server.
 */
function fakeSocketPrelude(meta) {
    const frames = JSON.stringify({
        connect: meta.connect_frame,
        reconnect: meta.reconnect_frame,
        resync: meta.resync_frame,
    });
    return `
const __AZ_FRAMES = ${frames};
class __AzFakeWS {
    constructor(url) {
        this.url = url;
        this.readyState = 0;
        const reconnect = url.includes('_az_reconnect=1');
        setTimeout(() => {
            this.readyState = 1;
            if (this.onopen) this.onopen();
            if (this.onmessage) {
                this.onmessage({ data: reconnect ? __AZ_FRAMES.reconnect : __AZ_FRAMES.connect });
            }
        }, 0);
    }
    send(data) {
        if (typeof data === 'string' && data.startsWith('["cached_fps"')) {
            setTimeout(() => {
                if (this.onmessage) this.onmessage({ data: __AZ_FRAMES.resync });
            }, 0);
        }
    }
    close(code = 1000) {
        this.readyState = 3;
        setTimeout(() => {
            if (this.onclose) this.onclose({ code });
        }, 0);
    }
}
self.WebSocket = __AzFakeWS;
`;
}

// localhost is a trustworthy origin, so COOP/COEP are honored (5 us timers);
// every request is fulfilled from the route -- the port never binds.
const ORIGIN = 'http://localhost:9743';

/** Serve page + client + worker from memory, COOP/COEP so timers are 5 us. */
async function routeFiles(context, html, clientSrc, workerSrc) {
    await context.route(`${ORIGIN}/**`, (route) => {
        const path = new URL(route.request().url()).pathname;
        const respond = (body, type) =>
            route.fulfill({
                body,
                headers: {
                    'Content-Type': type,
                    'Cross-Origin-Opener-Policy': 'same-origin',
                    'Cross-Origin-Embedder-Policy': 'require-corp',
                },
            });
        if (path === '/index.html') return respond(html, 'text/html');
        if (path === '/az/arizona.js') return respond(clientSrc, 'text/javascript');
        if (path === '/az/arizona-core.js') return respond(CORE, 'text/javascript');
        // The source client asks for arizona-worker.js, the bundle for its min
        // sibling -- serve the pass's worker under both names.
        if (path === '/az/arizona-worker.js' || path === '/az/arizona-worker.min.js') {
            return respond(workerSrc, 'text/javascript');
        }
        if (path === '/az/floor-worker.js') return respond('postMessage(1);', 'text/javascript');
        return route.fulfill({ status: 404, body: 'not found' });
    });
}

/** One full connect + reconnect cycle in a fresh context. */
async function measure(browser, html, clientSrc, workerSrc) {
    const context = await browser.newContext();
    const warnings = [];
    const page = await context.newPage();
    page.on('console', (m) => {
        if (m.type() === 'warning' || m.type() === 'error') warnings.push(m.text());
    });
    await routeFiles(context, html, clientSrc, workerSrc);
    await page.goto(`${ORIGIN}/index.html`);
    await page.waitForFunction(() => /** @type {any} */ (window).__ready === true);
    let r;
    let error = null;
    try {
        r = await page.evaluate(async () => {
            const w = /** @type {any} */ (window);
            const waitFor = (pred, timeout = 5000) =>
                new Promise((res, rej) => {
                    const t0 = performance.now();
                    (function poll() {
                        if (pred()) return res(performance.now());
                        if (performance.now() - t0 > timeout) return rej(new Error('timeout'));
                        setTimeout(poll, 0);
                    })();
                });
            const connectedNow = () =>
                document.documentElement.classList.contains('az-connected');

            const root = document.querySelector('[az-view]');
            const t0 = performance.now();
            w.__az.connect('/live');
            const tSync = performance.now() - t0;
            await waitFor(connectedNow);
            const tConnected = performance.now() - t0;
            // The az-attrs frame is delegated on arrival; its `noteAzAttrs` call
            // is the observable end of "the page can answer its own events".
            await waitFor(() => !w.__stats || w.__stats.noteAzAttrs?.n > 0);
            const tAttrs = performance.now() - t0;
            const connectStats = w.__stats ? JSON.parse(JSON.stringify(w.__stats)) : null;
            if (w.__stats) {
                for (const k in w.__stats) {
                    w.__stats[k].n = 0;
                    w.__stats[k].ms = 0;
                }
            }

            // Reconnect resync through the client's own bfcache handlers. The
            // sentinel proves the OP_REPLACE really rebuilt the view root.
            /** @type {any} */ (root).__azSentinel = true;
            window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true }));
            const t1 = performance.now();
            window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true }));
            await waitFor(() => {
                const el = /** @type {any} */ (document.querySelector('[az-view]'));
                return el && !el.__azSentinel && connectedNow();
            });
            const tResync = performance.now() - t1;
            const resyncStats = w.__stats ? JSON.parse(JSON.stringify(w.__stats)) : null;

            // Floor: spawn-to-first-message of a worker that does nothing --
            // the browser machinery share of "to az-connected". Context for
            // the totals, not a target.
            const t2 = performance.now();
            const tFloor = await new Promise((res, rej) => {
                const fw = new Worker('/az/floor-worker.js', { type: 'module' });
                fw.onmessage = () => {
                    fw.terminate();
                    res(performance.now() - t2);
                };
                fw.onerror = () => rej(new Error('floor worker failed'));
            });
            return {
                tSync, tConnected, tAttrs, tResync, tFloor, connectStats, resyncStats,
                isolated: crossOriginIsolated,
            };
        });
    } catch (err) {
        error = err.message.split('\n')[0];
    }
    await context.close();
    return { r, warnings, error };
}

const fmt = (arr) => {
    const s = [...arr].sort((a, b) => a - b);
    return `${s[0].toFixed(2)} ms min   ${s[Math.floor(s.length / 2)].toFixed(2)} ms p50`;
};

function printShares(title, stats) {
    if (!stats) return;
    const rows = Object.entries(stats)
        .filter(([, v]) => v.n > 0)
        .sort((a, b) => b[1].ms - a[1].ms);
    if (rows.length === 0) return;
    const top = Math.max(...rows.map(([, v]) => v.ms));
    console.log(`  ${title} breakdown (instrumented -- read shares, not ms)`);
    for (const [k, v] of rows) {
        console.log(
            `    ${k.padEnd(22)} ${String(v.n).padStart(5)} calls ${((v.ms / top) * 100).toFixed(1).padStart(8)}%`,
        );
    }
}

const labels = readdirSync(fixtureDir)
    .filter((f) => f.endsWith('.json'))
    .map((f) => f.slice(0, -5))
    .filter((l) => only.length === 0 || only.includes(l));

let browser;
try {
    browser = await chromium.launch();
} catch (err) {
    console.error(`cannot launch chromium: ${err.message.split('\n')[0]}`);
    console.error('run `make setup-e2e` to install the browser Playwright needs.');
    process.exit(1);
}
let failed = false;
let ranAny = false;

for (const label of labels) {
    const meta = JSON.parse(readFileSync(join(fixtureDir, `${label}.json`), 'utf8'));
    if (meta.kind !== 'connect') continue;
    ranAny = true;
    const fixtureHtml = readFileSync(join(fixtureDir, `${label}.html`), 'utf8');
    const html = fixtureHtml.replace(
        '</body>',
        '<script type="module">' +
            "import * as az from '/az/arizona.js';" +
            'window.__az = az; window.__ready = true;' +
            '</script></body>',
    );
    const workerSrc = fakeSocketPrelude(meta) + WORKER;

    console.log(
        `\n${label}  (connect ${meta.connect_frame.length} B, resync ${meta.resync_frame.length} B)`,
    );

    // Plain passes for the headline timings (source shape, then the committed
    // production bundle); instrumented pass for shares only.
    const timedPass = async (clientSrc, passWorkerSrc) => {
        const timings = { tSync: [], tConnected: [], tAttrs: [], tResync: [], tFloor: [] };
        let isolated = null;
        for (let i = 0; i < runs; i++) {
            const { r, warnings, error } = await measure(browser, html, clientSrc, passWorkerSrc);
            if (error || warnings.length > 0) return { broken: { error, warnings } };
            isolated = r.isolated;
            for (const k of Object.keys(timings)) timings[k].push(r[k]);
        }
        return { timings, isolated };
    };
    const refuse = (broken) => {
        failed = true;
        console.log(`  REFUSING TO REPORT: ${broken.error || 'console output'}`);
        for (const wtext of (broken.warnings || []).slice(0, 3)) console.log(`    ${wtext}`);
        console.log('  A connect that never reaches az-connected, a resync that never');
        console.log('  replaces the root, or console noise means the numbers would');
        console.log('  measure a broken boot, not the boot.');
    };

    const src = await timedPass(CLIENT, workerSrc);
    if (src.broken) {
        refuse(src.broken);
        continue;
    }
    if (!src.isolated) {
        console.log('  note: crossOriginIsolated=false -- timings quantized to 100 us');
    }
    console.log(`  connect() sync        ${fmt(src.timings.tSync)}`);
    console.log(`  to az-connected       ${fmt(src.timings.tConnected)}`);
    console.log(`  to az-attrs applied   ${fmt(src.timings.tAttrs)}`);
    console.log(`  reconnect resync      ${fmt(src.timings.tResync)}`);
    console.log(`  worker spawn floor    ${fmt(src.timings.tFloor)}   (bare worker, spawn-to-message)`);

    const bundled = await timedPass(MIN_CLIENT, fakeSocketPrelude(meta) + MIN_WORKER);
    if (bundled.broken) {
        refuse(bundled.broken);
        continue;
    }
    console.log('  production bundle (arizona.min.js):');
    console.log(`    connect() sync      ${fmt(bundled.timings.tSync)}`);
    console.log(`    to az-connected     ${fmt(bundled.timings.tConnected)}`);
    console.log(`    reconnect resync    ${fmt(bundled.timings.tResync)}`);

    // Shares from one boot are a single call per function -- aggregate several
    // instrumented runs so one scheduling hiccup can't own a share.
    const instRuns = Math.max(5, Math.floor(runs / 3));
    const agg = { connect: {}, resync: {} };
    const accumulate = (into, stats) => {
        if (!stats) return;
        for (const [k, v] of Object.entries(stats)) {
            const slot = (into[k] ??= { n: 0, ms: 0 });
            slot.n += v.n;
            slot.ms += v.ms;
        }
    };
    let instOk = 0;
    for (let i = 0; i < instRuns; i++) {
        const inst = await measure(browser, html, instrumented(CLIENT), workerSrc);
        if (inst.error || inst.warnings.length > 0 || !inst.r) continue;
        instOk++;
        accumulate(agg.connect, inst.r.connectStats);
        accumulate(agg.resync, inst.r.resyncStats);
    }
    if (instOk > 0) {
        printShares(`connect (${instOk} boots)`, agg.connect);
        printShares(`resync (${instOk} boots)`, agg.resync);
    }
}

if (!ranAny) {
    console.error(`no connect fixtures in ${fixtureDir}${only.length ? ` matching ${only}` : ''}`);
    failed = true;
}
await browser.close();
process.exit(failed ? 1 : 0);
