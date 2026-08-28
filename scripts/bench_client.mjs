#!/usr/bin/env node
/**
 * Client-side benchmark for `applyOps`, run in a real Chromium via Playwright.
 *
 * Run with `make bench-client`. Never wired into `make ci` -- numbers are noisy
 * and need human comparison, like `make bench`.
 *
 * Two guards exist because both of these silently produced plausible numbers
 * for work that was not happening:
 *
 *  1. Fixtures come from a REAL diff (`scripts/client_fixture.escript`), never
 *     hand-written ops. The op shape is not guessable: a bulk change collapses
 *     to one container `OP_TEXT`, a partial change emits per-item
 *     `OP_ITEM_PATCH` that all share the container's az. A benchmark inventing
 *     one op per element measures a workload the engine never emits.
 *  2. The run REFUSES to report unless the DOM visibly changed and the console
 *     stayed silent. An op whose target does not resolve is skipped with a
 *     `console.warn`, so a mis-scoped batch otherwise benchmarks warning spam.
 *
 * Usage: bench_client.mjs FIXTURE_DIR [--only LABEL] [--runs N]
 */
import { readFileSync, readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';

// Relative to this script, not the cwd, so it runs from anywhere.
const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

const [, , fixtureDir, ...rest] = process.argv;
if (!fixtureDir) {
    console.error('usage: bench_client.mjs FIXTURE_DIR [--only LABEL] [--runs N]');
    process.exit(1);
}
const only = [];
let runs = 300;
for (let i = 0; i < rest.length; i++) {
    if (rest[i] === '--only') only.push(rest[++i]);
    else if (rest[i] === '--runs') runs = Number(rest[++i]);
}

const CLIENT = join(ROOT, 'assets/js/arizona.js');
const CORE = join(ROOT, 'assets/js/arizona-core.js');

// Module-internal functions worth a per-call breakdown. A name that is not a
// `function` declaration in the client is skipped rather than failing the run.
const INTERNALS = [
    'applyOps', 'applyTextOp', 'applyItemPatch', 'applyItemOps', 'applySetAttrOp',
    'resolveOpTarget', 'resolveInnerEl', 'findMarker', 'findMarkerDeep',
    'buildKeyMap', 'itemByKey', 'updateMarkerContent', 'updateLoneTextNode',
    'forEachNodeInSlot', 'parseFragmentIn', 'mountHooksOnly', 'notifyUpdated',
];

function instrumented(src) {
    const present = INTERNALS.filter((n) => src.includes(`function ${n}(`));
    let block = '\nwindow.__stats = {};\n';
    for (const n of present) {
        // A `function` declaration creates a mutable module binding, so internal
        // call sites see the wrapper too.
        block +=
            `{const _o=${n};window.__stats['${n}']={n:0,ms:0};` +
            `${n}=function(...a){const s=performance.now();try{return _o.apply(this,a);}` +
            `finally{const r=window.__stats['${n}'];r.n++;r.ms+=performance.now()-s;}};}\n`;
    }
    return src + block + 'window.__az={applyOps};\n';
}

// Mirrors `resolveOps` in assets/js/arizona-worker.js: the worker flattens every
// template payload to an HTML string before the main thread ever sees it, so a
// bench feeding raw diff output would measure a shape the client never receives.
const RESOLVE = `
window.__resolveOps = (ops) => {
    const T = 0, INS = 5, IP = 7, REP = 8, LP = 10;
    for (const op of ops) {
        if (typeof op[0] !== 'number') { window.__resolveOps(op[1]); continue; }
        switch (op[0]) {
            case T: { const h = typeof op[2] !== 'string';
                op[2] = window.__core.resolveHtml(op[2]); op[3] = h; break; }
            case REP: op[2] = window.__core.resolveHtml(op[2]); break;
            case INS: op[4] = window.__core.resolveHtml(op[4]); break;
            case IP: window.__resolveOps(op[3]); break;
            case LP: for (const s of op[2]) {
                if (s[0] === IP) window.__resolveOps(s[2]);
                else if (s[0] === INS) s[2] = window.__core.resolveHtml(s[2]);
            } break;
        }
    }
    return ops;
};`;

const labels = readdirSync(fixtureDir)
    .filter((f) => f.endsWith('.json'))
    .map((f) => f.slice(0, -5))
    .filter((l) => only.length === 0 || only.includes(l));
if (labels.length === 0) {
    console.error(`no fixtures in ${fixtureDir}${only.length ? ` matching ${only}` : ''}`);
    process.exit(1);
}

const client = readFileSync(CLIENT, 'utf8');
const core = readFileSync(CORE, 'utf8');
let browser;
try {
    browser = await chromium.launch();
} catch (err) {
    console.error(`cannot launch chromium: ${err.message.split('\n')[0]}`);
    console.error('run `make setup-e2e` to install the browser Playwright needs.');
    process.exit(1);
}
let failed = false;

for (const label of labels) {
    const meta = JSON.parse(readFileSync(join(fixtureDir, `${label}.json`), 'utf8'));
    const html = readFileSync(join(fixtureDir, `${label}.html`), 'utf8');

    let plainTotal = null;
    for (const mode of ['plain', 'instrumented']) {
        const page = await browser.newPage();
        const warnings = [];
        page.on('console', (m) => {
            if (m.type() === 'warning' || m.type() === 'error') warnings.push(m.text());
        });
        await page.setContent(html);
        await page.addScriptTag({ type: 'module', content: `${core}\nwindow.__core={resolveHtml};` });
        await page.addScriptTag({
            type: 'module',
            content: mode === 'plain' ? `${client}\nwindow.__az={applyOps};` : instrumented(client),
        });
        await page.addScriptTag({ content: RESOLVE });

        const r = await page.evaluate(
            ({ ops: rawOps, runs }) => {
                const ops = window.__resolveOps(JSON.parse(JSON.stringify(rawOps)));
                // Guard: the first application must visibly change the DOM.
                const before = document.body.innerHTML;
                window.__az.applyOps(ops);
                const applied = document.body.innerHTML !== before;

                for (let i = 0; i < 30; i++) window.__az.applyOps(ops);
                if (window.__stats) for (const k in window.__stats) {
                    window.__stats[k].n = 0;
                    window.__stats[k].ms = 0;
                }
                let t = performance.now();
                for (let i = 0; i < runs; i++) window.__az.applyOps(ops);
                const total = (performance.now() - t) / runs;

                // Floor: the least DOM work that produces the same result, for the
                // two shapes a real diff emits. Context for the total, not a target.
                let floor = null;
                const IP = 7, T = 0;
                if (ops.every((o) => o[0] === IP)) {
                    const nodes = [];
                    for (const op of ops) {
                        const li = document.querySelector(`[az-key="${op[2]}"]`);
                        if (!li) continue;
                        for (const c of li.childNodes) if (c.nodeType === 3) { nodes.push(c); break; }
                    }
                    for (let i = 0; i < 30; i++) for (const n of nodes) n.data = `x${i}`;
                    t = performance.now();
                    for (let i = 0; i < runs; i++) for (const n of nodes) n.data = `x${i}`;
                    floor = (performance.now() - t) / runs;
                } else if (ops.length === 1 && ops[0][0] === T && ops[0][3]) {
                    const host = document.querySelector('ul') || document.body;
                    const frag = ops[0][2];
                    for (let i = 0; i < 10; i++) host.innerHTML = frag;
                    t = performance.now();
                    for (let i = 0; i < runs; i++) host.innerHTML = frag;
                    floor = (performance.now() - t) / runs;
                }
                return { total, floor, applied, ops: ops.length, stats: window.__stats || null };
            },
            { ops: meta.ops, runs },
        );
        await page.close();

        if (mode === 'plain') {
            plainTotal = r.total;
            const ok = r.applied && warnings.length === 0;
            console.log(
                `\n${label}  (${meta.items} items, ${meta.changed} changed -> ${r.ops} ops)`,
            );
            if (!ok) {
                failed = true;
                console.log(
                    `  REFUSING TO REPORT: applied=${r.applied} warnings=${warnings.length}`,
                );
                for (const w of warnings.slice(0, 3)) console.log(`    ${w}`);
                console.log('  An op whose target does not resolve is skipped with a warning, so');
                console.log('  timing it would measure warning spam. (A batch that legitimately');
                console.log('  changes nothing reads the same way, and is equally not worth timing.)');
                break;
            }
            const us = (r.total / r.ops) * 1000;
            console.log(`  applied: yes    warnings: 0`);
            console.log(`  total    ${r.total.toFixed(3)} ms/batch    ${us.toFixed(2)} us/op`);
            if (r.floor !== null) {
                console.log(
                    `  floor    ${r.floor.toFixed(3)} ms/batch    ` +
                        `(raw DOM, ${(r.total / r.floor).toFixed(1)}x)`,
                );
            }
        } else if (r.stats) {
            // Wrapping every internal costs two `performance.now()` calls per call,
            // which inflates this run several times over. Absolute ms here would be
            // read as real next to the plain total above, so report SHARES: the
            // proportions survive the overhead, the milliseconds do not.
            const rows = Object.entries(r.stats)
                .map(([k, v]) => [k, v.n / runs, v.ms / runs])
                .filter(([, n]) => n > 0)
                .sort((a, b) => b[2] - a[2]);
            const top = rows.length ? Math.max(...rows.map(([, , ms]) => ms)) : 0;
            const factor = plainTotal ? r.total / plainTotal : null;
            console.log(
                `\n  breakdown (instrumented${factor ? `, ~${factor.toFixed(1)}x slower than above` : ''}` +
                    ` -- read shares, not ms)`,
            );
            console.log(`  function              calls/batch    share`);
            for (const [k, n, ms] of rows) {
                const share = top ? (ms / top) * 100 : 0;
                console.log(`  ${k.padEnd(22)} ${n.toFixed(0).padStart(8)} ${share.toFixed(1).padStart(8)}%`);
            }
        }
    }
}

await browser.close();
process.exit(failed ? 1 : 0);
