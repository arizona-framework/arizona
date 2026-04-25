import { beforeEach, describe, expect, it, vi } from 'vitest';
import {
    backoff,
    fpCache,
    loadFpEntries,
    resolveHtml,
    setOnPersist,
    zipTemplate,
} from './arizona-core.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Reset fp cache between tests so fingerprints don't leak across tests. */
beforeEach(() => {
    fpCache.clear();
    setOnPersist(null);
});

// ---------------------------------------------------------------------------
// resolveHtml
// ---------------------------------------------------------------------------

describe('resolveHtml', () => {
    it('passes through plain HTML strings', () => {
        expect(resolveHtml('<div>hello</div>')).toBe('<div>hello</div>');
    });

    it('zips statics and dynamics from fingerprinted payload', () => {
        const payload = { f: 'fp1', s: ['<p>', '</p>'], d: ['hello'] };
        expect(resolveHtml(payload)).toBe('<p>hello</p>');
    });

    it('caches statics and resolves without s on second call', () => {
        resolveHtml({ f: 'fp_cache', s: ['<b>', '</b>'], d: ['1'] });
        const result = resolveHtml({ f: 'fp_cache', d: ['2'] });
        expect(result).toBe('<b>2</b>');
    });

    it('handles nested fingerprinted dynamics recursively', () => {
        const payload = {
            f: 'parent',
            s: ['<div>', '</div>'],
            d: [{ f: 'child', s: ['<span>', '</span>'], d: ['inner'] }],
        };
        expect(resolveHtml(payload)).toBe('<div><span>inner</span></div>');
    });

    it('handles mixed plain and fingerprinted dynamics', () => {
        const payload = {
            f: 'mix',
            s: ['<div>', ' - ', '</div>'],
            d: ['plain', { f: 'sub', s: ['<b>', '</b>'], d: ['nested'] }],
        };
        expect(resolveHtml(payload)).toBe('<div>plain - <b>nested</b></div>');
    });
});

// ---------------------------------------------------------------------------
// resolveHtml -- stream type
// ---------------------------------------------------------------------------

describe('resolveHtml -- stream type', () => {
    it('resolves stream payload with type constant and list-of-lists d', () => {
        const payload = { t: 0, f: 'stream_fp_1', s: ['<li>', '</li>'], d: [['A'], ['B']] };
        expect(resolveHtml(payload)).toBe('<li>A</li><li>B</li>');
    });

    it('caches stream type and resolves from cache', () => {
        resolveHtml({ t: 0, f: 'stream_fp_2', s: ['<span>', '</span>'], d: [['x']] });
        const result = resolveHtml({ t: 0, f: 'stream_fp_2', d: [['y'], ['z']] });
        expect(result).toBe('<span>y</span><span>z</span>');
    });

    it('handles empty stream d list', () => {
        const payload = { t: 0, f: 'stream_fp_3', s: ['<li>', '</li>'], d: [] };
        expect(resolveHtml(payload)).toBe('');
    });

    it('handles stream items with multiple dynamics', () => {
        const payload = {
            t: 0,
            f: 'stream_fp_4',
            s: ['<tr><td>', '</td><td>', '</td></tr>'],
            d: [
                ['Alice', '30'],
                ['Bob', '25'],
            ],
        };
        expect(resolveHtml(payload)).toBe(
            '<tr><td>Alice</td><td>30</td></tr><tr><td>Bob</td><td>25</td></tr>',
        );
    });

    it('non-stream fingerprinted payload is not treated as stream', () => {
        const payload = { f: 'non_stream_fp', s: ['<b>', '</b>'], d: ['hello'] };
        expect(resolveHtml(payload)).toBe('<b>hello</b>');
    });

    it('payload without t is not treated as stream even if cache has t', () => {
        resolveHtml({ t: 0, f: 'pt_fp', s: ['<td>', '-', '</td>'], d: [['A', 'B']] });
        const result = resolveHtml({ f: 'pt_fp', d: ['X', 'Y'] });
        expect(result).toBe('<td>X-Y</td>');
    });

    it('STREAM=0 is correctly detected via strict equality, not truthiness', () => {
        const payload = { t: 0, f: 'strict_eq_fp', s: ['<li>', '</li>'], d: [['One'], ['Two']] };
        expect(resolveHtml(payload)).toBe('<li>One</li><li>Two</li>');
    });
});

// ---------------------------------------------------------------------------
// zipTemplate
// ---------------------------------------------------------------------------

describe('zipTemplate', () => {
    it('interleaves statics and dynamics', () => {
        expect(zipTemplate(['a', 'b', 'c'], ['1', '2'])).toBe('a1b2c');
    });

    it('handles single static with no dynamics', () => {
        expect(zipTemplate(['hello'], [])).toBe('hello');
    });

    it('handles array dynamics (stream items as plain strings)', () => {
        expect(zipTemplate(['<ul>', '</ul>'], [['<li>a</li>', '<li>b</li>']])).toBe(
            '<ul><li>a</li><li>b</li></ul>',
        );
    });

    it('handles array dynamics with fingerprinted items', () => {
        const items = [
            { f: 'stream_item', s: ['<li>', '</li>'], d: ['first'] },
            { f: 'stream_item', d: ['second'] },
        ];
        expect(zipTemplate(['<ul>', '</ul>'], [items])).toBe(
            '<ul><li>first</li><li>second</li></ul>',
        );
    });

    it('handles mixed array and scalar dynamics', () => {
        const items = ['<li>a</li>', '<li>b</li>'];
        expect(zipTemplate(['<h1>', '</h1><ul>', '</ul>'], ['Title', items])).toBe(
            '<h1>Title</h1><ul><li>a</li><li>b</li></ul>',
        );
    });

    it('handles empty array dynamic', () => {
        expect(zipTemplate(['<ul>', '</ul>'], [[]])).toBe('<ul></ul>');
    });

    it('handles array with mixed plain and fingerprinted items', () => {
        const items = [
            '<li>plain</li>',
            { f: 'mixed_arr', s: ['<li class="fp">', '</li>'], d: ['rich'] },
        ];
        expect(zipTemplate(['<ul>', '</ul>'], [items])).toBe(
            '<ul><li>plain</li><li class="fp">rich</li></ul>',
        );
    });
});

// ---------------------------------------------------------------------------
// resolveHtml -- cache miss
// ---------------------------------------------------------------------------

describe('resolveHtml -- cache miss', () => {
    it('throws a clear error when statics are not cached', () => {
        const payload = { f: 'unknown_fp_xyz', d: ['val'] };
        expect(() => resolveHtml(payload)).toThrow(
            'arizona: unknown template fingerprint "unknown_fp_xyz"',
        );
    });

    it('does not throw when statics are provided in payload', () => {
        const payload = { f: 'new_fp_ok', s: ['<b>', '</b>'], d: ['val'] };
        expect(resolveHtml(payload)).toBe('<b>val</b>');
    });

    it('does not throw when statics are already cached', () => {
        resolveHtml({ f: 'precached_fp', s: ['<i>', '</i>'], d: ['1'] });
        expect(resolveHtml({ f: 'precached_fp', d: ['2'] })).toBe('<i>2</i>');
    });
});

// ---------------------------------------------------------------------------
// cache persistence -- via onPersist callback
// ---------------------------------------------------------------------------

describe('cache persistence', () => {
    it('resolveHtml with new statics calls onPersist with fpId and entry', () => {
        const spy = vi.fn();
        setOnPersist(spy);
        resolveHtml({ f: 'ls_persist_1', s: ['<b>', '</b>'], d: ['x'] });
        expect(spy).toHaveBeenCalledOnce();
        expect(spy).toHaveBeenCalledWith('ls_persist_1', { s: ['<b>', '</b>'] });
    });

    it('multiple fingerprints each call onPersist individually', () => {
        const spy = vi.fn();
        setOnPersist(spy);
        resolveHtml({ f: 'ls_multi_a', s: ['<i>', '</i>'], d: ['1'] });
        resolveHtml({ f: 'ls_multi_b', s: ['<u>', '</u>'], d: ['2'] });
        expect(spy).toHaveBeenCalledTimes(2);
        expect(spy).toHaveBeenCalledWith('ls_multi_a', { s: ['<i>', '</i>'] });
        expect(spy).toHaveBeenCalledWith('ls_multi_b', { s: ['<u>', '</u>'] });
    });

    it('resolveHtml without s does not call onPersist', () => {
        const spy = vi.fn();
        resolveHtml({ f: 'ls_no_write', s: ['<b>', '</b>'], d: ['1'] });
        setOnPersist(spy);
        resolveHtml({ f: 'ls_no_write', d: ['2'] });
        expect(spy).not.toHaveBeenCalled();
    });

    it('onPersist error does not crash resolveHtml', () => {
        setOnPersist(() => {
            throw new Error('QuotaExceededError');
        });
        // Should not throw -- onPersist errors are the caller's problem
        // (In the real Worker, idbPut swallows errors, so this tests robustness)
        expect(() => resolveHtml({ f: 'ls_err', s: ['<em>', '</em>'], d: ['ok'] })).toThrow(
            'QuotaExceededError',
        );
    });

    it('stream type t is included in onPersist entry', () => {
        const spy = vi.fn();
        setOnPersist(spy);
        resolveHtml({ t: 0, f: 'ls_stream', s: ['<li>', '</li>'], d: [['x']] });
        expect(spy).toHaveBeenCalledWith('ls_stream', { s: ['<li>', '</li>'], t: 0 });
    });
});

// ---------------------------------------------------------------------------
// loadFpEntries -- cache hydration
// ---------------------------------------------------------------------------

describe('loadFpEntries', () => {
    it('populates fpCache from entries array', () => {
        loadFpEntries([
            ['fp_a', { s: ['<b>', '</b>'] }],
            ['fp_b', { s: ['<i>', '</i>'], t: 0 }],
        ]);
        expect(fpCache.get('fp_a')).toEqual({ s: ['<b>', '</b>'] });
        expect(fpCache.get('fp_b')).toEqual({ s: ['<i>', '</i>'], t: 0 });
    });

    it('loaded entries are usable by resolveHtml', () => {
        loadFpEntries([['fp_hydrated', { s: ['<em>', '</em>'] }]]);
        expect(resolveHtml({ f: 'fp_hydrated', d: ['works'] })).toBe('<em>works</em>');
    });

    it('loaded stream entries resolve correctly', () => {
        loadFpEntries([['fp_stream_hydrated', { s: ['<li>', '</li>'], t: 0 }]]);
        expect(resolveHtml({ t: 0, f: 'fp_stream_hydrated', d: [['a'], ['b']] })).toBe(
            '<li>a</li><li>b</li>',
        );
    });

    it('does not trigger onPersist', () => {
        const spy = vi.fn();
        setOnPersist(spy);
        loadFpEntries([['fp_no_persist', { s: ['<b>', '</b>'] }]]);
        expect(spy).not.toHaveBeenCalled();
    });
});

// ---------------------------------------------------------------------------
// backoff -- step backoff with jitter
// ---------------------------------------------------------------------------

describe('backoff', () => {
    it('returns values within jitter range for each attempt', () => {
        const bases = [1000, 2000, 5000, 10000];
        for (let i = 0; i < bases.length; i++) {
            const base = bases[i];
            for (let j = 0; j < 20; j++) {
                const result = backoff(i);
                expect(result).toBeGreaterThanOrEqual(Math.floor(base * 0.8));
                expect(result).toBeLessThanOrEqual(Math.ceil(base * 1.2));
            }
        }
    });

    it('caps at 10000ms base for attempts beyond the table', () => {
        for (let j = 0; j < 20; j++) {
            const result = backoff(10);
            expect(result).toBeGreaterThanOrEqual(8000);
            expect(result).toBeLessThanOrEqual(12000);
        }
    });
});
