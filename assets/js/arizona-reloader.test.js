import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { connect } from './arizona-reloader.js';

/** Minimal EventSource mock that captures addEventListener callbacks. */
function installMockEventSource() {
    const instances = [];
    class MockEventSource {
        constructor(url) {
            this.url = url;
            this.listeners = {};
            instances.push(this);
        }
        addEventListener(name, fn) {
            this.listeners[name] = fn;
        }
        dispatch(name, evt = {}) {
            const fn = this.listeners[name];
            if (fn) fn(evt);
        }
    }
    const Orig = globalThis.EventSource;
    globalThis.EventSource = MockEventSource;
    return {
        instances,
        restore() {
            globalThis.EventSource = Orig;
        },
    };
}

describe('arizona-reloader connect', () => {
    let mockES;
    const origReload = window.location.reload;

    beforeEach(() => {
        mockES = installMockEventSource();
        document.head.innerHTML = '';
        document.body.innerHTML = '';
        // jsdom's window.location.reload is a no-op; replace with spy
        Object.defineProperty(window, 'location', {
            configurable: true,
            value: { ...window.location, reload: vi.fn() },
        });
    });

    afterEach(() => {
        mockES.restore();
        Object.defineProperty(window, 'location', {
            configurable: true,
            value: { ...window.location, reload: origReload },
        });
    });

    it('opens an EventSource at the configured URL', () => {
        connect('/dev/reload');
        expect(mockES.instances).toHaveLength(1);
        expect(mockES.instances[0].url).toBe('/dev/reload');
    });

    it('reload event triggers location.reload', () => {
        connect('/dev/reload');
        mockES.instances[0].dispatch('reload');
        expect(window.location.reload).toHaveBeenCalledTimes(1);
    });

    it('reload_css event busts cache on every stylesheet link', () => {
        document.head.innerHTML = `
            <link rel="stylesheet" href="http://localhost/a.css" />
            <link rel="stylesheet" href="http://localhost/b.css?v=1" />
            <link rel="icon" href="http://localhost/favicon.ico" />
        `;
        const nowSpy = vi.spyOn(Date, 'now').mockReturnValue(123456);

        connect('/dev/reload');
        mockES.instances[0].dispatch('reload_css');

        const links = document.querySelectorAll('link[rel="stylesheet"]');
        for (const link of links) {
            expect(link.href).toContain('_t=123456');
        }
        // Non-stylesheet link untouched
        const icon = document.querySelector('link[rel="icon"]');
        expect(icon.href).toBe('http://localhost/favicon.ico');

        nowSpy.mockRestore();
    });

    it('reload_css preserves existing query params and overwrites _t', () => {
        document.head.innerHTML = `<link rel="stylesheet" href="http://localhost/a.css?v=1&_t=old" />`;
        vi.spyOn(Date, 'now').mockReturnValue(999);

        connect('/dev/reload');
        mockES.instances[0].dispatch('reload_css');

        const link = document.querySelector('link[rel="stylesheet"]');
        expect(link.href).toContain('v=1');
        expect(link.href).toContain('_t=999');
        expect(link.href).not.toContain('_t=old');
    });
});
