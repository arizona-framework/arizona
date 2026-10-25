/**
 * @module arizona
 *
 * Arizona -- Client Runtime
 *
 * Thin DOM patcher for server-rendered views. A Web Worker owns the WebSocket
 * connection and resolves template payloads; this module receives pre-computed
 * DOM-ready ops, applies them, collects user events, and sends them to the
 * Worker for transmission.
 *
 * Wire protocol (Worker -> Main):
 *   [0, ops|null, effects|null, firstAfterReconnect, azAttrs|null] -- resolved message
 *   [1, isReconnect]                                  -- WS opened
 *   [2, closeCode]                                    -- WS closed
 *
 * Wire protocol (Main -> Worker):
 *   [0, wsUrl, isReconnect] -- connect
 *   [1, jsonString]         -- send data
 *   [2, code]               -- close WS
 */

export { applyEffects, executeJS } from './arizona-commands.js';
export {
    connect,
    pushEvent,
    pushEventTo,
    restoreFormState,
    saveFormState,
} from './arizona-connection.js';
export { exitPip, requestPip } from './arizona-docs.js';
export { mountHooksPublic as mountHooks } from './arizona-events.js';
export { hooks } from './arizona-hooks.js';
export { get, set, setAll } from './arizona-locals.js';
export { applyOps, OP, resolveEl } from './arizona-ops.js';
