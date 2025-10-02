/**
 * @typedef {Object} ArizonaOptions
 * @property {import('./logger/arizona-logger.js').default} [logger] - Logger implementation
 */
/**
 * @typedef {Object} ConnectOptions
 * @property {string} [wsPath] - WebSocket path (default: '/live')
 */
/**
 * @typedef {Object} EventParams
 * @property {*} [key] - Event parameters
 */
/**
 * Arizona Framework JavaScript Client
 * Provides real-time WebSocket communication with the Arizona server
 */
export default class Arizona {
    /**
     * Creates a new Arizona client instance
     * @param {ArizonaOptions} [opts={}] - Client configuration options
     */
    constructor(opts?: ArizonaOptions);
    /** @type {Worker|null} */
    worker: Worker | null;
    /** @type {boolean} */
    connected: boolean;
    /** @type {Map<string, Set<Function>>} */
    eventListeners: Map<string, Set<Function>>;
    /** @type {import('./logger/arizona-logger.js').default|null} */
    logger: import('./logger/arizona-logger.js').default | null;
    /** @type {number} */
    nextRefId: number;
    /** @type {Map<string, {resolve: Function, reject: Function, timeout: number}>} */
    pendingCalls: Map<string, {
        resolve: Function;
        reject: Function;
        timeout: number;
    }>;
    /**
     * Initialize worker if not already created
     * @private
     * @returns {void}
     */
    private initializeWorker;
    /**
     * Connect to the Arizona WebSocket server
     * @param {string} websocketEndpoint - WebSocket endpoint path
     * @returns {void}
     */
    connect(websocketEndpoint: string): void;
    /**
     * Push an event to the Arizona server
     * @param {string} event - Event name
     * @param {EventParams} [params={}] - Event parameters
     * @returns {void}
     */
    pushEvent(event: string, params?: EventParams): void;
    /**
     * Push an event to a specific stateful component
     * @param {string} statefulId - Target stateful component ID
     * @param {string} event - Event name
     * @param {EventParams} [params={}] - Event parameters
     * @returns {void}
     */
    pushEventTo(statefulId: string, event: string, params?: EventParams): void;
    /**
     * Call an event on the Arizona server and wait for reply
     * @param {string} event - Event name
     * @param {EventParams} [params={}] - Event parameters
     * @param {Object} [options={}] - Call options
     * @param {number} [options.timeout=10000] - Timeout in milliseconds
     * @returns {Promise<*>} Promise that resolves with reply data
     */
    callEvent(event: string, params?: EventParams, options?: {
        timeout?: number | undefined;
    }): Promise<any>;
    /**
     * Call an event on a specific stateful component and wait for reply
     * @param {string} statefulId - Target stateful component ID
     * @param {string} event - Event name
     * @param {EventParams} [params={}] - Event parameters
     * @param {Object} [options={}] - Call options
     * @param {number} [options.timeout=10000] - Timeout in milliseconds
     * @returns {Promise<*>} Promise that resolves with reply data
     */
    callEventFrom(statefulId: string, event: string, params?: EventParams, options?: {
        timeout?: number | undefined;
    }): Promise<any>;
    /**
     * Internal helper to call an event and wait for reply
     * @private
     * @param {string|undefined} statefulId - Target stateful component ID (undefined for view)
     * @param {string} event - Event name
     * @param {EventParams} params - Event parameters
     * @param {Object} options - Call options
     * @returns {Promise<*>} Promise that resolves with reply data
     */
    private _callEvent;
    /**
     * Disconnect from the Arizona WebSocket server
     * @returns {void}
     */
    disconnect(): void;
    /**
     * Handle messages from the worker thread
     * @private
     * @param {Object} message - Worker message
     * @returns {void}
     */
    private handleWorkerMessage;
    handleStatus(data: any): void;
    handleHtmlPatch(data: any): void;
    applyHtmlPatch(patch: any): void;
    handleWorkerError(data: any): void;
    handleReload(data: any): void;
    handleDispatch(data: any): void;
    handleReply(data: any): void;
    handleRedirect(data: any): void;
    handleUnknownMessage(message: any): void;
    /**
     * Check if client is connected to server
     * @returns {boolean} True if connected
     */
    isConnected(): boolean;
    /**
     * Subscribe to an Arizona event
     * @param {string} event - Event name (e.g., 'connected', 'disconnected')
     * @param {Function} callback - Callback function to invoke when event occurs
     * @returns {Function} Unsubscribe function
     */
    on(event: string, callback: Function): Function;
    /**
     * Subscribe to an Arizona event that will only fire once
     * @param {string} event - Event name
     * @param {Function} callback - Callback function to invoke when event occurs
     * @returns {Function} Unsubscribe function
     */
    once(event: string, callback: Function): Function;
    /**
     * Unsubscribe from an Arizona event
     * @param {string} event - Event name
     * @param {Function} callback - Callback function to remove
     * @returns {void}
     */
    off(event: string, callback: Function): void;
    /**
     * Remove all listeners for a specific event, or all events if no event specified
     * @param {string} [event] - Optional event name. If not provided, removes all listeners for all events
     * @returns {void}
     */
    removeAllListeners(event?: string): void;
    /**
     * Emit an Arizona event to all subscribed listeners
     * @private
     * @param {string} event - Event name
     * @param {*} data - Event data to pass to listeners
     * @returns {void}
     */
    private emit;
}
export type ArizonaOptions = {
    /**
     * - Logger implementation
     */
    logger?: import('./logger/arizona-logger.js').default | undefined;
};
export type ConnectOptions = {
    /**
     * - WebSocket path (default: '/live')
     */
    wsPath?: string | undefined;
};
export type EventParams = {
    /**
     * - Event parameters
     */
    key?: any;
};
