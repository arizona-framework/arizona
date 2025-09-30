/**
 * Arizona Framework JavaScript Client
 * Provides real-time WebSocket communication with the Arizona server
 */
export default class ArizonaClient {
    /**
     * Creates a new Arizona client instance
     * @param {ArizonaClientOptions} [opts={}] - Client configuration options
     */
    constructor(opts?: ArizonaClientOptions);
    /** @type {Worker|null} */
    worker: Worker | null;
    /** @type {boolean} */
    connected: boolean;
    /** @type {number} */
    logLevel: number;
    /** @type {Map<string, Set<Function>>} */
    eventListeners: Map<string, Set<Function>>;
    /** @type {import('./patcher/arizona-patcher.js').default|null} */
    patcher: import('./patcher/arizona-patcher.js').default | null;
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
    handleRedirect(data: any): void;
    handleUnknownMessage(message: any): void;
    /**
     * Check if client is connected to server
     * @returns {boolean} True if connected
     */
    isConnected(): boolean;
    /**
     * Log error message (always shown)
     * @param {string} message - Error message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    error(message: string, ...args: any[]): void;
    /**
     * Log warning message (shown if log level allows)
     * @param {string} message - Warning message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    warning(message: string, ...args: any[]): void;
    /**
     * Log info message (shown if log level allows)
     * @param {string} message - Info message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    info(message: string, ...args: any[]): void;
    /**
     * Log debug message (shown if log level allows)
     * @param {string} message - Debug message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    debug(message: string, ...args: any[]): void;
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
export type ArizonaClientOptions = {
    /**
     * - Log level for client output
     */
    logLevel?: "silent" | "error" | "warning" | "info" | "debug" | undefined;
    /**
     * - DOM patcher implementation
     */
    patcher?: import('./patcher/arizona-patcher.js').default | undefined;
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
