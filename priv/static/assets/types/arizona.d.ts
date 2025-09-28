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
    /**
     * Connect to the Arizona WebSocket server
     * @param {string} websocketEndpoint - WebSocket endpoint path
     * @returns {void}
     */
    connect(websocketEndpoint: string): void;
    /**
     * Send an event to the Arizona server
     * @param {string} event - Event name
     * @param {EventParams} [params={}] - Event parameters
     * @returns {void}
     */
    sendEvent(event: string, params?: EventParams): void;
    /**
     * Send an event to a specific stateful component
     * @param {string} statefulId - Target stateful component ID
     * @param {string} event - Event name
     * @param {EventParams} [params={}] - Event parameters
     * @returns {void}
     */
    sendEventTo(statefulId: string, event: string, params?: EventParams): void;
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
    handleReply(data: any): void;
    handleRedirect(data: any): void;
    handleUnknownMessage(message: any): void;
    dispatchArizonaEvent(eventType: any, eventData: any): void;
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
}
export type ArizonaClientOptions = {
    /**
     * - Log level for client output
     */
    logLevel?: "silent" | "error" | "warning" | "info" | "debug" | undefined;
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
