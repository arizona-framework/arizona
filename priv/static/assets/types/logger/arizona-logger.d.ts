export namespace LOG_LEVELS {
    let error: number;
    let warning: number;
    let info: number;
    let debug: number;
}
export default ArizonaLogger;
/**
 * Base class for Arizona logging strategies
 *
 * Implementations must provide a `handleLog` method that handles
 * writing log messages to the output target.
 */
declare class ArizonaLogger {
    /**
     * @param {Object} [options] - Logger options
     * @param {number} [options.logLevel] - Minimum log level to display (default: LOG_LEVELS.info)
     */
    constructor(options?: {
        logLevel?: number | undefined;
    });
    options: {
        logLevel?: number | undefined;
    };
    /**
     * Handle a log message (must be implemented by subclass)
     * @param {number} level - Log level (from LOG_LEVELS enum)
     * @param {string} message - Log message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    handleLog(level: number, message: string, ...args: any[]): void;
    /**
     * Log a message with level filtering
     * @param {number} level - Log level (from LOG_LEVELS enum)
     * @param {string} message - Log message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    log(level: number, message: string, ...args: any[]): void;
    /**
     * Log error message
     * @param {string} message - Error message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    error(message: string, ...args: any[]): void;
    /**
     * Log warning message
     * @param {string} message - Warning message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    warning(message: string, ...args: any[]): void;
    /**
     * Log info message
     * @param {string} message - Info message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    info(message: string, ...args: any[]): void;
    /**
     * Log debug message
     * @param {string} message - Debug message
     * @param {...*} args - Additional arguments
     * @returns {void}
     */
    debug(message: string, ...args: any[]): void;
}
