// Log levels (aligned with Erlang logger levels)
export const LOG_LEVELS = {
  error: 3,
  warning: 4,
  info: 6,
  debug: 7,
};

/**
 * Base class for Arizona logging strategies
 *
 * Implementations must provide a `handleLog` method that handles
 * writing log messages to the output target.
 */
class ArizonaLogger {
  /**
   * @param {Object} [options] - Logger options
   * @param {number} [options.logLevel] - Minimum log level to display (default: LOG_LEVELS.info)
   */
  constructor(options = {}) {
    this.options = options;
    this.options.logLevel = this.options.logLevel ?? LOG_LEVELS.info;
  }

  /**
   * Sanitizes a value for safe logging by removing control characters from strings
   * @private
   * @param {*} value - The value to sanitize
   * @returns {*} - The sanitized value (strings sanitized, other types passed through)
   */
  sanitize(value) {
    // Only sanitize strings - preserve objects/errors/numbers for proper formatting
    if (typeof value !== 'string') return value;

    // Remove control characters from strings to prevent log injection
    return (
      value
        // eslint-disable-next-line no-control-regex
        .replace(/[\r\n\u2028\u2029\t\v\f\b\0\x1B]/g, '')
        // eslint-disable-next-line no-control-regex
        .replace(/[\x00-\x1f\x7f]/g, '') // further strip ASCII control chars
        .trim()
    );
  }

  /**
   * Handle a log message (must be implemented by subclass)
   * @param {number} level - Log level (from LOG_LEVELS enum)
   * @param {string} message - Log message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  // eslint-disable-next-line no-unused-vars
  handleLog(level, message, ...args) {
    throw new Error('handleLog() must be implemented by subclass');
  }

  /**
   * Log a message with level filtering and sanitization
   * @param {number} level - Log level (from LOG_LEVELS enum)
   * @param {string} message - Log message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  log(level, message, ...args) {
    // Filter by log level
    if (level > this.options.logLevel) {
      return;
    }

    // Sanitize message and args to prevent log injection attacks
    const sanitizedMessage = this.sanitize(message);
    const sanitizedArgs = args.map((arg) => {
      return this.sanitize(arg);
    });

    this.handleLog(level, sanitizedMessage, ...sanitizedArgs);
  }

  /**
   * Log error message
   * @param {string} message - Error message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  error(message, ...args) {
    this.log(LOG_LEVELS.error, message, ...args);
  }

  /**
   * Log warning message
   * @param {string} message - Warning message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  warning(message, ...args) {
    this.log(LOG_LEVELS.warning, message, ...args);
  }

  /**
   * Log info message
   * @param {string} message - Info message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  info(message, ...args) {
    this.log(LOG_LEVELS.info, message, ...args);
  }

  /**
   * Log debug message
   * @param {string} message - Debug message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  debug(message, ...args) {
    this.log(LOG_LEVELS.debug, message, ...args);
  }
}

export default ArizonaLogger;
