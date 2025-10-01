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
   * Log a message with level filtering
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

    this.handleLog(level, message, ...args);
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
