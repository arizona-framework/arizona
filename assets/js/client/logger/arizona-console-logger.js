import ArizonaLogger, { LOG_LEVELS } from './arizona-logger.js';

/**
 * Console-based logger implementation
 *
 * Logs messages to the browser console.
 * Default opinionated behavior:
 * - Adds [Arizona] prefix to all messages
 * - Uses appropriate console methods (error, warn, log)
 * - Inherits level filtering from ArizonaLogger base class
 *
 * @example
 * // With default options (info level)
 * import { ArizonaConsoleLogger, LOG_LEVELS } from '@arizona-framework/client/logger';
 * const logger = new ArizonaConsoleLogger();
 *
 * // With custom log level
 * const logger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug });
 */
export default class ArizonaConsoleLogger extends ArizonaLogger {
  handleLog(level, message, ...args) {
    // Map level to console method
    const prefix = '[Arizona]';
    switch (level) {
      case LOG_LEVELS.error:
        console.error(prefix, message, ...args);
        break;
      case LOG_LEVELS.warning:
        console.warn(prefix, message, ...args);
        break;
      case LOG_LEVELS.info:
      case LOG_LEVELS.debug:
        console.log(prefix, message, ...args);
        break;
      default:
        console.log(prefix, message, ...args);
    }
  }
}
