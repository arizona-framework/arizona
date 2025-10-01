import { default as ArizonaLogger } from './arizona-logger.js';
export default ArizonaConsoleLogger;
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
declare class ArizonaConsoleLogger extends ArizonaLogger {
    handleLog(level: any, message: any, ...args: any[]): void;
}
