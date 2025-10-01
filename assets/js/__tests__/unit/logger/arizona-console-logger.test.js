// ArizonaConsoleLogger tests
import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import ArizonaConsoleLogger from '../../../client/logger/arizona-console-logger.js';
import { LOG_LEVELS } from '../../../client/logger/arizona-logger.js';

describe('ArizonaConsoleLogger', () => {
  let logger;
  let consoleSpies;

  beforeEach(() => {
    logger = new ArizonaConsoleLogger();

    // Spy on all console methods
    consoleSpies = {
      error: vi.spyOn(console, 'error').mockImplementation(() => {}),
      warn: vi.spyOn(console, 'warn').mockImplementation(() => {}),
      log: vi.spyOn(console, 'log').mockImplementation(() => {}),
    };
  });

  afterEach(() => {
    // Restore all console methods
    Object.values(consoleSpies).forEach((spy) => spy.mockRestore());
  });

  describe('constructor', () => {
    test('initializes with default log level (info)', () => {
      expect(logger.options.logLevel).toBe(LOG_LEVELS.info);
    });

    test('accepts custom log level', () => {
      const debugLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug });
      expect(debugLogger.options.logLevel).toBe(LOG_LEVELS.debug);
    });

    test('accepts error log level', () => {
      const errorLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.error });
      expect(errorLogger.options.logLevel).toBe(LOG_LEVELS.error);
    });
  });

  describe('handleLog() implementation', () => {
    test('uses console.error for error level', () => {
      logger.handleLog(LOG_LEVELS.error, 'error message', 'detail');

      expect(consoleSpies.error).toHaveBeenCalledWith('[Arizona]', 'error message', 'detail');
      expect(consoleSpies.warn).not.toHaveBeenCalled();
      expect(consoleSpies.log).not.toHaveBeenCalled();
    });

    test('uses console.warn for warning level', () => {
      logger.handleLog(LOG_LEVELS.warning, 'warning message', 'detail');

      expect(consoleSpies.warn).toHaveBeenCalledWith('[Arizona]', 'warning message', 'detail');
      expect(consoleSpies.error).not.toHaveBeenCalled();
      expect(consoleSpies.log).not.toHaveBeenCalled();
    });

    test('uses console.log for info level', () => {
      logger.handleLog(LOG_LEVELS.info, 'info message', 'detail');

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'info message', 'detail');
      expect(consoleSpies.error).not.toHaveBeenCalled();
      expect(consoleSpies.warn).not.toHaveBeenCalled();
    });

    test('uses console.log for debug level', () => {
      logger.handleLog(LOG_LEVELS.debug, 'debug message', 'detail');

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'debug message', 'detail');
      expect(consoleSpies.error).not.toHaveBeenCalled();
      expect(consoleSpies.warn).not.toHaveBeenCalled();
    });

    test('uses console.log for unknown levels (fallback)', () => {
      logger.handleLog(999, 'unknown level message');

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'unknown level message');
    });

    test('adds [Arizona] prefix to all messages', () => {
      logger.handleLog(LOG_LEVELS.error, 'test');
      logger.handleLog(LOG_LEVELS.warning, 'test');
      logger.handleLog(LOG_LEVELS.info, 'test');
      logger.handleLog(LOG_LEVELS.debug, 'test');

      expect(consoleSpies.error).toHaveBeenCalledWith('[Arizona]', 'test');
      expect(consoleSpies.warn).toHaveBeenCalledWith('[Arizona]', 'test');
      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'test');
    });

    test('passes all arguments to console methods', () => {
      const obj = { key: 'value' };
      const arr = [1, 2, 3];

      logger.handleLog(LOG_LEVELS.info, 'message', obj, arr, 'string', 123);

      expect(consoleSpies.log).toHaveBeenCalledWith(
        '[Arizona]',
        'message',
        obj,
        arr,
        'string',
        123
      );
    });
  });

  describe('convenience methods with filtering', () => {
    test('error() always logs (at info level)', () => {
      logger.error('error message', 'detail');

      expect(consoleSpies.error).toHaveBeenCalledWith('[Arizona]', 'error message', 'detail');
    });

    test('warning() logs at info level', () => {
      logger.warning('warning message', 'detail');

      expect(consoleSpies.warn).toHaveBeenCalledWith('[Arizona]', 'warning message', 'detail');
    });

    test('info() logs at info level', () => {
      logger.info('info message', 'detail');

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'info message', 'detail');
    });

    test('debug() is filtered at default info level', () => {
      logger.debug('debug message', 'detail');

      expect(consoleSpies.log).not.toHaveBeenCalled();
      expect(consoleSpies.error).not.toHaveBeenCalled();
      expect(consoleSpies.warn).not.toHaveBeenCalled();
    });
  });

  describe('log level filtering', () => {
    test('error level logs only errors', () => {
      const errorLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.error });

      errorLogger.error('error');
      errorLogger.warning('warning');
      errorLogger.info('info');
      errorLogger.debug('debug');

      expect(consoleSpies.error).toHaveBeenCalledTimes(1);
      expect(consoleSpies.warn).not.toHaveBeenCalled();
      expect(consoleSpies.log).not.toHaveBeenCalled();
    });

    test('warning level logs errors and warnings', () => {
      const warningLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.warning });

      warningLogger.error('error');
      warningLogger.warning('warning');
      warningLogger.info('info');
      warningLogger.debug('debug');

      expect(consoleSpies.error).toHaveBeenCalledTimes(1);
      expect(consoleSpies.warn).toHaveBeenCalledTimes(1);
      expect(consoleSpies.log).not.toHaveBeenCalled();
    });

    test('info level logs errors, warnings, and info', () => {
      const infoLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.info });

      infoLogger.error('error');
      infoLogger.warning('warning');
      infoLogger.info('info');
      infoLogger.debug('debug');

      expect(consoleSpies.error).toHaveBeenCalledTimes(1);
      expect(consoleSpies.warn).toHaveBeenCalledTimes(1);
      expect(consoleSpies.log).toHaveBeenCalledTimes(1);
    });

    test('debug level logs all messages', () => {
      const debugLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug });

      debugLogger.error('error');
      debugLogger.warning('warning');
      debugLogger.info('info');
      debugLogger.debug('debug');

      expect(consoleSpies.error).toHaveBeenCalledTimes(1);
      expect(consoleSpies.warn).toHaveBeenCalledTimes(1);
      expect(consoleSpies.log).toHaveBeenCalledTimes(2); // info + debug
    });
  });

  describe('complex arguments', () => {
    beforeEach(() => {
      logger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug });
    });

    test('handles objects', () => {
      const obj = { nested: { key: 'value' } };
      logger.info('Object:', obj);

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'Object:', obj);
    });

    test('handles arrays', () => {
      const arr = [1, 2, { key: 'value' }];
      logger.info('Array:', arr);

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'Array:', arr);
    });

    test('handles errors', () => {
      const error = new Error('Test error');
      logger.error('Error occurred:', error);

      expect(consoleSpies.error).toHaveBeenCalledWith('[Arizona]', 'Error occurred:', error);
    });

    test('handles mixed argument types', () => {
      logger.info('Mixed:', 'string', 123, true, null, undefined, { obj: true }, [1, 2]);

      expect(consoleSpies.log).toHaveBeenCalledWith(
        '[Arizona]',
        'Mixed:',
        'string',
        123,
        true,
        null,
        undefined,
        { obj: true },
        [1, 2]
      );
    });

    test('handles no additional arguments', () => {
      logger.info('Just message');

      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'Just message');
    });
  });

  describe('integration with ArizonaLogger base class', () => {
    test('inherits log level filtering from base class', () => {
      const errorLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.error });

      // log() method is from base class
      errorLogger.log(LOG_LEVELS.error, 'should log');
      errorLogger.log(LOG_LEVELS.warning, 'should not log');

      expect(consoleSpies.error).toHaveBeenCalledTimes(1);
      expect(consoleSpies.warn).not.toHaveBeenCalled();
    });

    test('inherits convenience methods from base class', () => {
      const debugLogger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug });

      debugLogger.error('error');
      debugLogger.warning('warning');
      debugLogger.info('info');
      debugLogger.debug('debug');

      // All should log at debug level
      expect(consoleSpies.error).toHaveBeenCalledWith('[Arizona]', 'error');
      expect(consoleSpies.warn).toHaveBeenCalledWith('[Arizona]', 'warning');
      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'info');
      expect(consoleSpies.log).toHaveBeenCalledWith('[Arizona]', 'debug');
    });
  });
});
