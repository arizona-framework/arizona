// ArizonaLogger tests
import { describe, test, expect, beforeEach, vi } from 'vitest';
import ArizonaLogger, { LOG_LEVELS } from './arizona-logger.js';

// Create a test implementation of ArizonaLogger
class TestLogger extends ArizonaLogger {
  constructor(options) {
    super(options);
    this.logs = [];
  }

  handleLog(level, message, ...args) {
    this.logs.push({ level, message, args });
  }

  clearLogs() {
    this.logs = [];
  }
}

describe('ArizonaLogger', () => {
  let logger;

  beforeEach(() => {
    logger = new TestLogger();
  });

  describe('LOG_LEVELS', () => {
    test('defines correct log levels aligned with Erlang', () => {
      expect(LOG_LEVELS.error).toBe(3);
      expect(LOG_LEVELS.warning).toBe(4);
      expect(LOG_LEVELS.info).toBe(6);
      expect(LOG_LEVELS.debug).toBe(7);
    });
  });

  describe('constructor', () => {
    test('initializes with default options', () => {
      expect(logger.options).toEqual({ logLevel: LOG_LEVELS.info });
    });

    test('accepts custom logLevel option', () => {
      const customLogger = new TestLogger({ logLevel: LOG_LEVELS.debug });
      expect(customLogger.options.logLevel).toBe(LOG_LEVELS.debug);
    });

    test('accepts custom options object', () => {
      const customLogger = new TestLogger({ logLevel: LOG_LEVELS.error, custom: 'value' });
      expect(customLogger.options.logLevel).toBe(LOG_LEVELS.error);
      expect(customLogger.options.custom).toBe('value');
    });
  });

  describe('handleLog() abstract method', () => {
    test('throws error when not implemented', () => {
      const baseLogger = new ArizonaLogger();
      expect(() => {
        baseLogger.handleLog(LOG_LEVELS.info, 'test');
      }).toThrow('handleLog() must be implemented by subclass');
    });
  });

  describe('log() with level filtering', () => {
    test('logs messages at or below configured level', () => {
      logger.options.logLevel = LOG_LEVELS.info;

      logger.log(LOG_LEVELS.error, 'error message');
      logger.log(LOG_LEVELS.warning, 'warning message');
      logger.log(LOG_LEVELS.info, 'info message');
      logger.log(LOG_LEVELS.debug, 'debug message'); // Should be filtered

      expect(logger.logs).toHaveLength(3);
      expect(logger.logs[0]).toEqual({
        level: LOG_LEVELS.error,
        message: 'error message',
        args: [],
      });
      expect(logger.logs[1]).toEqual({
        level: LOG_LEVELS.warning,
        message: 'warning message',
        args: [],
      });
      expect(logger.logs[2]).toEqual({ level: LOG_LEVELS.info, message: 'info message', args: [] });
    });

    test('filters messages above configured level', () => {
      logger.options.logLevel = LOG_LEVELS.error;

      logger.log(LOG_LEVELS.error, 'error message');
      logger.log(LOG_LEVELS.warning, 'warning message'); // Filtered
      logger.log(LOG_LEVELS.info, 'info message'); // Filtered
      logger.log(LOG_LEVELS.debug, 'debug message'); // Filtered

      expect(logger.logs).toHaveLength(1);
      expect(logger.logs[0].message).toBe('error message');
    });

    test('logs all messages at debug level', () => {
      logger.options.logLevel = LOG_LEVELS.debug;

      logger.log(LOG_LEVELS.error, 'error');
      logger.log(LOG_LEVELS.warning, 'warning');
      logger.log(LOG_LEVELS.info, 'info');
      logger.log(LOG_LEVELS.debug, 'debug');

      expect(logger.logs).toHaveLength(4);
    });

    test('passes additional arguments to handleLog', () => {
      logger.log(LOG_LEVELS.info, 'message', 'arg1', { key: 'value' }, 123);

      expect(logger.logs[0].args).toEqual(['arg1', { key: 'value' }, 123]);
    });
  });

  describe('convenience methods', () => {
    beforeEach(() => {
      logger.options.logLevel = LOG_LEVELS.debug; // Allow all logs
    });

    test('error() calls log with error level', () => {
      logger.error('error message', 'detail');

      expect(logger.logs).toHaveLength(1);
      expect(logger.logs[0].level).toBe(LOG_LEVELS.error);
      expect(logger.logs[0].message).toBe('error message');
      expect(logger.logs[0].args).toEqual(['detail']);
    });

    test('warning() calls log with warning level', () => {
      logger.warning('warning message', 'detail');

      expect(logger.logs).toHaveLength(1);
      expect(logger.logs[0].level).toBe(LOG_LEVELS.warning);
      expect(logger.logs[0].message).toBe('warning message');
    });

    test('info() calls log with info level', () => {
      logger.info('info message', 'detail');

      expect(logger.logs).toHaveLength(1);
      expect(logger.logs[0].level).toBe(LOG_LEVELS.info);
      expect(logger.logs[0].message).toBe('info message');
    });

    test('debug() calls log with debug level', () => {
      logger.debug('debug message', 'detail');

      expect(logger.logs).toHaveLength(1);
      expect(logger.logs[0].level).toBe(LOG_LEVELS.debug);
      expect(logger.logs[0].message).toBe('debug message');
    });

    test('convenience methods respect log level filtering', () => {
      logger.options.logLevel = LOG_LEVELS.warning;

      logger.error('error'); // Logged
      logger.warning('warning'); // Logged
      logger.info('info'); // Filtered
      logger.debug('debug'); // Filtered

      expect(logger.logs).toHaveLength(2);
      expect(logger.logs[0].message).toBe('error');
      expect(logger.logs[1].message).toBe('warning');
    });

    test('convenience methods support multiple arguments', () => {
      logger.info('message', 'arg1', 'arg2', { data: 'value' });

      expect(logger.logs[0].args).toEqual(['arg1', 'arg2', { data: 'value' }]);
    });
  });

  describe('level filtering edge cases', () => {
    test('filters exact threshold correctly', () => {
      logger.options.logLevel = LOG_LEVELS.info;

      logger.log(LOG_LEVELS.info, 'at threshold'); // Should log
      logger.log(LOG_LEVELS.info + 1, 'above threshold'); // Should filter

      expect(logger.logs).toHaveLength(1);
      expect(logger.logs[0].message).toBe('at threshold');
    });

    test('handles very low log level', () => {
      logger.options.logLevel = 0;

      logger.error('error');
      logger.warning('warning');
      logger.info('info');
      logger.debug('debug');

      expect(logger.logs).toHaveLength(0); // All filtered
    });

    test('handles very high log level', () => {
      logger.options.logLevel = 999;

      logger.error('error');
      logger.warning('warning');
      logger.info('info');
      logger.debug('debug');

      expect(logger.logs).toHaveLength(4); // All logged
    });
  });
});
