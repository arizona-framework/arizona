import pluginJs from '@eslint/js';
import globals from 'globals';

export default [
  {
    files: [
      './*.m?js',
      // Ignored files
      '!**/*.min.js',
    ],
    languageOptions: {
      sourceType: 'module',
      // Globals define variables that are automatically available in environment
      globals: {
        ...globals.browser, // Adds browser globals (window, document, etc.)
        ...globals.node, // Adds Node.js globals (process, require, __dirname)
        ...globals.es2021, // Modern JavaScript globals (Promise, Set, Map)
      },
      parserOptions: {
        ecmaVersion: 'latest',
      },
    },
    rules: {
      // Code Quality
      'no-var': 'error', // Force `let`/`const` instead of `var`
      'prefer-const': 'error', // Use `const` when variables aren’t reassigned
      eqeqeq: 'error', // Require `===` and `!==` (no loose equality)
      'no-implicit-coercion': 'error', // Ban `+str`, `!!bool` coercions
      strict: ['error', 'global'], // Require `'use strict'` (for older JS)
      'no-console': 'off', // Allow console.* functions in client library for logging

      // Error Prevention
      'no-undef': 'error', // All variables must be defined
      'no-unused-vars': 'error', // No unused variables/imports
      'no-shadow': 'error', // Prevent variable shadowing
      'no-param-reassign': 'error', // Don’t reassign function params

      // Async/Await Safety
      'no-await-in-loop': 'error', // Avoid `await` inside loops
      'require-atomic-updates': 'error', // Prevent race conditions in async code

      // Security
      'no-eval': 'error', // Ban `eval()` (security risk)
      'no-implied-eval': 'error', // Ban `setTimeout("code")` (indirect eval)
      'no-script-url': 'error', // Ban `javascript:` URLs (XSS risk)
      'no-multi-str': 'error', // Ban multi-line strings (can hide attacks)

      // Modern JavaScript
      'arrow-body-style': ['error', 'always'], // Force arrow functions
      'prefer-arrow-callback': 'error', // Prefer `() => {}` over `function`
      'prefer-template': 'error', // Force template literals (`${var}`)
      'object-shorthand': 'error', // Force `{ foo }` instead of `{ foo: foo }`
    },
  },
  pluginJs.configs.recommended,
];
