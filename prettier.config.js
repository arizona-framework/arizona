/**
 * @see https://prettier.io/docs/configuration
 * @type {import("prettier").Config}
 */
export default {
  // Default configuration for all files
  printWidth: 100,
  tabWidth: 2,
  useTabs: false,
  semi: true,
  trailingComma: 'es5',
  bracketSpacing: true,
  arrowParens: 'always',

  overrides: [
    // JavaScript files
    {
      files: ['*.{js,mjs}'],
      options: {
        parser: 'babel',
        singleQuote: true,
      },
    },
    // CSS files
    {
      files: ['*.css'],
      options: {
        parser: 'css',
      },
    },
    // JSON files
    {
      files: ['*.json'],
      options: {
        parser: 'json',
        tabWidth: 2,
        printWidth: 80,
      },
      excludeFiles: ['package-lock.json'],
    },
    // YAML files
    {
      files: ['*.yml'],
      options: {
        parser: 'yaml',
        proseWrap: 'preserve',
      },
    },
  ],
};
