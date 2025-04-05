export default {
  testRegex: 'assets/.*\\.test\\.js$',
  testPathIgnorePatterns: ['_build/', '.github'],
  modulePathIgnorePatterns: ['_build/', '.github'],
  collectCoverage: true,
  coveragePathIgnorePatterns: ['_build/', '.github'],
  coverageDirectory: '_build/test/cover/js',
  coverageProvider: 'v8',
  transform: {
    '^.+\\.(js|mjs)$': 'babel-jest',
  },
  moduleFileExtensions: ['js', 'mjs', 'json'],
};
