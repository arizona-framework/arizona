# Contributing to Arizona

1. [Development Setup](#development-setup)
1. [Testing](#testing)
1. [Pre-commit Hooks](#pre-commit-hooks)
1. [License](#license)
1. [Reporting a bug](#reporting-a-bug)
1. [Requesting or implementing a feature](#requesting-or-implementing-a-feature)
1. [Submitting your changes](#submitting-your-changes)
   1. [Code Style](#code-style)
   1. [Committing your changes](#committing-your-changes)
   1. [Pull requests and branching](#pull-requests-and-branching)
   1. [Credits](#credits)

## Development Setup

Arizona uses modern Erlang/OTP patterns. See [.tool-versions](../.tool-versions) for exact version requirements.

### Quick Start

```bash
# Clone the repository
git clone https://github.com/arizona-framework/arizona.git
cd arizona

# Install JavaScript dependencies (includes Husky pre-commit hooks)
npm install

# Compile Erlang code
rebar3 compile

# Run all checks and tests to verify setup
rebar3 ci
npm test
```

### Code Quality Tools

Arizona maintains high code quality with:

- **Comprehensive type contracts** with Dialyzer
- **Elvis linting** for code quality
- **Xref analysis** for unused exports
- **Full test coverage** with 319+ tests
- **Pre-commit hooks** with Husky for automatic quality checks

## Testing

Arizona includes comprehensive test coverage across multiple layers:

### Erlang Tests

```bash
# Run all checks and tests (recommended)
rebar3 ci

# Run individual checks
rebar3 check    # lint, hank, xref, dialyzer
rebar3 ct       # Common Test suites

# Run specific test suite
rebar3 ct --suite test/arizona_live_SUITE

# Run with coverage (prints table + generates HTML report)
rebar3 as test do ct, cover -v
```

### JavaScript Tests

```bash
# Run unit tests
npm run test:unit

# Run E2E tests (requires compiled Erlang code)
npm run test:e2e

# Run all tests
npm test

# Run with coverage
npm run test:unit:coverage

# Check code formatting
npm run format:check
npm run lint:check
```

### Manual Testing

```bash
# Start test server for manual testing
./scripts/start_test_server.sh

# Visit http://localhost:8080/counter
# Visit http://localhost:8080/todo
```

## Pre-commit Hooks

Arizona includes pre-commit hooks that automatically run when you commit. These hooks help
maintain code quality and prevent common issues.

### What Gets Checked

- **Commit message format** (72 character limit)
- **Merge conflict markers** detection
- **Erlang code formatting** (`rebar3 fmt --check`)
- **JavaScript code formatting** (`npm run format:check`)

### Automatic Installation

The hooks are automatically installed when you run `npm install` thanks to Husky. No manual setup required!

### Fixing Issues

If a hook fails, fix the issue and try committing again:

```bash
# Fix Erlang formatting
rebar3 fmt

# Fix JavaScript formatting
npm run format

# Then commit again
git commit -m "Your commit message"
```

## License

Arizona is licensed under the [Apache License Version 2.0](LICENSE.md), for all code.

## Reporting a bug

Arizona is not perfect software and will be buggy.

Bugs can be reported via
[GitHub issues: bug report](https://github.com/arizona-framework/arizona/issues/new?template=bug_report.md).

Some contributors and maintainers may be unpaid developers working on Arizona, in their own time,
with limited resources. We ask for respect and understanding, and will provide the same back.

If your contribution is an actual bug fix, we ask you to include tests that, not only show the issue
is solved, but help prevent future regressions related to it.

## Requesting or implementing a feature

Before requesting or implementing a new feature, do the following:

- search, in existing [issues](https://github.com/arizona-framework/arizona/issues) (open or closed),
whether the feature might already be in the works, or has already been rejected,
- make sure you're using the latest software release (or even the latest code, if you're going for
_bleeding edge_).

If this is done, open up a
[GitHub issues: feature request](https://github.com/arizona-framework/arizona/issues/new?template=feature_request.md).

We may discuss details with you regarding the implementation, and its inclusion within the project.

We try to have as many of Arizona's features tested as possible. Everything that a user can do,
and is repeatable in any way, should be tested, to guarantee backwards compatible.

## Submitting your changes

### Code Style

- do not introduce trailing whitespace
- indentation is 4 spaces, not tabs
- try not to introduce lines longer than 100 characters
- write small functions whenever possible, and use descriptive names for functions and variables
- comment tricky or non-obvious decisions made to explain their rationale

### Committing your changes

Merging to the `main` branch will usually be preceded by a squash.

While it's Ok (and expected) your commit messages relate to why a given change was made, be aware
that the final commit (the merge one) will be the issue title, so it's important it is as specific
as possible. This will also help eventual automated changelog generation.

### Pull requests and branching

All fixes to Arizona end up requiring a +1 from one or more of the project's maintainers.

During the review process, you may be asked to correct or edit a few things before a final rebase
to merge things. Do send edits as individual commits to allow for gradual and partial reviews to be
done by reviewers.

### Credits

Arizona has been improved by
[many contributors](https://github.com/arizona-framework/arizona/graphs/contributors)!
