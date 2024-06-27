# Contributing to Arizona

1. [License](#license)
1. [Reporting a bug](#reporting-a-bug)
1. [Requesting or implementing a feature](#requesting-or-implementing-a-feature)
1. [Submitting your changes](#submitting-your-changes)
   1. [Code Style](#code-style)
   1. [Committing your changes](#committing-your-changes)
   1. [Pull requests and branching](#pull-requests-and-branching)
   1. [Credits](#credits)

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
