---
name: commit
description: Format, check, test, and commit changes. Use before committing any code changes.
argument-hint: [commit_message]
allowed-tools: Read, Bash, Grep, Glob
---

Prepare and commit the current changes.

## 1. Snapshot current state

Run `git diff --name-only` and `git diff --name-only --staged` to capture which files are modified before precommit.

## 2. Pre-commit checks

Check the diff from step 1. Only run `make precommit` if `.erl` or `.js` files were touched:
```bash
make precommit
```

If no `.erl` or `.js` files changed (e.g. only docs, configs, markdown), skip `make precommit`.

If it fails, fix the issues and re-run before proceeding. Full CI with E2E is `make ci`.

## 3. Check for new modifications

Run `git diff --name-only` again. If `make precommit` modified files (e.g. formatting changes), show them to the user and ask if they should be included in this commit.

## 4. Review changes

Run `git status` and `git diff` to review what will be committed.

## 5. Commit

If a commit message was provided via `$ARGUMENTS`, use it. Otherwise, draft a concise message based on the changes.

Stage the relevant files and commit. Do not stage files that contain secrets (`.env`, credentials, etc.).
