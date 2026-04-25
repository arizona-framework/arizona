---
name: explain-op
description: Explain what an Arizona op code does on both server and client. Use when understanding patch operations.
argument-hint: [op_name_or_number]
allowed-tools: Read, Grep, Glob
---

Explain the `$ARGUMENTS` op code.

## Op code reference

| Code | Name | Args |
|------|------|------|
| 0 | OP_TEXT | [target, value] |
| 1 | OP_SET_ATTR | [target, attr, value] |
| 2 | OP_REM_ATTR | [target, attr] |
| 3 | OP_UPDATE | [target, html] |
| 4 | OP_REMOVE_NODE | [target] |
| 5 | OP_INSERT | [target, key, pos, html] |
| 6 | OP_REMOVE | [target, key] |
| 7 | OP_ITEM_PATCH | [target, key, innerOps] |
| 8 | OP_REPLACE | [target, html] |
| 9 | OP_MOVE | [target, key, afterKey] |

For the requested op, explain:

## 1. Server-side generation

Find where this op is produced in `src/arizona_diff.erl`:
- Which `make_op` or `make_child_op` clause generates it
- What condition triggers it (value change, attribute change, stream mutation, etc.)
- Show the wire format with example values

## 2. Client-side application

Find how `assets/js/arizona.js` handles this op in `applyOps`:
- DOM manipulation performed
- Target resolution via `resolveEl`
- For OP_TEXT: comment marker navigation
- For OP_INSERT/OP_REMOVE: stream container manipulation

## 3. Hook lifecycle

Which hook callbacks fire:
- `mounted()` -- for newly added elements
- `updated()` -- for modified elements
- `destroyed()` -- for removed elements
- Whether `destroyHooks` or `destroyChildHooks` is used

## 4. Example

Show a concrete scenario where this op is produced and applied.
