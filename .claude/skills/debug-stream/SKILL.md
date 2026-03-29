---
name: debug-stream
description: Debug Arizona stream operations. Use when investigating stream insert/delete/update/move/sort/reset behavior.
argument-hint: [handler_module]
allowed-tools: Read, Grep, Glob
---

Debug stream operations in `$ARGUMENTS`.

## 1. Stream setup

Find how the stream is created in `mount/1`:
- Key function used in `arizona_stream:new/1,2,3`
- Initial items and order
- Limit and on_limit options

## 2. Template binding

Find the `?each(Fun, Source)` call in `render/1`:
- How items are mapped to dynamics
- Template structure for each item (statics, dynamics)

## 3. Mutations

Find `handle_event/3` clauses that modify the stream:
- Which `arizona_stream` functions are called (insert, delete, update, move, sort, reset)
- What pending ops each mutation generates
- How `clear_stream_pending/2` is called after diff

## 4. Diff path

Trace how `arizona_diff:diff_stream/4` processes pending ops:
- Queue processing order
- Op generation: `OP_INSERT`, `OP_REMOVE`, `OP_ITEM_PATCH`, `OP_MOVE`
- Limit reconciliation via `apply_limit/5`
- Reorder via LIS algorithm (`compute_reorder_ops`)

Reference files:
- Stream data structure: `src/arizona_stream.erl`
- Stream eval: `src/arizona_eval.erl` (render_stream_item, eval_stream_items)
- Stream diff: `src/arizona_diff.erl` (diff_stream, diff_stream_pending, diff_stream_op)
