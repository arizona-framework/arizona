---
name: debug-template
description: Debug an Arizona template's compile/eval/diff flow. Use when investigating how a template compiles, evaluates, or diffs.
argument-hint: [module_or_file]
allowed-tools: Read, Grep, Glob
---

Debug the template in `$ARGUMENTS`. Trace the full lifecycle:

## 1. Compile-time (parse transform)

Read the module's `render/1` function and analyze the `?html(...)` call:
- Show the compiled template map structure: `#{s => Statics, d => Dynamics, f => Fingerprint}`
- List each static segment in `s`
- List each dynamic in `d` with its Az value, type (text/attr/nested), and source location
- Note if `diff => false` is present (from `az-nodiff`)
- Show the fingerprint and how az values are scoped (`fingerprint-N`)

## 2. Runtime evaluation

Trace what happens when `render/1` is called with sample bindings:
- Which `arizona_eval` function handles each dynamic
- For stateful children: mount vs handle_update path
- For stateless children: callback invocation
- For streams/each: item rendering
- Dep tracking: which keys each dynamic reads via `?get`

## 3. Diff behavior

Explain what happens on subsequent renders:
- Which diff function is used (`diff/2`, `diff/3`, or `diff/4`)
- How `diff => false` short-circuits
- Which dynamics are skipped via dep checking
- What ops are produced for changed dynamics

## 4. Wire format

Show the op codes that would be sent to the client and how `arizona.js` applies them.

Reference files:
- Parse transform: `src/arizona_parse_transform.erl`
- Eval: `src/arizona_eval.erl`
- Render: `src/arizona_render.erl`
- Diff: `src/arizona_diff.erl`
- Architecture: `docs/architecture.md`
