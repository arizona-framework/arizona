---
name: add-test
description: Add tests for an Arizona module. Use when creating CT suites, inline EUnit tests, or E2E tests.
argument-hint: [module_or_file]
allowed-tools: Read, Write, Edit, Grep, Glob, Bash
---

Add tests for `$ARGUMENTS`.

## 1. Determine test type

Read the module to understand what it does, then choose the right test approach:

**Common Test (default)** -- for all Erlang tests (unit, stateful, integration):
- Create `test/<module>_SUITE.erl`
- Include `-include_lib("stdlib/include/assert.hrl").`
- Export `all/0` and all test functions with arity `/1`
- Test functions take `(Config) when is_list(Config) ->` -- no `_test` suffix
- Add `init_per_suite/1`, `end_per_suite/1` only if setup is needed
- Support modules (handlers, helpers) go in `test/support/`
- Run: `rebar3 ct --suite=<module>_SUITE`

**EUnit (inline only)** -- for testing private functions within a source module:
- Add `-ifdef(TEST).` / `-endif.` block at the bottom of the source module
- Include `-include_lib("eunit/include/eunit.hrl").`
- Test functions end with `_test()` (EUnit convention)
- Run: `rebar3 eunit --module=<module>`
- Only use this when you need access to private (non-exported) functions

**E2E (Playwright)** -- for full browser interaction:
- Create spec in `e2e/parallel/<name>.spec.js` (or `e2e/sequential/` if tests share state)
- Sequential tests need `workers: 1` in config to avoid pg channel leaks
- Run: `npx playwright test <spec_file>`

## 2. CT suite structure

Use named `groups/0` matching section comments in the file. Use `[parallel]` for stateless tests, `[sequence]` for tests with shared state (persistent_term, pg, ets). `all/0` must come before `groups/0`. Never use list comprehensions in `all/0`.

```erlang
-module(my_module_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    renders_html/1,
    diffs_no_change/1,
    diffs_attr_change/1
]).

%% all/0 before groups/0, plain list of {group, Name} tuples
all() ->
    [
        {group, render},
        {group, diff}
    ].

%% Group names match section comments in the file
groups() ->
    [
        {render, [parallel], [
            renders_html
        ]},
        {diff, [parallel], [
            diffs_no_change,
            diffs_attr_change
        ]}
    ].

%% =============================================================================
%% render
%% =============================================================================

renders_html(Config) when is_list(Config) ->
    T = Handler:render(Handler:mount(#{})),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertMatch(<<"<div", _/binary>>, HTML).

%% =============================================================================
%% diff
%% =============================================================================

diffs_no_change(Config) when is_list(Config) ->
    B = Handler:mount(#{}),
    {_HTML, Snap} = arizona_render:render(Handler:render(B)),
    {Ops, _Snap2} = arizona_diff:diff(Handler:render(B), Snap),
    ?assertEqual([], Ops).

diffs_attr_change(Config) when is_list(Config) ->
    ok.
```

**Cleanup rules:**
- ALL setup (start processes, subscribe, create dirs) goes in `init_per_testcase`
- ALL cleanup (stop processes, unsubscribe, delete dirs) goes in `end_per_testcase`
- Test bodies NEVER do cleanup -- if the test crashes, `end_per_testcase` still runs
- Use pattern matching on test name for per-test setup/cleanup variations
- Use a `watcher_opts(TC)` style helper to centralize per-test config

For parse transform, compile source as string:
```erlang
compile_and_render(Config) when is_list(Config) ->
    Mod = compile_module("-module(test_mod). "
        "-include(\"arizona_stateless.hrl\"). "
        "-export([render/1]). "
        "render(Bindings) -> ?html({p, [], [?get(x)]})."),
    T = Mod:render(#{x => <<"hello">>}),
    ?assert(is_map(T)).
```

## 3. Run and verify

```bash
rebar3 ct --suite=$ARGUMENTS_SUITE      # single CT suite
rebar3 eunit --module=$ARGUMENTS        # inline EUnit tests
npx playwright test $ARGUMENTS.spec.js  # single E2E spec
```
