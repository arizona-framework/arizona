-module(arizona_static).
-moduledoc """
Offline static-site generation: render route-level handlers to HTML files.

After the request/transport decoupling, `arizona_render:render_view_to_iolist/2`
renders a handler to HTML with no server, live process, or WebSocket. This
module is the thin batch/file layer over it: mount a handler, render it, and
write the result to disk.

`generate/2,3` renders a list of specs under a base output directory and returns
`{Written, Failed}`: the paths written and `{Spec, Failure}` for any spec that
failed (one failing page does not stop the rest). `generate/3` also takes a
`DefaultOpts` map that each spec's own options override (key-wise), so a shared
layout need not be repeated per page:

```erlang
{Written, Failed} = arizona_static:generate(~"_site", [
    {home_page, ~"index.html"},
    {about_page, ~"about/index.html", #{bindings => #{title => ~"About"}}}
], #{layouts => [{site_layout, render}]}).
```

## Options

`opts()` (per spec, and the `DefaultOpts`) is the offline-relevant subset of
`t:arizona_live:route_opts/0`: `bindings`, `on_mount`, and `layouts`.
`middlewares` does not apply offline (there is no request); any extra keys are
ignored, so a route's opts map can be pasted in. A spec's own options override
`DefaultOpts` key-wise (a map value like `bindings` is replaced, not deep-merged).

## Failures

A spec that fails -- a `mount`/`render` crash, or a write error (a
`filelib:ensure_dir`/`file:write_file` `{error, _}`) -- is collected in `Failed`
rather than stopping the batch; the caller decides what is fatal. A
structurally-malformed spec (not a 2- or 3-tuple) still crashes -- that's a
caller bug, not a per-page failure.

A crash is collected as the whole exception, `{Class, Reason, Stacktrace}` (see
`t:failure/0`), so one bad page in a 200-page build still says *where* it broke
-- the `{arizona_loc, {Module, Line}, _}` wrapper `arizona_render` attaches
included. A write error keeps the `file` module's own `{error, Reason}`, which
the tuple size tells apart from a crash. Render one:

```erlang
{_Written, Failed} = arizona_static:generate(~"_site", Specs),
[io:format("~ts: ~ts~n", [Outfile, erl_error:format_exception(C, R, ST)])
 || {{_Handler, Outfile}, {C, R, ST}} <- Failed].
```

## Caveats

Generated pages are SSR HTML. The client `<script>` / `connect('/ws')` lives in
your layout, not here: a purely static page uses a layout without the WS connect.
The output carries the usual `az-view` / `id` markers (inert as static HTML;
meaningful only if later hydrated by an Arizona server). `generate/2,3` writes
files but does not clean `OutDir`, so a page removed from `Specs` leaves its
previously generated file behind.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([generate/2]).
-export([generate/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([generate/2]).
-ignore_xref([generate/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([spec/0]).
-export_type([opts/0]).
-export_type([failure/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal opts() :: #{
    bindings => arizona_template:bindings(),
    on_mount => arizona_live:on_mount(),
    layouts => [arizona_render:layout()]
}.

%% `Outfile` is relative to the `generate/2,3` `OutDir`.
-nominal spec() ::
    {module(), file:filename_all()}
    | {module(), file:filename_all(), opts()}.

%% Why one spec did not produce a file. The 3-tuple is a raised exception kept
%% whole (an `erlang:raise/3`-shaped triple, so a caller can re-raise or hand it
%% to `erl_error:format_exception/3`); the 2-tuple is a `file` module error
%% returned rather than raised. The shapes are distinguishable by size, which a
%% bare reason was not.
-nominal failure() ::
    {Class :: error | exit | throw, Reason :: term(), Stacktrace :: [tuple()]}
    | {error, Reason :: term()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Equivalent to `generate(OutDir, Specs, #{})`.
""".
-spec generate(OutDir, Specs) -> {Written, Failed} when
    OutDir :: file:filename_all(),
    Specs :: [spec()],
    Written :: [file:filename_all()],
    Failed :: [{spec(), failure()}].
generate(OutDir, Specs) ->
    generate(OutDir, Specs, #{}).

-doc """
Renders each spec under `OutDir`, writes the HTML files, and returns
`{Written, Failed}` -- the paths written (in `Specs` order) and
`{Spec, t:failure/0}` for any spec that failed. A failing spec is collected
rather than stopping the batch; the caller decides what is fatal.

A spec is `{Handler, Outfile}` or `{Handler, Outfile, Opts}`, where `Outfile`
is joined onto `OutDir` (an absolute `Outfile` bypasses `OutDir`, per standard
`filename:join` behavior) and parent directories are created as needed. A spec's
own `Opts` override `DefaultOpts` key-wise.
""".
-spec generate(OutDir, Specs, DefaultOpts) -> {Written, Failed} when
    OutDir :: file:filename_all(),
    Specs :: [spec()],
    DefaultOpts :: opts(),
    Written :: [file:filename_all()],
    Failed :: [{spec(), failure()}].
generate(OutDir, Specs, DefaultOpts) when is_list(Specs), is_map(DefaultOpts) ->
    generate_loop(Specs, OutDir, DefaultOpts, [], []).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

generate_loop([], _OutDir, _DefaultOpts, Written, Failed) ->
    {lists:reverse(Written), lists:reverse(Failed)};
generate_loop([Spec | T], OutDir, DefaultOpts, Written, Failed) ->
    case generate_spec(OutDir, DefaultOpts, Spec) of
        {ok, Filename} ->
            generate_loop(T, OutDir, DefaultOpts, [Filename | Written], Failed);
        {failed, Failure} ->
            generate_loop(T, OutDir, DefaultOpts, Written, [{Spec, Failure} | Failed])
    end.

generate_spec(OutDir, DefaultOpts, {Handler, Outfile}) ->
    safe_generate(Handler, OutDir, Outfile, DefaultOpts);
generate_spec(OutDir, DefaultOpts, {Handler, Outfile, Opts}) ->
    safe_generate(Handler, OutDir, Outfile, maps:merge(DefaultOpts, Opts)).

%% A raised failure is collected WHOLE -- class, reason, and stacktrace -- so one
%% bad page in a large batch still says what failed and where (including the
%% `{arizona_loc, {Module, Line}, _}` wrapper `arizona_render` attaches). The
%% batch does not stop either way; the 3-tuple also tells a raise apart from a
%% write error's `{error, Reason}`, which the bare-reason shape could not.
safe_generate(Handler, OutDir, Outfile, Opts) ->
    try
        Filename = filename:join(OutDir, Outfile),
        case generate_one(Handler, Filename, Opts) of
            ok ->
                {ok, Filename};
            {error, _Reason} = Error ->
                {failed, Error}
        end
    catch
        Class:Reason:Stacktrace ->
            {failed, {Class, Reason, Stacktrace}}
    end.

generate_one(Handler, Filename, Opts) when is_atom(Handler), is_map(Opts) ->
    maybe
        RenderOpts = maps:with([bindings, on_mount, layouts], Opts),
        Content = arizona_render:render_view_to_iolist(Handler, RenderOpts),
        ok ?= filelib:ensure_dir(Filename),
        ok ?= file:write_file(Filename, Content),
        ok
    end.
