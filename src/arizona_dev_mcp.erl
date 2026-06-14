-module(arizona_dev_mcp).
-moduledoc """
A dev-time MCP server for an Arizona app, in the spirit of Tidewave for Phoenix.

Mount it in your **dev** config and point a coding agent (Claude Code, Cursor,
the MCP Inspector, ...) at it; the agent can then introspect and drive the
running app:

```erlang
%% in your dev server's routes:
arizona_dev_mcp:route(~"/mcp")
```

Tools:

- `list_routes` -- the app's routes (from the `arizona` `server` app env).
- `describe_component` -- a component's kind (stateful/stateless), its exports, and moduledoc.
- `get_docs` -- a module's or function's documentation (EEP-48).
- `get_source_location` -- where a module (or function) is defined.
- `reloader_status` -- the dev reloader's current compile error, if any.
- `app_info` -- Arizona version, OTP release, node, process count.
- `render_component` -- render a component to HTML with given bindings.
- `eval` -- **run Erlang in the live node**, with bindings that persist across calls.

`eval` is arbitrary remote code execution. Only ever mount this on a trusted,
dev-only, localhost route behind the `Origin` allowlist, and never expose it to
an untrusted client.

It runs in session mode so `eval`'s bindings persist across calls (a REPL the
agent drives) and a slow `eval`/`render_component` runs in the session's worker
rather than blocking the session.
""".
-behaviour(arizona_mcp).

-export([route/1]).
-export([route/2]).
-export([init/1]).
-export([tools/1]).
-export([handle_tool/4]).

%% Mount helpers, called from an app's route config (not from `arizona` itself).
-ignore_xref([route/1]).
-ignore_xref([route/2]).

%% --------------------------------------------------------------------
%% Mount helpers
%% --------------------------------------------------------------------

-doc """
A ready-to-mount dev route. Add `arizona_dev_mcp:route(~"/mcp")` to your **dev**
server's routes; it defaults to `sessions => true` so `eval` is a persistent
REPL. A CLI agent (Claude Code, the Inspector's proxy) connects with no extra
config; `origins` stays opt-in (omitted blocks all browser origins, the safe
DNS-rebinding posture).
""".
-spec route(Path) -> Route when
    Path :: binary(),
    Route :: arizona_roadrunner_router:route().
route(Path) ->
    route(Path, #{}).

-doc """
Like `route/1`, with extra MCP route opts merged over the defaults (e.g.
`#{origins => [~"http://localhost:5173"]}` to also allow a browser client, or
`#{sessions => false}` to opt out of the REPL).
""".
-spec route(Path, Opts) -> Route when
    Path :: binary(),
    Opts :: arizona_roadrunner_router:arizona_mcp_route_opts(),
    Route :: arizona_roadrunner_router:route().
route(Path, Opts) ->
    {mcp, Path, ?MODULE, maps:merge(#{sessions => true}, Opts)}.

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

init(_InitParams) ->
    {ok, #{name => ~"arizona_dev", version => ~"0.1.0"}, #{tools => #{}}, #{
        bindings => erl_eval:new_bindings()
    }}.

tools(_State) ->
    [
        #{
            name => ~"list_routes",
            description => ~"List the routes this Arizona app serves",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"describe_component",
            description => ~"A component's kind (stateful/stateless), its exports, and moduledoc",
            input_schema => module_arg_schema()
        },
        #{
            name => ~"get_docs",
            description => ~"Documentation for a module, or a function within it",
            input_schema => module_function_arg_schema()
        },
        #{
            name => ~"get_source_location",
            description => ~"Where a module (or a function within it) is defined",
            input_schema => module_function_arg_schema()
        },
        #{
            name => ~"reloader_status",
            description => ~"The dev reloader's current compile error, if any",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"app_info",
            description => ~"Arizona version, OTP release, node, and process count",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"render_component",
            description => ~"Render a component module to HTML with the given bindings",
            input_schema => #{
                type => ~"object",
                properties => #{
                    module => #{type => ~"string"},
                    bindings => #{type => ~"object"}
                },
                required => [~"module"]
            }
        },
        #{
            name => ~"eval",
            description => ~"Evaluate Erlang in the live node (dev only); bindings persist",
            input_schema => #{
                type => ~"object",
                properties => #{code => #{type => ~"string"}},
                required => [~"code"]
            }
        }
    ].

handle_tool(~"list_routes", _Args, _Ctx, State) ->
    {reply, list_routes(), State};
handle_tool(~"describe_component", #{~"module" := ModBin}, _Ctx, State) ->
    outcome(with_module(ModBin, fun describe_component/1), State);
handle_tool(~"get_docs", #{~"module" := ModBin} = Args, _Ctx, State) ->
    outcome(with_module(ModBin, fun(Mod) -> get_docs(Mod, Args) end), State);
handle_tool(~"get_source_location", #{~"module" := ModBin} = Args, _Ctx, State) ->
    outcome(with_module(ModBin, fun(Mod) -> get_source_location(Mod, Args) end), State);
handle_tool(~"reloader_status", _Args, _Ctx, State) ->
    {reply, reloader_status(), State};
handle_tool(~"app_info", _Args, _Ctx, State) ->
    {reply, app_info(), State};
handle_tool(~"render_component", #{~"module" := ModBin} = Args, _Ctx, State) ->
    Bindings = maps:get(~"bindings", Args, #{}),
    outcome(with_module(ModBin, fun(Mod) -> render_component(Mod, Bindings) end), State);
handle_tool(~"eval", #{~"code" := Code}, _Ctx, #{bindings := Bindings} = State) ->
    case eval_code(Code, Bindings) of
        {ok, Value, NewBindings} -> {reply, format_value(Value), State#{bindings := NewBindings}};
        {error, Message} -> {error, Message, State}
    end.

%% --------------------------------------------------------------------
%% Internal functions -- tool implementations
%% --------------------------------------------------------------------

list_routes() ->
    case application:get_env(arizona, server) of
        {ok, ServerOpts} ->
            case maps:get(routes, ServerOpts, []) of
                [] -> ~"(no routes configured)";
                Routes -> iolist_to_binary(lists:join($\n, [format_route(R) || R <- Routes]))
            end;
        undefined ->
            ~"(no routes configured)"
    end.

%% Route shapes: 4-tuple `{live|controller|mcp, Path, Handler, Opts}` and 3-tuple
%% `{ws|asset|reload, Path, _}`; a catch-all keeps an unknown shape from crashing.
format_route({Type, Path, Handler, _Opts}) when is_atom(Handler) ->
    fmt("~p ~ts -> ~p", [Type, Path, Handler]);
format_route({Type, Path, _}) ->
    fmt("~p ~ts", [Type, Path]);
format_route(Other) ->
    fmt("~p", [Other]).

describe_component(Module) ->
    Exports = lists:join(~", ", [fmt("~p/~p", [F, A]) || {F, A} <- module_exports(Module)]),
    Text = fmt("kind: ~p~nexports: ~ts~n~nmoduledoc:~n~ts", [
        module_kind(Module), Exports, module_docs(Module)
    ]),
    {ok, Text}.

get_docs(Module, #{~"function" := FnBin}) ->
    case to_existing_atom(FnBin) of
        {ok, Fn} -> {ok, function_docs(Module, Fn)};
        error -> {ok, ~"(no documentation for that function)"}
    end;
get_docs(Module, _Args) ->
    {ok, module_docs(Module)}.

get_source_location(Module, #{~"function" := FnBin}) ->
    case to_existing_atom(FnBin) of
        {ok, Fn} -> {ok, function_location(Module, Fn)};
        error -> {ok, fmt("~ts (no such function ~ts)", [source_file(Module), FnBin])}
    end;
get_source_location(Module, _Args) ->
    {ok, source_file(Module)}.

reloader_status() ->
    case arizona_reloader:get_error() of
        undefined -> ~"ok (no compile error)";
        Error -> fmt("compile error: ~tp", [Error])
    end.

app_info() ->
    Vsn =
        case application:get_key(arizona, vsn) of
            {ok, V} -> V;
            undefined -> "unknown"
        end,
    fmt("arizona ~s, OTP ~s, node ~p, ~p processes", [
        Vsn, erlang:system_info(otp_release), node(), erlang:system_info(process_count)
    ]).

%% Render a stateful handler (mount + render) or a stateless template module,
%% guarded so a bad module/binding answers in-band rather than crashing.
render_component(Module, BindingsJson) ->
    case module_kind(Module) of
        other ->
            {error, fmt("~p is not a renderable component (no mount/1 or render/1)", [Module])};
        Kind ->
            try
                {ok, iolist_to_binary(render_html(Kind, Module, to_atom_bindings(BindingsJson)))}
            catch
                Class:Reason -> {error, fmt("render failed: ~p: ~tp", [Class, Reason])}
            end
    end.

render_html(stateful, Module, Bindings) ->
    arizona_render:render_to_iolist(Module, #{bindings => Bindings});
render_html(stateless, Module, Bindings) ->
    arizona_render:render_to_iolist(Module:render(Bindings)).

%% --------------------------------------------------------------------
%% Internal functions -- helpers
%% --------------------------------------------------------------------

module_arg_schema() ->
    #{type => ~"object", properties => #{module => #{type => ~"string"}}, required => [~"module"]}.

module_function_arg_schema() ->
    #{
        type => ~"object",
        properties => #{module => #{type => ~"string"}, function => #{type => ~"string"}},
        required => [~"module"]
    }.

%% Resolve a binary module name to a loaded module, then run `Fun`. A name that
%% isn't an already-known module answers in-band rather than crashing.
with_module(ModBin, Fun) ->
    case to_existing_atom(ModBin) of
        {ok, Module} ->
            case code:ensure_loaded(Module) of
                {module, Module} -> Fun(Module);
                {error, Reason} -> {error, fmt("module not loaded: ~ts (~p)", [ModBin, Reason])}
            end;
        error ->
            {error, fmt("unknown module: ~ts", [ModBin])}
    end.

%% Resolve a binary to an already-known atom. Returns `error` for an unknown name
%% rather than minting an atom from agent input (atom-table safety).
to_existing_atom(Bin) ->
    try
        {ok, binary_to_existing_atom(Bin, utf8)}
    catch
        error:badarg -> error
    end.

outcome({ok, Reply}, State) ->
    {reply, Reply, State};
outcome({error, Message}, State) ->
    {error, Message, State}.

%% `stateful` if it carries the behaviour or exports `mount/1`; `stateless` if it
%% exports `render/1` but no `mount/1`; otherwise `other` (not a component).
module_kind(Module) ->
    Exports = Module:module_info(exports),
    case
        lists:member(arizona_stateful, behaviours(Module)) orelse lists:member({mount, 1}, Exports)
    of
        true ->
            stateful;
        false ->
            case lists:member({render, 1}, Exports) of
                true -> stateless;
                false -> other
            end
    end.

%% The `behaviour` attribute reads as a list-of-lists, so flatten it.
behaviours(Module) ->
    lists:append(proplists:get_all_values(behaviour, Module:module_info(attributes))).

module_exports(Module) ->
    [{F, A} || {F, A} <- Module:module_info(exports), F =/= module_info].

source_file(Module) ->
    case proplists:get_value(source, Module:module_info(compile)) of
        undefined -> ~"(source unknown)";
        Source -> list_to_binary(Source)
    end.

function_location(Module, Fn) ->
    Source = source_file(Module),
    case function_lines(Module, Fn) of
        [] -> fmt("~ts (no line info for ~p)", [Source, Fn]);
        Lines -> iolist_to_binary([Source, $\n, lists:join($\n, Lines)])
    end.

function_lines(Module, Fn) ->
    case code:get_doc(Module) of
        {ok, {docs_v1, _, _, _, _, _, Docs}} ->
            [
                fmt("  ~p/~p at line ~p", [F, A, doc_line(Anno)])
             || {{function, F, A}, Anno, _, _, _} <- Docs, F =:= Fn
            ];
        {error, _} ->
            []
    end.

%% Doc-chunk annotations are `{Line, Col}` tuples in this build (older/other
%% producers use a `#{line => _}` map or a bare integer); normalize them all.
doc_line(#{line := Line}) -> Line;
doc_line({Line, _Col}) -> Line;
doc_line(Line) when is_integer(Line) -> Line;
doc_line(_) -> undefined.

module_docs(Module) ->
    %% docs_v1 = {docs_v1, Anno, Lang, Format, ModuleDoc, Meta, Docs}.
    case code:get_doc(Module) of
        {ok, {docs_v1, _, _, _, ModuleDoc, _, _}} -> doc_text(ModuleDoc);
        {error, Reason} -> fmt("no docs: ~tp", [Reason])
    end.

function_docs(Module, Fn) ->
    case code:get_doc(Module) of
        {ok, {docs_v1, _, _, _, _, _, Docs}} ->
            case [doc_text(Doc) || {{function, F, _A}, _, _, Doc, _} <- Docs, F =:= Fn] of
                [] -> ~"(no documentation for that function)";
                Texts -> iolist_to_binary(lists:join(~"\n\n", Texts))
            end;
        {error, Reason} ->
            fmt("no docs: ~tp", [Reason])
    end.

doc_text(#{<<"en">> := Doc}) -> Doc;
doc_text(_) -> ~"(no documentation)".

%% Convert a JSON bindings object (binary keys) to the atom keys an Arizona
%% handler reads. `binary_to_existing_atom` keeps an agent from leaking atoms; an
%% unseen key surfaces as an in-band error via the caller's try/catch.
to_atom_bindings(Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            try
                Acc#{binary_to_existing_atom(Key, utf8) => Value}
            catch
                error:badarg -> error({unknown_binding_key, Key})
            end
        end,
        #{},
        Map
    ).

%% Scan/parse/eval the code against the session's persistent bindings. Any
%% failure (parse or runtime) comes back as an in-band error, never a crash.
eval_code(Code, Bindings) ->
    try
        {ok, Tokens, _} = erl_scan:string(ensure_dot(binary_to_list(Code))),
        {ok, Exprs} = erl_parse:parse_exprs(Tokens),
        {value, Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
        {ok, Value, NewBindings}
    catch
        Class:Reason -> {error, fmt("~p: ~tp", [Class, Reason])}
    end.

ensure_dot(Str) ->
    case lists:reverse(string:trim(Str)) of
        [$. | _] -> Str;
        _ -> Str ++ "."
    end.

format_value(Value) ->
    fmt("~tp", [Value]).

fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).
