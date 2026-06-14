-module(arizona_demo_mcp).
-moduledoc """
A worked example MCP server: a dev-time introspection tool for an Arizona app,
in the spirit of Tidewave. A coding agent connects to it (mounted at `/mcp` in
`arizona_test_server`) and can ask the running app about itself, then run code
against it.

Tools:

- `list_routes` -- the routes the dev server serves.
- `get_docs` -- a module's (or function's) documentation, read from its EEP-48
  doc chunk.
- `app_info` -- Arizona version, OTP release, node, process count.
- `reloader_status` -- the dev reloader's current compile error, if any.
- `eval` -- **run Erlang in the live node**, with bindings that persist across
  calls (the session holds them). This is the powerful, Tidewave-style tool, and
  it is a remote-code-execution surface: only ever mount it on a trusted,
  dev-only, localhost route behind the `Origin` allowlist. Never expose it.

It runs in **session mode** so `eval`'s bindings carry across calls (a REPL the
agent drives) and so a slow eval runs in the session's worker without blocking
the session. Tools-only: `init/1`, `tools/1`, `handle_tool/4` are the only
callbacks an MCP server must implement.
""".
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/4]).

init(_InitParams) ->
    {ok, #{name => ~"arizona_dev_tools", version => ~"0.1.0"}, #{tools => #{}}, #{
        bindings => erl_eval:new_bindings()
    }}.

tools(_State) ->
    [
        #{
            name => ~"list_routes",
            description => ~"List the routes the dev server serves",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"get_docs",
            description => ~"Documentation for a module, or a function within it",
            input_schema => #{
                type => ~"object",
                properties => #{
                    module => #{type => ~"string"},
                    function => #{type => ~"string"}
                },
                required => [~"module"]
            }
        },
        #{
            name => ~"app_info",
            description => ~"Arizona version, OTP release, node, and process count",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"reloader_status",
            description => ~"The dev reloader's current compile error, if any",
            input_schema => #{type => ~"object", properties => #{}}
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
    Lines = [format_route(Route) || Route <- arizona_test_server:routes()],
    {reply, iolist_to_binary(lists:join($\n, Lines)), State};
handle_tool(~"get_docs", #{~"module" := ModBin} = Args, _Ctx, State) ->
    Module = binary_to_atom(ModBin),
    Docs =
        case Args of
            #{~"function" := FnBin} -> function_docs(Module, binary_to_atom(FnBin));
            _ -> module_docs(Module)
        end,
    {reply, Docs, State};
handle_tool(~"app_info", _Args, _Ctx, State) ->
    {reply, app_info(), State};
handle_tool(~"reloader_status", _Args, _Ctx, State) ->
    Status =
        case arizona_reloader:get_error() of
            undefined -> ~"ok (no compile error)";
            Error -> iolist_to_binary(io_lib:format("compile error: ~tp", [Error]))
        end,
    {reply, Status, State};
handle_tool(~"eval", #{~"code" := Code}, _Ctx, #{bindings := Bindings} = State) ->
    case eval_code(Code, Bindings) of
        {ok, Value, NewBindings} ->
            {reply, format_value(Value), State#{bindings := NewBindings}};
        {error, Message} ->
            {error, Message, State}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% A route is `{Type, Path, Handler, _Opts}` (live/controller/mcp) or
%% `{Type, Path, _}` (ws/asset, no handler module).
format_route({Type, Path, Handler, _Opts}) when is_atom(Handler) ->
    io_lib:format("~p ~ts -> ~p", [Type, Path, Handler]);
format_route({Type, Path, _}) ->
    io_lib:format("~p ~ts", [Type, Path]).

module_docs(Module) ->
    case code:get_doc(Module) of
        {ok, {docs_v1, _, _, _, ModuleDoc, _, _}} ->
            doc_text(ModuleDoc);
        {error, Reason} ->
            iolist_to_binary(io_lib:format("no docs: ~tp", [Reason]))
    end.

function_docs(Module, Fn) ->
    case code:get_doc(Module) of
        {ok, {docs_v1, _, _, _, _, _, Docs}} ->
            case [doc_text(Doc) || {{function, F, _A}, _, _, Doc, _} <- Docs, F =:= Fn] of
                [] -> ~"(no documentation for that function)";
                Texts -> iolist_to_binary(lists:join(~"\n\n", Texts))
            end;
        {error, Reason} ->
            iolist_to_binary(io_lib:format("no docs: ~tp", [Reason]))
    end.

doc_text(#{<<"en">> := Doc}) -> Doc;
doc_text(_) -> ~"(no documentation)".

app_info() ->
    Vsn =
        case application:get_key(arizona, vsn) of
            {ok, V} -> V;
            undefined -> "unknown"
        end,
    iolist_to_binary(
        io_lib:format("arizona ~s, OTP ~s, node ~p, ~p processes", [
            Vsn,
            erlang:system_info(otp_release),
            node(),
            erlang:system_info(process_count)
        ])
    ).

%% Scan/parse/eval the code against the session's persistent bindings. Any
%% failure (parse or runtime) comes back as an in-band tool error, never a crash.
eval_code(Code, Bindings) ->
    try
        {ok, Tokens, _} = erl_scan:string(ensure_dot(binary_to_list(Code))),
        {ok, Exprs} = erl_parse:parse_exprs(Tokens),
        {value, Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
        {ok, Value, NewBindings}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p: ~tp", [Class, Reason]))}
    end.

ensure_dot(Str) ->
    case lists:reverse(string:trim(Str)) of
        [$. | _] -> Str;
        _ -> Str ++ "."
    end.

format_value(Value) ->
    iolist_to_binary(io_lib:format("~tp", [Value])).
