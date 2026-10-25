-module(arizona_route).
-moduledoc """
The kinds of route an Arizona route list holds, independent of any server.

An `arizona_server` adapter compiles each route into its server's own routing.
`kind/1` names what a route is, so an adapter dispatches on one atom instead of
re-matching every tuple shape, and refuses a route it cannot serve with
`unsupported/2` -- at route compilation, before a request could reach it, the
way an adapter refuses a controller reply kind it cannot send
(`arizona_controller:unsupported_reply/2`).

| Kind | Route |
|------|-------|
| `live` | `{live, Path, Handler, Opts}` |
| `ws` | `{ws, Path, Opts}` |
| `asset` | `{asset, Path, Source}`, `{asset, Path, Source, Opts}` |
| `controller` | `{Verb, Path, Handler, Opts}`, `{match, Spec, Path, Handler, Opts}` |
| `mcp` | `{mcp, Path, Handler, Opts}` |
| `reload` | `{reload, Path, Opts}` |

`live`, `ws`, `asset`, and `controller` are what an application is built from,
and every adapter serves them. `mcp` (the Model Context Protocol endpoint) and
`reload` (the dev hot-reload event stream) are capabilities.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([kind/1]).
-export([unsupported/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% For adapters whose server lacks a route kind; roadrunner serves them all.
-ignore_xref([unsupported/2]).
%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([kind/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal kind() :: live | ws | asset | controller | mcp | reload.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
The kind of `Route`. Raises `{unknown_route, Route}` for a term that is no
route: a route list is plain data a `sys.config` can spell by hand, so a typo
lands here rather than at a constructor.
""".
-spec kind(Route) -> kind() when
    Route :: term().
kind({live, Path, _Handler, _Opts}) when is_binary(Path) ->
    live;
kind({ws, Path, _Opts}) when is_binary(Path) ->
    ws;
kind({asset, Path, _Source}) when is_binary(Path) ->
    asset;
kind({asset, Path, _Source, _Opts}) when is_binary(Path) ->
    asset;
kind({match, _Spec, Path, _Handler, _Opts}) when is_binary(Path) ->
    controller;
kind({mcp, Path, _Handler, _Opts}) when is_binary(Path) ->
    mcp;
kind({reload, Path, _Opts}) when is_binary(Path) ->
    reload;
kind({Verb, Path, _Handler, _Opts} = Route) when is_atom(Verb), is_binary(Path) ->
    case lists:member(Verb, [get, post, put, patch, delete, head, options]) of
        true -> controller;
        false -> unknown_route(Route)
    end;
kind(Route) ->
    unknown_route(Route).

-doc """
Raises `{unsupported_route, Kind, Adapter}`: what an adapter does, while
compiling its routes, with a route kind its server cannot serve -- so the
listener refuses to start instead of answering that path with something else.
""".
-spec unsupported(Kind, Adapter) -> no_return() when
    Kind :: kind(),
    Adapter :: module().
unsupported(Kind, Adapter) ->
    erlang:error({unsupported_route, Kind, Adapter}, [Kind, Adapter], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Renders this module's errors. Picked up by `erl_error` via the `error_info`
annotation.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({unknown_route, Route}, [{_M, _F, _Args, _Info} | _]) ->
    #{
        general => io_lib:format(
            "~0tp is not a route. A route is {live, Path, Handler, Opts}, "
            "{ws, Path, Opts}, {asset, Path, Source[, Opts]}, "
            "{Verb, Path, Handler, Opts} (get, post, put, patch, delete, head, "
            "options), {match, Spec, Path, Handler, Opts}, {mcp, Path, Handler, "
            "Opts}, or {reload, Path, Opts}, with Path a binary.",
            [Route]
        )
    };
format_error({unsupported_route, Kind, Adapter}, _ST) ->
    #{
        general => io_lib:format(
            "the ~s server adapter cannot serve a ~s route. Drop the route, or "
            "serve it through an adapter that has it.",
            [Adapter, Kind]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

unknown_route(Route) ->
    erlang:error({unknown_route, Route}, [Route], [{error_info, #{module => ?MODULE}}]).
