-module(arizona_cowboy_router).
-moduledoc """
Route matching for the Cowboy adapter.

Cowboy's own router cannot express Arizona's routing contract: it has no method
dimension, so it can neither dispatch one path to several routes by verb nor
build a `405`'s `Allow` set, and it answers a miss itself. So the adapter mounts
a single catch-all route on Cowboy and matches here, from the rules in
`arizona_route`.

`compile/2` turns an Arizona route list into an ordered table (refusing what
the contract refuses: a route the earlier ones fully shadow, a `mcp` or
`reload` route this adapter does not serve). `match/3` follows the contract
in `docs/adapters.md`: declaration order, first match wins; `:name` captures one
segment and `*name` the rest, percent-decoded; empty segments ignored; a path
whose matching routes all refuse the method answers `method_not_allowed` with
the union of their methods.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile/2]).
-export([match/3]).
-export([allow/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([table/0]).
-export_type([entry/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal segment() :: {literal, binary()} | {param, binary()} | {wildcard, binary()}.

-nominal entry() :: #{
    route := term(),
    kind := arizona_route:kind(),
    pattern := [segment()],
    methods := arizona_route:methods(),
    %% `OPTIONS` was added only so a CORS transform sees the preflight; a plain
    %% `OPTIONS` is then answered by the adapter, never the route.
    cors_options := boolean(),
    transforms := [arizona_middleware:http_transform()],
    build_opts := arizona_route:build_opts()
}.

-nominal table() :: [entry()].

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Compiles `Routes` into a match table. Raises `arizona_route:unsupported/2` for a
`mcp` or `reload` route, `{invalid_route_path, Path, wildcard_not_last}` for a
`*name` followed by more segments, and `{unreachable_route, Pos, Path, By}` for a
route the earlier ones fully shadow.
""".
-spec compile(Routes, BuildOpts) -> table() when
    Routes :: [term()],
    BuildOpts :: arizona_route:build_opts().
compile(Routes, BuildOpts) ->
    Table = [entry(Route, BuildOpts) || Route <- Routes],
    ok = check_shadowing(lists:zip(lists:seq(1, length(Table)), Table)),
    Table.

-doc """
Matches a request. `{ok, Entry, Bindings}` for the first route matching both
path and method (bindings percent-decoded), `{method_not_allowed, Allow}` when
routes match the path but none allows the method, `not_found` otherwise.
""".
-spec match(Method, Path, Table) ->
    {ok, entry(), #{binary() => binary() | [binary()]}}
    | {method_not_allowed, [binary()]}
    | not_found
when
    Method :: binary(),
    Path :: binary(),
    Table :: table().
match(Method, Path, Table) ->
    match(Method, segments(Path), Table, none).

-doc """
The `Allow` set a `405` on `Path` would carry, or `[]` when a route matching
it answers every method (no set can enumerate that).
""".
-spec allow(Path, Table) -> [binary()] when
    Path :: binary(),
    Table :: table().
allow(Path, Table) ->
    case match(~"ARIZONA-ALLOW-PROBE", Path, Table) of
        {method_not_allowed, Allow} -> Allow;
        _Answered -> []
    end.

-doc "Renders this module's route-compilation errors.".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({invalid_route_path, Path, wildcard_not_last}, [{_M, _F, _Args, _Info} | _]) ->
    #{
        general => io_lib:format(
            "route ~ts has a segment after its *wildcard, which consumes the rest "
            "of the path, so the route could never match.",
            [Path]
        )
    };
format_error({unreachable_route, Pos, Path, By}, _ST) ->
    #{
        general => io_lib:format(
            "route #~b (~ts) can never be reached: the routes before it (~tp) "
            "already answer every request it would. Routes match in declaration "
            "order; list the more specific one first.",
            [Pos, Path, By]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

entry(Route, BuildOpts) ->
    Kind = arizona_route:kind(Route),
    ok = check_supported(Kind),
    Path = route_path(Route),
    Pattern = pattern(Path),
    Transforms = arizona_route:transforms(Route, BuildOpts),
    Methods0 = arizona_route:methods(Route),
    {Methods, CorsOptions} = open_cors_options(Methods0, Transforms),
    #{
        route => Route,
        kind => Kind,
        pattern => Pattern,
        methods => Methods,
        cors_options => CorsOptions,
        transforms => Transforms,
        build_opts => BuildOpts
    }.

check_supported(Kind) when Kind =:= mcp; Kind =:= reload ->
    arizona_route:unsupported(Kind, arizona_cowboy_server);
check_supported(_Kind) ->
    ok.

%% An asset route serves everything under its prefix.
route_path({asset, Path, _Source}) -> <<Path/binary, "/*path">>;
route_path({asset, Path, _Source, _Opts}) -> <<Path/binary, "/*path">>;
route_path(Route) -> arizona_route:path(Route).

open_cors_options(any, _Transforms) ->
    {any, false};
open_cors_options(Methods, Transforms) ->
    HasCors = lists:any(fun({http_transform, T}) -> is_cors(T) end, Transforms),
    case HasCors andalso not lists:member(~"OPTIONS", Methods) of
        true -> {Methods ++ [~"OPTIONS"], true};
        false -> {Methods, false}
    end.

is_cors({cors, _Config}) -> true;
is_cors(_Transform) -> false.

pattern(Path) ->
    Pattern = [segment(S) || S <- segments(Path)],
    case lists:dropwhile(fun(S) -> element(1, S) =/= wildcard end, Pattern) of
        [] -> Pattern;
        [_Wildcard] -> Pattern;
        [_Wildcard | _Rest] -> invalid_path(Path)
    end.

invalid_path(Path) ->
    erlang:error({invalid_route_path, Path, wildcard_not_last}, [Path], [
        {error_info, #{module => ?MODULE}}
    ]).

segment(<<":", Name/binary>>) -> {param, Name};
segment(<<"*", Name/binary>>) -> {wildcard, Name};
segment(Literal) -> {literal, Literal}.

segments(Path) ->
    binary:split(Path, ~"/", [global, trim_all]).

match(_Method, _Segments, [], none) ->
    not_found;
match(_Method, _Segments, [], Allow) ->
    {method_not_allowed, lists:usort(Allow)};
match(Method, Segments, [#{pattern := Pattern, methods := Methods} = Entry | Rest], Allow) ->
    case match_pattern(Pattern, Segments, #{}) of
        nomatch ->
            match(Method, Segments, Rest, Allow);
        {ok, Bindings} ->
            case allows(Method, Methods) of
                true -> {ok, Entry, decode(Bindings)};
                false -> match(Method, Segments, Rest, add_allow(Methods, Allow))
            end
    end.

allows(_Method, any) -> true;
allows(Method, Methods) -> lists:member(Method, Methods).

add_allow(Methods, none) -> Methods;
add_allow(Methods, Allow) -> Methods ++ Allow.

match_pattern([], [], Bindings) ->
    {ok, Bindings};
match_pattern([{wildcard, Name}], Segments, Bindings) ->
    {ok, Bindings#{Name => Segments}};
match_pattern([{literal, S} | Pattern], [S | Segments], Bindings) ->
    match_pattern(Pattern, Segments, Bindings);
match_pattern([{param, Name} | Pattern], [S | Segments], Bindings) ->
    match_pattern(Pattern, Segments, Bindings#{Name => S});
match_pattern(_Pattern, _Segments, _Bindings) ->
    nomatch.

%% Captures are decoded once, on the winning route; a malformed escape is kept
%% raw rather than failing a match on client-controlled input.
decode(Bindings) ->
    #{Name => decode_value(Value) || Name := Value <- Bindings}.

decode_value(Segments) when is_list(Segments) -> [decode_segment(S) || S <- Segments];
decode_value(Segment) -> decode_segment(Segment).

decode_segment(Segment) ->
    case uri_string:percent_decode(Segment) of
        Decoded when is_binary(Decoded) -> Decoded;
        {error, _Reason, _Term} -> Segment
    end.

%% A route the earlier ones answer in full, on every path and method it could
%% match, is dead: refuse the table rather than 404 at runtime.
check_shadowing(Numbered) ->
    lists:foreach(
        fun({Pos, #{pattern := Pattern, methods := Methods, route := Route}}) ->
            case shadowed_by([E || {P, _} = E <- Numbered, P < Pos], Pattern, Methods) of
                reachable ->
                    ok;
                {shadowed, By} ->
                    erlang:error(
                        {unreachable_route, Pos, route_path(Route), By},
                        none,
                        [{error_info, #{module => ?MODULE}}]
                    )
            end
        end,
        Numbered
    ).

shadowed_by([], _Later, _Uncovered) ->
    reachable;
shadowed_by(
    [{Pos, #{pattern := Earlier, methods := EarlierMethods, route := Route}} | Rest],
    Later,
    Uncovered
) ->
    case covers(Earlier, Later) of
        false ->
            shadowed_by(Rest, Later, Uncovered);
        true ->
            case take_methods(EarlierMethods, Uncovered) of
                [] ->
                    {shadowed, [{Pos, route_path(Route)}]};
                Uncovered ->
                    shadowed_by(Rest, Later, Uncovered);
                Narrowed ->
                    case shadowed_by(Rest, Later, Narrowed) of
                        reachable -> reachable;
                        {shadowed, By} -> {shadowed, [{Pos, route_path(Route)} | By]}
                    end
            end
    end.

%% What the later route can still answer once an earlier, path-covering one has
%% taken its methods. An earlier route answering any method leaves nothing; a
%% later one answering any method is never exhausted by finite allowlists.
take_methods(any, _Uncovered) -> [];
take_methods(_Methods, any) -> any;
take_methods(Methods, Uncovered) -> Uncovered -- Methods.

%% Does `Earlier` match every path `Later` matches?
covers([{wildcard, _}], _Later) -> true;
covers([{literal, S} | Earlier], [{literal, S} | Later]) -> covers(Earlier, Later);
covers([{param, _} | Earlier], [{literal, _} | Later]) -> covers(Earlier, Later);
covers([{param, _} | Earlier], [{param, _} | Later]) -> covers(Earlier, Later);
covers([], []) -> true;
covers(_Earlier, _Later) -> false.
