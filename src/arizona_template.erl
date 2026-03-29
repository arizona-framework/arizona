-module(arizona_template).
-export([
    get/2, get/3,
    get_lazy/3,
    track/1,
    html/1,
    stateful/2,
    stateless/2, stateless/3,
    each/2,
    to_bin/1,
    dyn_az/1,
    format_error/1,
    unwrap_val/1,
    maybe_propagate/2,
    maybe_put_fingerprint/2,
    make_child_snap/4,
    call_handle_update/3,
    unzip_triples/1,
    split_triples/1,
    visible_keys/2
]).
-ignore_xref([format_error/1]).
-include("arizona.hrl").
-compile({nowarn_redefined_builtin_type, [{dynamic, 0}]}).

%% --- Types ------------------------------------------------------------------

-type az() :: binary() | undefined.
-type deps() :: #{term() => true}.

-type loc() :: {module(), pos_integer()}.
-type dynamic() ::
    {az(), fun(() -> term()), loc()}
    | {az(), {attr, binary(), fun(() -> term())}, loc()}
    | {az(), template(), loc()}
    | {az(), term(), loc()}
    | {az(), fun(() -> term())}
    | {az(), {attr, binary(), fun(() -> term())}}
    | {az(), template()}
    | {az(), term()}.

-type template() :: #{s := [binary()], d := [dynamic()], f := binary(), diff => false}.

-type snapshot() :: #{
    s := [binary()],
    d := [{az(), term()}],
    f => binary(),
    deps => [deps()],
    diff => false,
    view_id => binary()
}.

-type stateful_descriptor() :: #{stateful := module(), props := map()}.
-type stateless_descriptor() :: #{callback := fun((map()) -> template()), props := map()}.

-export_type([
    az/0,
    dynamic/0,
    template/0
]).

%% --- Binding access with dep tracking ----------------------------------------

get(Key, Bindings) ->
    track(Key),
    maps:get(Key, Bindings).
get(Key, Bindings, Default) ->
    track(Key),
    maps:get(Key, Bindings, Default).
get_lazy(Key, Bindings, DefaultFun) when is_function(DefaultFun, 0) ->
    track(Key),
    case Bindings of
        #{Key := Val} -> Val;
        #{} -> DefaultFun()
    end.

track(Key) ->
    case erlang:get('$arizona_deps') of
        undefined -> ok;
        Deps -> erlang:put('$arizona_deps', Deps#{Key => true})
    end.

%% --- Template construction (replaced by parse transform) ---------------------

-spec html(term()) -> no_return().
html(_Elems) ->
    erlang:error(parse_transform_not_applied).

%% --- Descriptor constructors -------------------------------------------------

-spec stateful(module(), map()) -> stateful_descriptor().
stateful(Handler, Props) when is_atom(Handler), is_map(Props) ->
    #{stateful => Handler, props => Props}.

-spec stateless(fun((map()) -> template()), map()) -> stateless_descriptor().
stateless(Callback, Props) when is_function(Callback, 1), is_map(Props) ->
    #{callback => Callback, props => Props}.
-spec stateless(module(), atom(), map()) -> stateless_descriptor().
stateless(Handler, Fun, Props) when is_atom(Handler), is_atom(Fun), is_map(Props) ->
    #{callback => fun Handler:Fun/1, props => Props}.

each(Source, #{t := ?EACH, d := DFun} = Tmpl) when is_function(DFun, 1); is_function(DFun, 2) ->
    #{t => ?EACH, source => Source, template => Tmpl}.

%% --- to_bin ------------------------------------------------------------------

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
to_bin(V) when is_atom(V) -> atom_to_binary(V);
to_bin({arizona_js, _} = Cmd) -> arizona_js:encode(Cmd);
to_bin([{arizona_js, _} | _] = Cmds) -> arizona_js:encode(Cmds);
to_bin(V) when is_list(V) -> iolist_to_binary(V);
to_bin(V) -> erlang:error({bad_template_value, V}).

%% --- dyn_az ------------------------------------------------------------------

dyn_az({Az, _}) -> Az;
dyn_az({Az, _, _}) -> Az.

%% --- format_error ------------------------------------------------------------

format_error(parse_transform_not_applied) ->
    "parse transform not applied -- include arizona_stateful.hrl or arizona_stateless.hrl";
format_error({bad_template_value, V}) ->
    lists:flatten(io_lib:format("cannot convert ~0tp to binary in template", [V])).

%% --- unwrap_val ---------------------------------------------------------------

unwrap_val({attr, _, V}) -> V;
unwrap_val(V) -> V.

%% --- maybe_propagate/2 --------------------------------------------------------
%% Propagate both `f` and `diff => false` from template to snapshot.

-spec maybe_propagate(template(), snapshot()) -> snapshot().
maybe_propagate(Tmpl, Snap) ->
    Snap1 =
        case Tmpl of
            #{diff := false} -> Snap#{diff => false};
            #{} -> Snap
        end,
    maybe_put_fingerprint(Tmpl, Snap1).

%% --- maybe_put_fingerprint/2 --------------------------------------------------
%% Copy `f` from template to snapshot if present.

maybe_put_fingerprint(#{f := F}, Snap) -> Snap#{f => F};
maybe_put_fingerprint(#{}, Snap) -> Snap.

%% --- make_child_snap/4 -------------------------------------------------------
%% Build a stateful child snapshot, propagating diff => false from the template.

-spec make_child_snap(template(), [{az(), term()}], [deps()], binary()) -> snapshot().
make_child_snap(Tmpl, ChildD, ChildDeps, Id) ->
    #{s := S} = Tmpl,
    Snap = #{s => S, d => ChildD, deps => ChildDeps, view_id => Id},
    Snap1 =
        case Tmpl of
            #{diff := false} -> Snap#{diff => false};
            #{} -> Snap
        end,
    maybe_put_fingerprint(Tmpl, Snap1).

%% --- call_handle_update/3 ----------------------------------------------------

call_handle_update(H, Props, Bindings) ->
    case erlang:function_exported(H, handle_update, 2) of
        true -> H:handle_update(Props, Bindings);
        false -> {maps:merge(Bindings, Props), #{}}
    end.

%% --- unzip_triples/1 ---------------------------------------------------------

unzip_triples([]) ->
    {[], [], []};
unzip_triples([{Az, Val, Deps} | Rest]) ->
    {RestD, RestDeps, RestVals} = unzip_triples(Rest),
    {[{Az, Val} | RestD], [Deps | RestDeps], [unwrap_val(Val) | RestVals]}.

%% --- split_triples/1 ---------------------------------------------------------

split_triples([]) ->
    {[], []};
split_triples([{Az, Val, Deps} | Rest]) ->
    {RestD, RestDeps} = split_triples(Rest),
    {[{Az, Val} | RestD], [Deps | RestDeps]}.

%% --- visible_keys/2 -----------------------------------------------------------

visible_keys(Order, infinity) -> Order;
visible_keys(Order, Limit) -> lists:sublist(Order, Limit).
