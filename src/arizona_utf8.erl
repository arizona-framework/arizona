-module(arizona_utf8).
-moduledoc """
The UTF-8 output contract: naming a rendered value that is not UTF-8.

Every render target writes UTF-8 -- an HTML response is served as
`text/html; charset=utf-8`, and a WebSocket frame and a `?native` document are
JSON. A rendered binary must therefore hold UTF-8. Lists need nothing from this
module: `arizona_template:to_bin/1` decodes them as chardata and refuses one that
does not decode. A binary is emitted as the bytes it holds, and checking every
one of them on the render path was measured too expensive (see
`docs/performance.md`), so the contract is enforced where it costs nothing:

- **Compile time** -- `arizona_parse_transform` warns about a non-ASCII
  `<<"...">>` literal that is not folded into a template static, the usual
  source of Latin-1 bytes.
- **Failure paths** -- a JSON encoder that refuses a frame or a `?native` value
  already proved the bytes bad; `refuse/3` renames its `{invalid_byte, _}` /
  `unexpected_end` to `{non_utf8_render, Byte, Offset}`, naming the value.
- **Dev mode** -- when the dev reloader is wired, `arizona_http` checks each
  HTML response once (`check_page/3`) and `arizona_socket` locates a refused
  frame (`locate_live/3`). Both find the template line by rendering the view
  again with every value checked (an internal `wrap/2`), a walk only a failure pays for.
""".

-include("arizona.hrl").

%% Raised in the caller's own frame, whose argument is the offending value, so
%% format_error/2 can show it.
-define(RAISE(Byte, Offset, Bin),
    erlang:error({non_utf8_render, Byte, Offset}, [Bin], [{error_info, #{module => ?MODULE}}])
).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([check/1]).
-export([refuse/3]).
-export([check_page/3]).
-export([locate_live/3]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).
%% Public: an app can assert its own data before rendering it.
-ignore_xref([check/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Returns `Bin` when it is UTF-8, else raises `{non_utf8_render, Byte, Offset}`
naming the first byte that does not decode and its offset in `Bin`.
""".
-spec check(Bin) -> Bin when
    Bin :: binary().
check(Bin) ->
    case first_invalid(Bin) of
        none -> Bin;
        {Byte, Offset} -> ?RAISE(Byte, Offset, Bin)
    end.

-doc """
Re-raises a JSON encoder failure on `Term` as `{non_utf8_render, Byte, Offset}`
when `Reason` is the encoder's invalid-UTF-8 error and `Term` holds a binary that
is not UTF-8 (the offset is within that binary). Any other failure is re-raised
unchanged. Called only after the encoder raised, so it costs nothing otherwise.
""".
-spec refuse(Term, Reason, Stacktrace) -> no_return() when
    Term :: term(),
    Reason :: term(),
    Stacktrace :: erlang:stacktrace().
refuse(Term, Reason, ST) ->
    case encoder_utf8_error(Reason) of
        true -> refuse_found(find(Term), Reason, ST);
        false -> erlang:raise(error, Reason, ST)
    end.

refuse_found({Bin, Byte, Offset}, _Reason, _ST) ->
    ?RAISE(Byte, Offset, Bin);
refuse_found(none, Reason, ST) ->
    erlang:raise(error, Reason, ST).

-doc """
The dev-mode response check. Returns `ok` when `Page` is UTF-8; otherwise renders
`Handler` again with every value checked, raising
`{arizona_loc, {Module, Line}, {non_utf8_render, Byte, Offset}}` at the template
line that produced it -- or, when that render does not reproduce the bytes,
`{non_utf8_render, Byte, Offset}` with the offset in the page.

`Opts` are the `t:arizona_live:route_opts/0` the page rendered with. The check
renders the page once more, so the view's `mount/1` (and its children's) runs
again; that happens only for a page that already failed.
""".
-spec check_page(Page, Handler, Opts) -> ok when
    Page :: iodata(),
    Handler :: module(),
    Opts :: arizona_live:route_opts().
check_page(Page, Handler, Opts) ->
    Bin = iolist_to_binary(Page),
    case first_invalid(Bin) of
        none ->
            ok;
        {Byte, Offset} ->
            ok = locate_view(Handler, Opts),
            From = max(0, Offset - 16),
            ?RAISE(Byte, Offset, binary_part(Bin, From, min(32, byte_size(Bin) - From)))
    end.

-doc """
Locates a `non_utf8_render` raised for a WebSocket frame. The frame left the
live process's values behind, so it names the bytes but not the template line.
In dev mode the views `Pid` holds are rendered again from their current bindings
with every value checked; the first one that holds the bytes returns
`{{arizona_loc, {Module, Line}, Reason}, Stacktrace}`. Outside dev mode, or when
nothing is found, the input pair comes back unchanged.
""".
-spec locate_live(Reason, Stacktrace, Pid) -> {Reason1, Stacktrace1} when
    Reason :: term(),
    Stacktrace :: erlang:stacktrace(),
    Pid :: pid() | undefined,
    Reason1 :: term(),
    Stacktrace1 :: erlang:stacktrace().
locate_live(Reason, ST, Pid) when is_pid(Pid) ->
    case persistent_term:get(arizona_reload_url, undefined) of
        undefined -> {Reason, ST};
        _Dev -> locate_views(view_state(Pid), Reason, ST)
    end;
locate_live(Reason, ST, undefined) ->
    {Reason, ST}.

-doc """
Formats `non_utf8_render`, bare or wrapped in `arizona_loc`. Picked up by
`erl_error:format_exception/3` through the `error_info` annotation, so the dev
error page and the crash log name the byte and the fix.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({arizona_loc, {Mod, Line}, Reason}, ST) ->
    #{general := Msg} = format_error(Reason, ST),
    #{general => io_lib:format("~s:~b: ~s", [Mod, Line, Msg])};
format_error({non_utf8_render, Byte, Offset}, ST) ->
    #{
        general => io_lib:format(
            "rendered value is not valid UTF-8: byte 16#~2.16.0B at offset ~b~s. "
            "A bare <<\"...\">> literal holding non-ASCII characters is Latin-1, "
            "not UTF-8: write ~~\"...\" or <<\"...\"/utf8>> instead, or convert "
            "the data with unicode:characters_to_binary(Value, latin1).",
            [Byte, Offset, preview(ST)]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% The raising call's argument is the offending value. Its bytes are shown as
%% integers: `~p` prints a Latin-1 binary as the very text it fails to be.
preview([{_M, _F, [Bin], _Info} | _]) when is_binary(Bin), byte_size(Bin) =< 32 ->
    io_lib:format(" in ~w", [Bin]);
preview([{_M, _F, [Bin], _Info} | _]) when is_binary(Bin) ->
    io_lib:format(" in ~w...", [binary_part(Bin, 0, 32)]);
preview(_ST) ->
    "".

%% A BIF scan that returns its input uncopied when it is valid.
first_invalid(Bin) ->
    case unicode:characters_to_binary(Bin) of
        Valid when is_binary(Valid) -> none;
        {_Status, Prefix, <<Byte, _/binary>>} -> {Byte, byte_size(Prefix)}
    end.

%% What `json:encode/1` raises for a string that is not UTF-8.
encoder_utf8_error({invalid_byte, _}) -> true;
encoder_utf8_error(unexpected_end) -> true;
encoder_utf8_error(_Reason) -> false.

%% A binary in `Term` that is not UTF-8, depth first (map entries in iteration order).
find(Bin) when is_binary(Bin) ->
    case first_invalid(Bin) of
        none -> none;
        {Byte, Offset} -> {Bin, Byte, Offset}
    end;
find([H | T]) ->
    case find(H) of
        none -> find(T);
        Found -> Found
    end;
find(Map) when is_map(Map) ->
    find([[K, V] || K := V <- Map]);
find(Tuple) when is_tuple(Tuple) ->
    find(tuple_to_list(Tuple));
find(_Other) ->
    none.

%% Renders the page as arizona_render:render_view/2 does, every value checked. A
%% located `non_utf8_render` propagates; anything else this render raises is not
%% the failure being located, so it is dropped and the caller reports the page.
locate_view(Handler, Opts) ->
    try
        Bindings0 = arizona_live:apply_on_mount(
            maps:get(on_mount, Opts, []), maps:get(bindings, Opts, #{})
        ),
        {Bindings, _Resets} = arizona_stateful:call_mount(Handler, Bindings0),
        Page = arizona_stateful:call_render(Handler, Bindings),
        Inner = Bindings#{inner_content => #{s => [<<>>], d => []}},
        Layouts = [Mod:Fun(Inner) || {Mod, Fun} <- maps:get(layouts, Opts, [])],
        [arizona_render:render_to_iolist(wrap(T, mount)) || T <- [Page | Layouts]]
    of
        _Rendered -> ok
    catch
        error:{arizona_loc, _, {non_utf8_render, _, _}} = Located:LocST ->
            erlang:raise(error, Located, LocST);
        _:_ ->
            ok
    end.

view_state(Pid) ->
    try
        arizona_live:view_state(Pid)
    catch
        exit:_ -> undefined
    end.

locate_views(undefined, Reason, ST) ->
    {Reason, ST};
locate_views(#{handler := H, bindings := B, views := Views}, Reason, ST) ->
    first_located([{H, B} | [{CH, CB} || _Id := #{handler := CH, bindings := CB} <- Views]], {
        Reason, ST
    }).

first_located([], Default) ->
    Default;
first_located([{H, B} | Rest], Default) ->
    try arizona_render:render_to_iolist(wrap(arizona_stateful:call_render(H, B), skip_stateful)) of
        _Clean -> first_located(Rest, Default)
    catch
        error:{arizona_loc, _, {non_utf8_render, _, _}} = Located:LocST ->
            {Located, LocST};
        _:_ ->
            first_located(Rest, Default)
    end.

%% Wraps a template so every value it evaluates is checked with `check/1` inside the
%% slot's located evaluation -- nested templates, `?each` items, `?stateless`
%% children and `?local` inits alike. A `?stateful` child is mounted and wrapped
%% (`mount`) or skipped (`skip_stateful`, for a live view tree whose children are
%% checked from their own bindings).
-spec wrap(Template, Mode) -> Template when
    Template :: arizona_template:template(),
    Mode :: mount | skip_stateful.
wrap(#{d := Dynamics} = Tmpl, Mode) ->
    Tmpl#{d := [wrap_dyn(D, Mode) || D <- Dynamics]}.

wrap_dyn({Az, Spec, Loc}, Mode) -> {Az, wrap_spec(Spec, Mode), Loc};
wrap_dyn({Az, Spec}, Mode) -> {Az, wrap_spec(Spec, Mode)}.

%% Every check runs inside the closure, so it raises inside the slot's located
%% `try` and comes back tagged with the template line.
wrap_spec({esc, Fun}, Mode) when is_function(Fun, 0) ->
    {esc, fun() -> wrap_val(Fun(), Mode) end};
wrap_spec({attr, Name, Fun}, Mode) when is_function(Fun, 0) ->
    {attr, Name, fun() -> wrap_val(Fun(), Mode) end};
wrap_spec(Fun, Mode) when is_function(Fun, 0) ->
    fun() -> wrap_val(Fun(), Mode) end;
wrap_spec(Value, Mode) ->
    fun() -> wrap_val(Value, Mode) end.

wrap_val(Bin, _Mode) when is_binary(Bin) ->
    check(Bin);
wrap_val({arizona_raw, V}, Mode) ->
    {arizona_raw, wrap_val(V, Mode)};
wrap_val(#{t := ?EACH, template := #{d := DFun} = ItemTmpl} = Each, Mode) when
    is_function(DFun, 1)
->
    Each#{template := ItemTmpl#{d := fun(I) -> [wrap_dyn(D, Mode) || D <- DFun(I)] end}};
wrap_val(#{t := ?EACH, template := #{d := DFun} = ItemTmpl} = Each, Mode) when
    is_function(DFun, 2)
->
    Each#{template := ItemTmpl#{d := fun(I, K) -> [wrap_dyn(D, Mode) || D <- DFun(I, K)] end}};
wrap_val(#{stateful := _, props := _}, skip_stateful) ->
    <<>>;
wrap_val(#{stateful := H, props := Props}, mount) ->
    #{callback => fun(P) -> mount_and_wrap(H, P) end, props => Props};
wrap_val(#{callback := Callback} = Desc, Mode) ->
    Desc#{callback := fun(P) -> wrap_val(Callback(P), Mode) end};
wrap_val(#{az_local := _, v := V} = Local, Mode) ->
    Local#{v := wrap_val(V, Mode)};
wrap_val(#{s := _, d := _} = Tmpl, Mode) ->
    wrap(Tmpl, Mode);
wrap_val(Value, _Mode) ->
    Value.

mount_and_wrap(H, Props) ->
    {Bindings, _Resets} = arizona_stateful:call_mount(H, Props),
    wrap(arizona_stateful:call_render(H, Bindings), mount).
