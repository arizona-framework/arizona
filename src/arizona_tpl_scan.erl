-module(arizona_tpl_scan).
-moduledoc """
Template scanner.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([string/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec string(String) -> Tokens
    when String :: binary() | string(),
         Tokens :: [arizona_tpl_parse:token()].
string(Str) when is_binary(Str) ->
    scan(Str, Str, 0, 0);
string(Str) when is_list(Str) ->
    string(iolist_to_binary(Str)).

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

scan(<<$<, $/, Rest/binary>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        none ->
            [closing_tag | scan_tag_name(Rest, Str, Pos + Len + 2, 0)];
        TxtToken ->
            [TxtToken, closing_tag | scan_tag_name(Rest, Str, Pos + Len + 2, 0)]
    end;
scan(<<$<, Rest/binary>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        none ->
            [tag_open | scan_tag_name(Rest, Str, Pos + Len + 1, 0)];
        TxtToken ->
            [TxtToken, tag_open | scan_tag_name(Rest, Str, Pos + Len + 1, 0)]
    end;
scan(<<${, Rest/binary>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        none ->
            scan_expr(Rest, Str, Pos + Len + 1);
        TxtToken ->
            [TxtToken | scan_expr(Rest, Str, Pos + Len + 1)]
    end;
scan(<<_, Rest/binary>>, Str, Pos, Len) ->
    scan(Rest, Str, Pos, Len + 1);
scan(<<>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        none ->
            [];
        TxtToken ->
            [TxtToken]
    end.

scan_expr(Rest0, Str, Pos) ->
    {Len, MarkerLen, Rest} = find_expr_end(Rest0, 0, 0),
    Expr = binary_part(Str, Pos, Len),
    [{expr, Expr} | scan(Rest, Str, Pos + Len + MarkerLen, 0)].

find_expr_end(<<$}, Rest/binary>>, 0, Len) ->
    {Len, 1, Rest};
find_expr_end(<<$}, Rest/binary>>, Depth, Len) ->
    find_expr_end(Rest, Depth - 1, Len + 1);
find_expr_end(<<${, Rest/binary>>, Depth, Len) ->
    find_expr_end(Rest, Depth + 1, Len + 1);
find_expr_end(<<$", Rest0/binary>>, Depth, Len0) ->
    {Len, Rest} = find_str_end(Rest0, Len0 + 1),
    find_expr_end(Rest, Depth, Len);
find_expr_end(<<_, Rest/binary>>, Depth, Len) ->
    find_expr_end(Rest, Depth, Len + 1).

find_str_end(<<$\\, $", Rest/binary>>, Len) ->
    find_str_end(Rest, Len + 2);
find_str_end(<<$", Rest/binary>>, Len) ->
    {Len + 1, Rest};
find_str_end(<<_, Rest/binary>>, Len) ->
    find_str_end(Rest, Len + 1).

scan_tag_name(<<$/, $>, Rest/binary>>, Str, Pos, Len) ->
    Name = binary_part(Str, Pos, Len),
    [{tag_name, Name}, void_close | scan(Rest, Str, Pos + Len + 2, 0)];
scan_tag_name(<<$>, Rest/binary>>, Str, Pos, Len) ->
    Name = binary_part(Str, Pos, Len),
    [{tag_name, Name}, tag_close | scan(Rest, Str, Pos + Len + 1, 0)];
scan_tag_name(<<C, Rest/binary>>, Str, Pos, Len)
    when C =:= $\s; C =:= $\r; C =:= $\n ->
    Name = binary_part(Str, Pos, Len),
    [{tag_name, Name} | scan_attr_key(Rest, Str, Pos + Len + 1, 0)];
scan_tag_name(<<_, Rest/binary>>, Str, Pos, Len) ->
    scan_tag_name(Rest, Str, Pos, Len + 1).

scan_attr_key(<<$/, $>, Rest/binary>>, Str, Pos, 0) ->
    [void_close | scan(Rest, Str, Pos + 2, 0)];
scan_attr_key(<<$/, $>, Rest/binary>>, Str, Pos, Len) ->
    Key = binary_part(Str, Pos, Len),
    [{attr_key, Key}, void_close | scan(Rest, Str, Pos + Len + 2, 0)];
scan_attr_key(<<$>, Rest/binary>>, Str, Pos, 0) ->
    [tag_close | scan(Rest, Str, Pos + 1, 0)];
scan_attr_key(<<$>, Rest/binary>>, Str, Pos, Len) ->
    Key = binary_part(Str, Pos, Len),
    [{attr_key, Key}, tag_close | scan(Rest, Str, Pos + Len + 1, 0)];
scan_attr_key(<<C, Rest/binary>>, Str, Pos, 0)
    when C =:= $\s; C =:= $\r; C =:= $\n ->
    scan_attr_key(Rest, Str, Pos + 1, 0);
scan_attr_key(<<C, Rest/binary>>, Str, Pos, Len)
    when C =:= $\s; C =:= $\r; C =:= $\n ->
    Key = binary_part(Str, Pos, Len),
    [{attr_key, Key} | scan_attr_key(Rest, Str, Pos + Len + 1, 0)];
scan_attr_key(<<$=, ${, Rest/binary>>, Str, Pos, Len) ->
    Key = binary_part(Str, Pos, Len),
    [{attr_key, Key} | scan_attr_expr(Rest, Str, Pos + Len + 2)];
scan_attr_key(<<$=, C, Rest/binary>>, Str, Pos, Len)
    when C =:= $"; C =:= $' ->
    Key = binary_part(Str, Pos, Len),
    [{attr_key, Key} | scan_attr_value(Rest, Str, Pos + Len + 2, 0)];
scan_attr_key(<<_, Rest/binary>>, Str, Pos, Len) ->
    scan_attr_key(Rest, Str, Pos, Len + 1).

scan_attr_value(<<$\\, _, Rest/binary>>, Str, Pos, Len) ->
    scan_attr_value(Rest, Str, Pos, Len + 1);
scan_attr_value(<<C, Rest/binary>>, Str, Pos, Len)
    when C =:= $"; C =:= $' ->
    Value = binary_part(Str, Pos, Len),
    [{attr_value, Value} | scan_attr_key(Rest, Str, Pos + Len + 1, 0)];
scan_attr_value(<<_, Rest/binary>>, Str, Pos, Len) ->
    scan_attr_value(Rest, Str, Pos, Len + 1).

scan_attr_expr(Rest0, Str, Pos) ->
    {Len, MarkerLen, Rest} = find_expr_end(Rest0, 0, 0),
    Expr = binary_part(Str, Pos, Len),
    [{attr_expr, Expr} | scan_attr_key(Rest, Str, Pos + Len + MarkerLen, 0)].

text_token(Str, Pos, Len) ->
    case string:trim(binary_part(Str, Pos, Len)) of
        <<>> ->
            none;
        Txt ->
            {text, Txt}
    end.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

string_test() ->
    ?assertEqual([
        {text, <<"Start">>},
        {expr, <<"% This is a comment. ">>},
        tag_open,
        {tag_name, <<"main">>},
        {attr_key, <<"id">>},
        {attr_value, <<"foo">>},
        {attr_key, <<"class">>},
        {attr_expr, <<"_@class">>},
        {attr_key, <<"style">>},
        {attr_value, <<"display: none;">>},
        {attr_key, <<"hidden">>},
        tag_close,
        {text, <<"foo">>},
        {expr, <<"% Comments are allowed in expressions.\n        _@bar">>},
        {text, <<"baz">>},
        tag_open,
        {tag_name, <<"br">>},
        void_close, tag_open,
        {tag_name, <<"div">>},
        tag_close, tag_open,
        {tag_name, <<"span">>},
        {attr_key, <<"id">>},
        {attr_value, <<"nested">>},
        tag_close, tag_open,
        {tag_name, <<".foo:nested">>},
        tag_close,
        {text, <<"ok">>},
        closing_tag,
        {tag_name, <<".foo:nested">>},
        tag_close, closing_tag,
        {tag_name, <<"span">>},
        tag_close, closing_tag,
        {tag_name, <<"div">>},
        tag_close, tag_open,
        {tag_name, <<".foo:block">>},
        {attr_key, <<":if">>},
        {attr_expr, <<"_@true">>},
        void_close, tag_open,
        {tag_name, <<".foo:counter">>},
        {attr_key, <<"id">>},
        {attr_value, <<"counter">>},
        {attr_key, <<"count">>},
        {attr_expr, <<"0">>},
        tag_close, closing_tag,
        {tag_name, <<".foo:counter">>},
        tag_close, closing_tag,
        {tag_name, <<"main">>},
        tag_close,
        {text, <<"End">>}
    ], string(<<"""
    Start
    {% This is a comment. }
    <main id="foo" class={_@class} style='display: none;' hidden>
        foo{% Comments are allowed in expressions.
            _@bar}baz
        <br/>
        <div>
            <span id="nested">
                <.foo:nested>ok</.foo:nested>
            </span>
        </div>
        <.foo:block :if={_@true}/>
        <.foo:counter id="counter" count={0}></.foo:counter>
    </main>
    End
    """>>)).

-endif.
