%%
%% %CopyrightBegin%
%%
%% Copyright 2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(arizona_tpl_scan).
-moduledoc """
Template scanner.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([string/1]).

-opaque token() :: term().
-export_type([token/0]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

-spec string(String) -> Tokens
    when String :: binary() | string(),
         Tokens :: [token()].
string(Str) when is_binary(Str) ->
    scan(Str, Str, 0, 0);
string(Str) when is_list(Str) ->
    string(iolist_to_binary(Str)).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

scan(<<$<, $/, Rest/binary>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        {ok, TxtToken} ->
            [TxtToken, closing_tag | scan_tag_name(Rest, Str, Pos + Len + 2, 0)];
        none ->
            [closing_tag | scan_tag_name(Rest, Str, Pos + Len + 2, 0)]
    end;
scan(<<$<, Rest/binary>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        {ok, TxtToken} ->
            [TxtToken, tag_open | scan_tag_name(Rest, Str, Pos + Len + 1, 0)];
        none ->
            [tag_open | scan_tag_name(Rest, Str, Pos + Len + 1, 0)]
    end;
scan(<<${, Rest/binary>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        {ok, TxtToken} ->
            [TxtToken | scan_expr(Rest, Str, Pos + Len + 1)];
        none ->
            scan_expr(Rest, Str, Pos + Len + 1)
    end;
scan(<<_, Rest/binary>>, Str, Pos, Len) ->
    scan(Rest, Str, Pos, Len + 1);
scan(<<>>, Str, Pos, Len) ->
    case text_token(Str, Pos, Len) of
        {ok, TxtToken} ->
            [TxtToken];
        none ->
            []
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
            {ok, {text, Txt}}
    end.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

string_test() ->
    ?assertEqual({ok, [
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
    ], 1}, string(<<"""
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

