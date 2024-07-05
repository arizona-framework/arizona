-module(arizona_template_parser).
-moduledoc false.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse/1]).

%

-ignore_xref([parse/1]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-opaque element() :: {text, arizona_template_scanner:location(), binary()}
                   | {expr, arizona_template_scanner:location(), binary()}
                   | {block, arizona_template_scanner:location(), block()}
                   | {tag, arizona_template_scanner:location(), tag()}.
-export_type([element/0]).

-type block() :: #{
    name := binary(),
    attributes := [attribute()]
}.
-export_type([block/0]).

-type tag() :: #{
    name := binary(),
    is_stateful := boolean(),
    is_void := boolean(),
    attributes := [attribute()],
    inner_content := [element()]
}.
-export_type([tag/0]).

-type attribute_key() :: binary().
-type attribute_value() :: {text, arizona_template_scanner:location(), binary()}
                         | {expr, arizona_template_scanner:location(), binary()}.
-type attribute() :: {attribute_key(), attribute_value()}.
-export_type([attribute/0]).

-type error_reason() :: unexpected_block_end
                      | unexpected_tag_end
                      | invalid_directive.
-export_type([error_reason/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec parse(Tokens) -> Result
    when Tokens :: [arizona_template_scanner:token()],
         Result :: {ok, [element()]}
                 | {error, {error_reason(), arizona_template_scanner:location()}}.
parse(Tokens) ->
    try
        {ok, do_parse(Tokens)}
    catch
        throw:{Reason, Loc} ->
            {error, {Reason, Loc}}
    end.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

do_parse([{text, Loc, Txt} | T]) ->
    [{text, Loc, Txt} | do_parse(T)];
do_parse([{expr, Loc, Expr} | T]) ->
    [{expr, Loc, Expr} | do_parse(T)];
do_parse([{open_tag, Loc, <<$., Name/binary>>} | T0]) ->
    {Block, T} = parse_block(Name, Loc, T0),
    [{block, Loc, Block} | do_parse(T)];
do_parse([{open_tag, Loc, Name} | T0]) ->
    {Tag, T} = parse_tag(Name, Loc, T0),
    [{tag, Loc, Tag} | do_parse(T)];
do_parse([]) ->
    [].

parse_block(Name, Loc, T0) ->
    case collect_block_props(T0, [{name, Name}]) of
        {ok, {Props, T}} ->
            {normalize_block_props(Props), T};
        {error, Reason} ->
            throw({Reason, Loc})
    end.

collect_block_props([{attr_name, _, Name},
                     {attr_value, Loc, {text, Txt}} | T], Props) ->
    collect_block_props(T, [{attribute, {Name, {text, Loc, Txt}}} | Props]);
collect_block_props([{attr_name, _, Name},
                     {attr_value, Loc, {expr, Expr}} | T], Props) ->
    collect_block_props(T, [{attribute, {Name, {expr, Loc, Expr}}} | Props]);
collect_block_props([{bool_attr, Loc, Name} | T], Props) ->
    collect_block_props(T, [{attribute, {Name, {expr, Loc, true}}} | Props]);
collect_block_props([{close_tag, _, {void, true}} | T], Props) ->
    {ok, {Props, T}};
collect_block_props([{close_tag, _, {void, false}} | _T], _Props) ->
    {error, unexpected_block_end}.

normalize_block_props(Props) ->
    {name, Name} = proplists:lookup(name, Props),
    #{
        name => Name,
        attributes => lists:reverse(proplists:get_all_values(attribute, Props))
     }.

parse_tag(Name, Loc, T0) ->
    case collect_tag_props(T0, [{name, Name}]) of
        {ok, {Props, T}} ->
            {normalize_tag_props(Props), T};
        {error, {Reason, ErrLoc}} ->
            throw({Reason, ErrLoc});
        {error, Reason} ->
            throw({Reason, Loc})
    end.

collect_tag_props([{attr_name, _, <<":on", Action/binary>>},
                   {attr_value, Loc, {text, Event}} | T], Props) ->
    collect_tag_props(T, [{attribute, {<<"on", Action/binary>>,
                                       {text, Loc, arizona_js:send(Event)}}} | Props]);
collect_tag_props([{attr_name, _, <<":on", Action/binary>>},
                   {attr_value, Loc, {expr, Expr}} | T], Props) ->
    collect_tag_props(T, [{attribute, {<<"on", Action/binary>>, {expr, Loc, Expr}}} | Props]);
collect_tag_props([{attr_name, Loc, <<$:, _/binary>>} | _T], _Props) ->
    {error, {invalid_directive, Loc}};
collect_tag_props([{attr_name, _, Name},
                   {attr_value, Loc, {text, Txt}} | T], Props) ->
    collect_tag_props(T, [{attribute, {Name, {text, Loc, Txt}}} | Props]);
collect_tag_props([{attr_name, _, Name},
                   {attr_value, Loc, {expr, Expr}} | T], Props) ->
    collect_tag_props(T, [{attribute, {Name, {expr, Loc, Expr}}} | Props]);
collect_tag_props([{bool_attr, _, <<":stateful">>} | T], Props) ->
    collect_tag_props(T, [is_stateful | Props]);
collect_tag_props([{bool_attr, Loc, <<$:, _/binary>>} | _T], _Props) ->
    {error, {invalid_directive, Loc}};
collect_tag_props([{bool_attr, Loc, Name} | T], Props) ->
    collect_tag_props(T, [{attribute, {Name, {text, Loc, Name}}} | Props]);
collect_tag_props([{close_tag, _, {void, true}} | T], Props) ->
    {ok, {[is_void | Props], T}};
collect_tag_props([{close_tag, _, {void, false}} | T], Props) ->
    collect_nonvoid_tag_props(T, Props).

collect_nonvoid_tag_props([{closing_tag, _, CloseTagName} | T], Props) ->
    {name, OpenTagName} = proplists:lookup(name, Props),
    case OpenTagName =:= CloseTagName of
        true ->
            {ok, {Props, T}};
        false ->
            {error, unexpected_tag_end}
    end;
collect_nonvoid_tag_props([{text, Loc, Txt} | T], Props) ->
    collect_nonvoid_tag_props(T, [{inner_content, {text, Loc, Txt}} | Props]);
collect_nonvoid_tag_props([{expr, Loc, Expr} | T], Props) ->
    collect_nonvoid_tag_props(T, [{inner_content, {expr, Loc, Expr}} | Props]);
collect_nonvoid_tag_props([{open_tag, Loc, <<$., Name/binary>>} | T0], Props) ->
    {Block, T} = parse_block(Name, Loc, T0),
    collect_nonvoid_tag_props(T, [{inner_content, {block, Loc, Block}} | Props]);
collect_nonvoid_tag_props([{open_tag, Loc, Name} | T0], Props) ->
    {Tag, T} = parse_tag(Name, Loc, T0),
    collect_nonvoid_tag_props(T, [{inner_content, {tag, Loc, Tag}} | Props]);
collect_nonvoid_tag_props([], _Props) ->
    {error, unexpected_tag_end}.

normalize_tag_props(Props) ->
    {name, Name} = proplists:lookup(name, Props),
    #{
        name => Name,
        is_stateful => proplists:get_bool(is_stateful, Props),
        is_void => proplists:get_bool(is_void, Props),
        attributes => lists:reverse(proplists:get_all_values(attribute, Props)),
        inner_content => lists:reverse(proplists:get_all_values(inner_content, Props))
     }.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_ok_test() ->
    ?assertEqual({ok, [
        {tag,
         {1, 1},
         #{attributes => [{<<"html">>, {text, {1, 11}, <<"html">>}}],
           name => <<"!DOCTYPE">>, is_stateful => false,
           is_void => true, inner_content => []}},
        {tag,
         {2, 1},
         #{attributes => [{<<"lang">>, {text, {2, 12}, <<"en">>}}],
           name => <<"html">>, is_stateful => false,
           is_void => false,
           inner_content =>
            [{tag,
              {3, 1},
              #{attributes => [], name => <<"head">>,
                is_stateful => false, is_void => false,
                inner_content =>
                 [{tag,
                   {4, 5},
                   #{attributes =>
                      [{<<"charset">>, {text, {4, 19}, <<"UTF-8">>}}],
                     name => <<"meta">>, is_stateful => false,
                     is_void => true, inner_content => []}},
                  {tag,
                   {5, 5},
                   #{attributes => [], name => <<"title">>,
                     is_stateful => false, is_void => false,
                     inner_content =>
                      [{expr, {5, 12}, <<"_@title">>}]}},
                  {tag,
                   {6, 5},
                   #{attributes =>
                      [{<<"src">>,
                        {text, {6, 17}, <<"assets/js/main.js">>}}],
                     name => <<"script">>, is_stateful => false,
                     is_void => false, inner_content => []}}]}},
             {tag,
              {8, 1},
              #{attributes => [], name => <<"body">>,
                is_stateful => false, is_void => false,
                inner_content =>
                 [{tag,
                   {9, 5},
                   #{attributes => [], name => <<"h1">>,
                     is_stateful => false, is_void => false,
                     inner_content =>
                      [{text, {9, 9}, <<"Arizona Counter">>}]}},
                  {block,
                   {10, 5},
                   #{attributes =>
                      [{<<"count">>, {expr, {11, 15}, <<"_@count">>}},
                       {<<"btn_text">>,
                        {text, {12, 18}, <<"Increment">>}},
                       {<<"event">>, {text, {13, 15}, <<"incr">>}}],
                     name => <<"counter">>}}]}}]}}
    ]}, parse(tokens_ok())).

parse_unexpected_block_end_test() ->
    {ok, Tokens} = arizona_template_scanner:scan(~"""
    <.foo id="bar">
    """),
    ?assertEqual({error, {unexpected_block_end, {1, 1}}}, parse(Tokens)).

parse_unexpected_tag_end_test() ->
    {ok, Tokens} = arizona_template_scanner:scan(~"""
    <foo id="bar">
    """),
    ?assertEqual({error, {unexpected_tag_end, {1, 1}}}, parse(Tokens)).

parse_invalid_directive_test() ->
    {ok, Tokens} = arizona_template_scanner:scan(~"""
    <foo :id="bar"></foo>
    """),
    ?assertEqual({error, {invalid_directive, {1, 6}}}, parse(Tokens)).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

tokens_ok() ->
    {ok, Tokens} = arizona_template_scanner:scan(~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>{_@title}</title>
        <script src="assets/js/main.js"></script>
    </head>
    <body>
        <h1>Arizona Counter</h1>
        <.counter
            count={_@count}
            btn_text="Increment"
            event="incr"
        />
    </body>
    </html>
    """),
    Tokens.

-endif.

