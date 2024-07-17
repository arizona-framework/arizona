-module(arizona_template_parser).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse/1]).
-export([dummy_script_tag/1]).
-export([dummy_text_attribute/2]).

%

-ignore_xref([parse/1]).
-ignore_xref([dummy_script_tag/1]).
-ignore_xref([dummy_text_attribute/2]).

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
    attributes := [attribute()],
    condition := condition()
}.
-export_type([block/0]).

-type tag() :: #{
    name := binary(),
    is_stateful := boolean(),
    is_void := boolean(),
    attributes := [attribute()],
    inner_content := [element()],
    condition := condition()
}.
-export_type([tag/0]).

-type attribute_key() :: binary().
-export_type([attribute_key/0]).

-type attribute_value() :: text_attribute() | expr_attribute().
-export_type([attribute_value/0]).

-type text_attribute() :: {text, arizona_template_scanner:location(), binary()}.
-export_type([text_attribute/0]).

-type expr_attribute() :: {expr, arizona_template_scanner:location(), binary()}.
-export_type([expr_attribute/0]).

-type attribute() :: {attribute_key(), attribute_value()}.
-export_type([attribute/0]).

-type condition() :: none | {'if', arizona_template_scanner:location(), binary()}.
-export_type([condition/0]).

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

% Used by the arizona_template_compiler to inject scripts.
-spec dummy_script_tag(Src) -> TagElem
    when Src :: binary(),
         TagElem :: {tag, arizona_template_scanner:location(), tag()}.
dummy_script_tag(Src) when is_binary(Src) ->
    {tag, {1, 1}, normalize_tag_props([
        {name, ~"script"},
        {attribute, dummy_text_attribute(~"src", Src)}
    ])}.

% Used by the arizona_template_compiler to inject attributes.
-spec dummy_text_attribute(Name, Txt) -> Attr
    when Name :: attribute_key(),
         Txt :: binary(),
         Attr :: {attribute_key(), text_attribute()}.
dummy_text_attribute(Name, Txt) ->
    {Name, {text, {1, 1}, Txt}}.

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
do_parse([{comment, _, _} | T]) ->
    do_parse(T);
do_parse([]) ->
    [].

parse_block(Name, Loc, T0) ->
    case collect_block_props(T0, [{name, Name}]) of
        {ok, {Props, T}} ->
            {normalize_block_props(Props), T};
        {error, Reason} ->
            throw({Reason, Loc})
    end.

collect_block_props([{attr_name, _, <<":if">>},
                     {attr_value, Loc, {expr, Expr}} | T], Props) ->
    collect_block_props(T, [{condition, {'if', Loc, Expr}} | Props]);
collect_block_props([{attr_name, Loc, <<$:, _/binary>>} | _T], _Props) ->
    {error, {invalid_directive, Loc}};
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
        attributes => lists:reverse(proplists:get_all_values(attribute, Props)),
        condition => proplists:get_value(condition, Props, none)
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

collect_tag_props([{attr_name, _, <<":if">>},
                   {attr_value, Loc, {expr, Expr}} | T], Props) ->
    collect_tag_props(T, [{condition, {'if', Loc, Expr}} | Props]);
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
collect_nonvoid_tag_props([{comment, _, _} | T], Props) ->
    collect_nonvoid_tag_props(T, Props);
collect_nonvoid_tag_props([], _Props) ->
    {error, unexpected_tag_end}.

normalize_tag_props(Props) ->
    {name, Name} = proplists:lookup(name, Props),
    #{
        name => Name,
        is_stateful => proplists:get_bool(is_stateful, Props),
        is_void => proplists:get_bool(is_void, Props),
        attributes => lists:reverse(proplists:get_all_values(attribute, Props)),
        inner_content => lists:reverse(proplists:get_all_values(inner_content, Props)),
        condition => proplists:get_value(condition, Props, none)
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
           is_void => true, inner_content => [], condition => none}},
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
                     is_void => true, inner_content => [],
                     condition => none}},
                  {tag,
                   {5, 5},
                   #{attributes => [], name => <<"title">>,
                     is_stateful => false, is_void => false,
                     inner_content =>
                      [{expr, {5, 12}, <<"_@title">>}],
                     condition => none}},
                  {tag,
                   {6, 5},
                   #{attributes =>
                      [{<<"src">>,
                        {text, {6, 17}, <<"assets/js/main.js">>}}],
                     name => <<"script">>, is_stateful => false,
                     is_void => false, inner_content => [],
                     condition => none}}],
                condition => none}},
             {tag,
              {8, 1},
              #{attributes => [], name => <<"body">>,
                is_stateful => false, is_void => false,
                inner_content =>
                 [{tag,
                   {10, 5},
                   #{attributes => [], name => <<"h1">>,
                     is_stateful => false, is_void => false,
                     inner_content =>
                      [{text, {10, 9}, <<"Arizona Counter">>}],
                     condition => none}},
                  {block,
                   {11, 5},
                   #{attributes =>
                      [{<<"count">>, {expr, {12, 15}, <<"_@count">>}},
                       {<<"btn_text">>,
                        {text, {13, 18}, <<"Increment">>}},
                       {<<"event">>, {text, {14, 15}, <<"incr">>}}],
                     name => <<"counter">>, condition => none}}],
                condition => none}}],
           condition => none}}
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

if_directive_test() ->
    {ok, Tokens} = arizona_template_scanner:scan(~"""
    <.foo :if={_@bool} />
    <.foo />
    <foo :if={true} />
    <foo />
    """),
    ?assertEqual({ok, [
        {block, {1, 1}, #{
            name => <<"foo">>,
            attributes => [],
            condition => {'if', {1, 11}, <<"_@bool">>}
        }},
        {block, {2, 1}, #{
            name => <<"foo">>,
            attributes => [],
            condition => none
        }},
        {tag, {3, 1}, #{
            name => <<"foo">>,
            attributes => [],
            is_stateful => false,
            is_void => true,
            inner_content => [],
            condition => {'if', {3, 10}, <<"true">>}
        }},
        {tag, {4, 1}, #{
            name => <<"foo">>,
            attributes => [],
            is_stateful => false,
            is_void => true,
            inner_content => [],
            condition => none
        }}
    ]}, parse(Tokens)).

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
        {% Comments must be ignored. }
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
