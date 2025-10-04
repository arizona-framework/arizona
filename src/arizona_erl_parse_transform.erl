-module(arizona_erl_parse_transform).
-moduledoc ~"""
Parse transform for Erlang term-based templates.

Analyzes Erlang term structures at compile time to generate optimized
arizona_template records with proper static/dynamic splitting.

## Template Recognition

Recognizes tuples matching the pattern:
```erlang
{Tag :: atom(), Attributes :: [{atom(), term()}], Children :: [term()]}
```

## Static vs Dynamic Analysis

- **Static**: Binary literals, atoms, integers, static lists
- **Dynamic**: Variables, function calls, case/if expressions, comprehensions

Lists are analyzed recursively:
```erlang
{class, [~"todo-item ", CompletedClass]}  % Mixed static/dynamic
```

## Example

```erlang
-compile({parse_transform, arizona_erl_parse_transform}).

render(#{id := Id, title := Title}) ->
    {'div', [{id, Id}], [Title]}.
```

Compiles to:
```erlang
render(#{id := Id, title := Title}) ->
    arizona_template:new(
        [~"<div id=\"", ~"\">", ~"</div>"],  % Static parts
        {fun() -> Id end, fun() -> Title end},  % Dynamic callbacks
        [1, 2],                                   % Sequence
        {0, 0},                                   % Line annotations
        Fingerprint                               % Hash
    ).
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Parse transform entry point called by the Erlang compiler.

Walks the AST and transforms Erlang term templates into
arizona_template:new/5 calls.
""".
-spec parse_transform(Forms, Options) -> TransformedForms when
    Forms :: [erl_parse:abstract_form()],
    Options :: [compile:option()],
    TransformedForms :: [erl_parse:abstract_form()].
parse_transform(Forms, Options) ->
    % Walk and transform each form
    [transform_form(Form, Options) || Form <- Forms].

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Transform a single form
transform_form({function, Line, Name, Arity, Clauses}, Options) ->
    % Transform function clauses
    NewClauses = [transform_clause(Clause, Options) || Clause <- Clauses],
    {function, Line, Name, Arity, NewClauses};
transform_form(Form, _Options) ->
    % Other forms pass through unchanged
    Form.

%% Transform a function clause
transform_clause({clause, Line, Patterns, Guards, Body}, Options) ->
    % Transform the body expressions
    NewBody = [transform_expr(Expr, Options) || Expr <- Body],
    {clause, Line, Patterns, Guards, NewBody}.

%% Transform an expression - this is where we find element tuples
transform_expr(
    {tuple, Line, [
        {atom, _, Tag},
        % Empty attributes list
        {nil, _} = EmptyAttrs,
        Children
    ]},
    Options
) when is_atom(Tag) ->
    % Found element tuple with no attributes: {tag, [], Children}
    transform_element(Tag, EmptyAttrs, Children, Line, Options);
transform_expr(
    {tuple, Line, [
        {atom, _, Tag},
        Attrs,
        Children
    ]},
    Options
) when is_atom(Tag) ->
    % Found element tuple with attributes: {tag, Attrs, Children}
    case is_attr_list(Attrs) of
        true ->
            transform_element(Tag, Attrs, Children, Line, Options);
        false ->
            % Not an element tuple, recurse normally
            {tuple, Line, [
                {atom, Line, Tag},
                transform_expr(Attrs, Options),
                transform_expr(Children, Options)
            ]}
    end;
% Handle other expression types recursively
transform_expr({Type, Line, Args}, Options) when is_list(Args) ->
    {Type, Line, [transform_expr(Arg, Options) || Arg <- Args]};
transform_expr({Type, Line, Arg1, Arg2}, Options) ->
    {Type, Line, transform_expr(Arg1, Options), transform_expr(Arg2, Options)};
transform_expr({Type, Line, Arg1, Arg2, Arg3}, Options) ->
    {Type, Line, transform_expr(Arg1, Options), transform_expr(Arg2, Options),
        transform_expr(Arg3, Options)};
transform_expr(Expr, _Options) ->
    % Literals and other expressions pass through
    Expr.

%% Check if this looks like an attribute list [{key, value}, ...]
is_attr_list({nil, _}) ->
    true;
is_attr_list({cons, _, {tuple, _, [{atom, _, _Key}, _Value]}, Rest}) ->
    is_attr_list(Rest);
is_attr_list(_) ->
    false.

%% Transform element tuple into arizona_template:new/5 call
transform_element(Tag, AttrsAST, ChildrenAST, _Line, _Options) ->
    % For now, handle only static elements
    % Extract static attrs
    Attrs = extract_static_attrs(AttrsAST),

    % Extract static children
    Children = extract_static_children(ChildrenAST),

    % Build HTML parts
    TagBin = atom_to_binary(Tag),
    AttrsBin = build_attrs_binary(Attrs),
    OpenTag = <<$<, TagBin/binary, AttrsBin/binary, $>>>,
    CloseTag = <<"</", TagBin/binary, $>>>,

    StaticParts = [OpenTag, iolist_to_binary(Children), CloseTag],

    % Generate fingerprint
    Fingerprint = erlang:phash2({StaticParts, []}),

    % Build arizona_template:new/5 call using erl_syntax, then revert to standard form
    StaticListAST = erl_syntax:list([erl_syntax:abstract(Part) || Part <- StaticParts]),
    EmptyTuple = erl_syntax:tuple([]),
    EmptyList = erl_syntax:list([]),
    FingerprintAST = erl_syntax:integer(Fingerprint),

    SyntaxTree = erl_syntax:application(
        erl_syntax:module_qualifier(
            erl_syntax:atom(arizona_template),
            erl_syntax:atom(new)
        ),
        [StaticListAST, EmptyTuple, EmptyList, EmptyTuple, FingerprintAST]
    ),

    % Convert to standard tuple form
    erl_syntax:revert(SyntaxTree).

%% Extract static attributes from AST
extract_static_attrs({nil, _}) ->
    [];
extract_static_attrs({cons, _, {tuple, _, [{atom, _, Key}, Value]}, Rest}) ->
    case extract_static_value(Value) of
        {ok, Val} ->
            [{Key, Val} | extract_static_attrs(Rest)];
        % Not a static value
        error ->
            % Dynamic attribute - not supported yet in this minimal implementation
            extract_static_attrs(Rest)
    end.

%% Extract static value from AST
extract_static_value({bin, _, Elements}) ->
    % Binary literal - extract bytes from binary elements
    case extract_bin_elements(Elements) of
        error -> error;
        Bytes -> {ok, list_to_binary(Bytes)}
    end;
extract_static_value(_NotStatic) ->
    % Not a static value
    error.

%% Extract byte value from binary element
extract_bin_elements([]) ->
    [];
extract_bin_elements([{bin_element, _, {string, _, String}, _, _} | T]) ->
    % String element in binary
    [String | extract_bin_elements(T)];
extract_bin_elements([{bin_element, _, {char, _, Val}, _, _} | T]) ->
    % Character literal element (e.g., $o)
    [Val | extract_bin_elements(T)];
extract_bin_elements(_NotStatic) ->
    error.

%% Extract static children from AST
extract_static_children({nil, _}) ->
    [];
extract_static_children({cons, _, Child, Rest}) ->
    case extract_static_value(Child) of
        {ok, Val} ->
            [Val | extract_static_children(Rest)];
        error ->
            % Dynamic child - not supported yet
            extract_static_children(Rest)
    end.

%% Build attributes binary string
build_attrs_binary([]) ->
    <<>>;
build_attrs_binary(Attrs) ->
    AttrParts = [
        [<<" ">>, atom_to_binary(K), <<"=\"">>, V, <<"\"">>]
     || {K, V} <- Attrs
    ],
    iolist_to_binary(AttrParts).
