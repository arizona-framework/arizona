-module(arizona_html).
-moduledoc ~"""
HTML value conversion utilities for template rendering.

Provides unified conversion of Erlang data types to HTML-safe iodata.
Used throughout the rendering pipeline to convert template binding
values into HTML content.

## Supported Types

- `binary()` - Passed through unchanged
- `iolist()` - Passed through unchanged
- `atom()` - Converted to binary with UTF-8 encoding
- `integer()` - Converted to decimal binary representation
- `float()` - Converted to binary using default formatting

## Example

```erlang
1> arizona_html:to_html(~"hello").
~"hello"
2> arizona_html:to_html(world).
~"world"
3> arizona_html:to_html(123).
~"123"
4> arizona_html:to_html(45.67).
~"45.67"
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_html/1]).
-export([from_erl/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([html/0]).
-export_type([value/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal html() :: iodata().
-type value() :: binary() | iolist() | atom() | number().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Converts a value to HTML-safe `iodata/0`.

Handles multiple Erlang data types and converts them to a format
suitable for HTML output. Binaries and iolists are passed through
unchanged, while other types are converted to binary representation.
""".
-spec to_html(Value) -> HTML when
    Value :: value(),
    HTML :: html().
to_html(Value) when is_binary(Value) ->
    Value;
% Assume it is an iolist()
to_html(Value) when is_list(Value) ->
    Value;
to_html(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_html(Value) when is_integer(Value) ->
    integer_to_binary(Value, 10);
to_html(Value) when is_float(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

-doc ~"""
Converts Erlang term elements to HTML string for template compilation.

Transforms element syntax like `{'div', [{id, Id}], [Title]}` into
HTML strings like `"<div id=\"{Id}\">{Title}</div>"` for use with
arizona_template:from_html/1 pipeline.

Static binaries remain as text, dynamic expressions become `{Expr}`.
""".
-spec from_erl(ElementAST) -> HTML when
    ElementAST :: erl_syntax:syntaxTree(),
    HTML :: html().
from_erl(ElementAST) ->
    case erl_syntax:type(ElementAST) of
        list ->
            % List of elements
            Elements = erl_syntax:list_elements(ElementAST),
            [element_to_html(El) || El <- Elements];
        _Other ->
            % Single element
            element_to_html(ElementAST)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Convert single element to HTML
element_to_html(ElementAST) ->
    case extract_element_tuple(ElementAST) of
        {ok, Tag, AttrsAST, ChildrenAST} ->
            TagBin = atom_to_binary(Tag),
            AttrsHTML = attrs_to_html(AttrsAST),
            ChildrenHTML = children_to_html(ChildrenAST),
            [
                $<,
                TagBin,
                AttrsHTML,
                $>,
                ChildrenHTML,
                $<,
                $/,
                TagBin,
                $>
            ];
        error ->
            % Not an element, convert to dynamic expression
            ast_to_html_expr(ElementAST)
    end.

%% Extract element tuple from AST
extract_element_tuple(ElementAST) ->
    case erl_syntax:type(ElementAST) of
        tuple ->
            case erl_syntax:tuple_elements(ElementAST) of
                [{atom, _, Tag}, AttrsAST, ChildrenAST] ->
                    case is_attr_list(AttrsAST) of
                        true -> {ok, Tag, AttrsAST, ChildrenAST};
                        false -> error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% Check if this looks like an attribute list
is_attr_list({nil, _}) ->
    true;
is_attr_list({cons, _, {tuple, _, [{atom, _, _Key}, _Value]}, Rest}) ->
    is_attr_list(Rest);
is_attr_list({cons, _, {atom, _, _Key}, Rest}) ->
    % Boolean attribute
    is_attr_list(Rest);
is_attr_list(_) ->
    false.

%% Convert attributes list to HTML
attrs_to_html({nil, _}) ->
    [];
attrs_to_html({cons, _, Attr, Rest}) ->
    AttrHTML = attr_to_html(Attr),
    RestHTML = attrs_to_html(Rest),
    [AttrHTML | RestHTML].

%% Convert single attribute to HTML
attr_to_html({tuple, _, [{atom, _, Key}, Value]}) ->
    KeyBin = atom_to_binary(Key),
    ValueHTML = attr_value_to_html(Value),
    [$\s, KeyBin, $=, $", ValueHTML, $"];
attr_to_html({atom, _, Key}) ->
    % Boolean attribute
    [$\s, atom_to_binary(Key)].

%% Convert attribute value to HTML
attr_value_to_html({bin, _, Elements}) ->
    extract_binary_value(Elements);
attr_value_to_html({cons, _, _Head, _Tail} = List) ->
    % List of values (static + dynamic)
    list_to_html(List);
attr_value_to_html(ValueAST) ->
    % Dynamic expression
    ast_to_html_expr(ValueAST).

%% Convert children list to HTML
children_to_html({nil, _}) ->
    [];
children_to_html({cons, _, Child, Rest}) ->
    ChildHTML = child_to_html(Child),
    RestHTML = children_to_html(Rest),
    [ChildHTML | RestHTML].

%% Convert single child to HTML
child_to_html({bin, _, Elements}) ->
    extract_binary_value(Elements);
child_to_html(ChildAST) ->
    case extract_element_tuple(ChildAST) of
        {ok, _Tag, _Attrs, _Children} ->
            element_to_html(ChildAST);
        error ->
            ast_to_html_expr(ChildAST)
    end.

%% Convert list to HTML
list_to_html({nil, _}) ->
    [];
list_to_html({cons, _, Head, Tail}) ->
    HeadHTML =
        case Head of
            {bin, _, Elements} -> extract_binary_value(Elements);
            _ -> ast_to_html_expr(Head)
        end,
    TailHTML = list_to_html(Tail),
    [HeadHTML | TailHTML].

%% Extract static binary value
extract_binary_value(Elements) ->
    [extract_bin_element(El) || El <- Elements].

extract_bin_element({bin_element, _, {string, _, String}, _, _}) ->
    escape_braces(String);
extract_bin_element({bin_element, _, {char, _, Val}, _, _}) ->
    escape_braces(Val).

%% Escape { and " in static content, but preserve already-escaped sequences
escape_braces([]) ->
    [];
escape_braces([$\\, ${ | Rest]) ->
    % Already escaped \{ - keep as-is
    [$\\, ${ | escape_braces(Rest)];
escape_braces([$\\, $" | Rest]) ->
    % Already escaped \" - keep as-is
    [$\\, $" | escape_braces(Rest)];
escape_braces([${ | Rest]) ->
    % Unescaped { - escape it
    [$\\, ${ | escape_braces(Rest)];
escape_braces([$" | Rest]) ->
    % Unescaped " - escape it
    [$\\, $" | escape_braces(Rest)];
escape_braces([C | Rest]) ->
    % Single char, not special - return as-is
    [C | escape_braces(Rest)].

%% Convert AST to {DynamicExpr} for HTML
ast_to_html_expr(AST) ->
    ExprStr = erl_prettypr:format(erl_syntax:revert(AST)),
    [${, ExprStr, $}].
