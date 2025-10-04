-module(arizona_erl).
-moduledoc ~"""
Erlang term to HTML conversion utilities.

Provides both runtime term conversion and compile-time AST conversion:
- `to_html/1` - Runtime conversion of Erlang terms to HTML
- `ast_to_html/1` - Compile-time AST conversion for parse transform

## Supported Syntax

Elements are represented as tuples:
```erlang
{Tag :: atom(), Attributes :: [attr()], Children :: [child()]}
```

Where:
- `attr()` is `{Key :: atom(), Value :: term()}` or `atom()` (boolean attribute)
- `child()` is `binary() | iolist() | element()`

## Example

```erlang
1> arizona_erl:to_html({'div', [{id, "main"}], ["Hello"]}).
<<"<div id=\"main\">Hello</div>">>
2> arizona_erl:to_html({'input', [disabled, {type, "text"}], []}).
<<"<input disabled type=\"text\"></input>">>
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([to_html/1]).
-export([ast_to_html/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([to_html/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([element/0]).
-export_type([attr/0]).
-export_type([child/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal element() :: {Tag :: atom(), Attributes :: [attr()], Children :: [child()]}.
-nominal attr() :: {Key :: atom(), Value :: term()} | atom().
-nominal child() :: arizona_html:value() | element().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Converts Erlang term elements to HTML binary.

Handles element tuples, attributes (including boolean), and nested children.
""".
-spec to_html(Element) -> HTML when
    Element :: element() | [element()],
    HTML :: binary().
to_html(Elements) when is_list(Elements) ->
    iolist_to_binary([element_to_html(El) || El <- Elements]);
to_html(Element) ->
    iolist_to_binary(element_to_html(Element)).

-doc ~"""
Converts Erlang term AST to HTML string for template compilation.

Transforms element syntax like `{'div', [{id, Id}], [Title]}` into
HTML strings like `"<div id=\"{Id}\">{Title}</div>"` for use with
arizona_template:from_html/1 pipeline.

Static binaries remain as text, dynamic expressions become `{Expr}`.
Automatically escapes `{` and `"` in static content.
""".
-spec ast_to_html(ElementAST) -> HTML when
    ElementAST :: erl_syntax:syntaxTree(),
    HTML :: arizona_html:html().
ast_to_html(ElementAST) ->
    case erl_syntax:type(ElementAST) of
        list ->
            % List of elements
            Elements = erl_syntax:list_elements(ElementAST),
            [element_ast_to_html(El) || El <- Elements];
        _Other ->
            % Single element
            element_ast_to_html(ElementAST)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Convert single element to HTML
element_to_html({Tag, Attrs, Children}) when is_atom(Tag), is_list(Attrs), is_list(Children) ->
    TagBin = atom_to_binary(Tag),
    AttrsHTML = attrs_to_html(Attrs),
    case is_void_element(Tag) of
        true ->
            build_void_element_html(TagBin, AttrsHTML);
        false ->
            ChildrenHTML = children_to_html(Children),
            build_element_html(TagBin, AttrsHTML, ChildrenHTML)
    end.

%% Convert attributes list to HTML
attrs_to_html(Attrs) ->
    [attr_to_html(Attr) || Attr <- Attrs].

%% Convert single attribute to HTML
attr_to_html({Key, Value}) when is_atom(Key) ->
    KeyBin = atom_to_binary(Key),
    ValueHTML = arizona_html:to_html(Value),
    [$\s, KeyBin, $=, $", ValueHTML, $"];
attr_to_html(Key) when is_atom(Key) ->
    % Boolean attribute
    [$\s, atom_to_binary(Key)].

%% Convert children list to HTML
children_to_html(Children) ->
    [child_to_html(Child) || Child <- Children].

%% Convert single child to HTML
child_to_html({_Tag, _Attrs, _Children} = Element) ->
    % Nested element
    element_to_html(Element);
child_to_html(Child) ->
    arizona_html:to_html(Child).

%% Build HTML element from prepared parts
build_element_html(TagBin, AttrsHTML, ChildrenHTML) ->
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
    ].

%% Build void HTML element (no closing tag)
build_void_element_html(TagBin, AttrsHTML) ->
    [$<, TagBin, AttrsHTML, $>].

%% Check if element is a void element (no closing tag)
is_void_element(area) -> true;
is_void_element(base) -> true;
is_void_element(br) -> true;
is_void_element(col) -> true;
is_void_element(embed) -> true;
is_void_element(hr) -> true;
is_void_element(img) -> true;
is_void_element(input) -> true;
is_void_element(link) -> true;
is_void_element(meta) -> true;
is_void_element(param) -> true;
is_void_element(source) -> true;
is_void_element(track) -> true;
is_void_element(wbr) -> true;
is_void_element(_) -> false.

%% Convert single element AST to HTML
element_ast_to_html(ElementAST) ->
    case extract_element_tuple(ElementAST) of
        {ok, Tag, AttrsAST, ChildrenAST} ->
            TagBin = atom_to_binary(Tag),
            AttrsHTML = attrs_ast_to_html(AttrsAST),
            case is_void_element(Tag) of
                true ->
                    build_void_element_html(TagBin, AttrsHTML);
                false ->
                    ChildrenHTML = children_ast_to_html(ChildrenAST),
                    build_element_html(TagBin, AttrsHTML, ChildrenHTML)
            end;
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

%% Convert attributes list AST to HTML
attrs_ast_to_html({nil, _}) ->
    [];
attrs_ast_to_html({cons, _, Attr, Rest}) ->
    AttrHTML = attr_ast_to_html(Attr),
    RestHTML = attrs_ast_to_html(Rest),
    [AttrHTML | RestHTML].

%% Convert single attribute AST to HTML
attr_ast_to_html({tuple, _, [{atom, _, Key}, Value]}) ->
    KeyBin = atom_to_binary(Key),
    ValueHTML = attr_value_ast_to_html(Value),
    [$\s, KeyBin, $=, $", ValueHTML, $"];
attr_ast_to_html({atom, _, Key}) ->
    % Boolean attribute
    [$\s, atom_to_binary(Key)].

%% Convert attribute value AST to HTML
attr_value_ast_to_html({bin, _, Elements}) ->
    extract_binary_value(Elements);
attr_value_ast_to_html({cons, _, _Head, _Tail} = List) ->
    % List of values (static + dynamic)
    list_ast_to_html(List);
attr_value_ast_to_html(ValueAST) ->
    % Dynamic expression
    ast_to_html_expr(ValueAST).

%% Convert children list AST to HTML
children_ast_to_html({nil, _}) ->
    [];
children_ast_to_html({cons, _, Child, Rest}) ->
    ChildHTML = child_ast_to_html(Child),
    RestHTML = children_ast_to_html(Rest),
    [ChildHTML | RestHTML].

%% Convert single child AST to HTML
child_ast_to_html({bin, _, Elements}) ->
    extract_binary_value(Elements);
child_ast_to_html(ChildAST) ->
    case extract_element_tuple(ChildAST) of
        {ok, _Tag, _Attrs, _Children} ->
            element_ast_to_html(ChildAST);
        error ->
            ast_to_html_expr(ChildAST)
    end.

%% Convert list AST to HTML
list_ast_to_html({nil, _}) ->
    [];
list_ast_to_html({cons, _, Head, Tail}) ->
    HeadHTML =
        case Head of
            {bin, _, Elements} -> extract_binary_value(Elements);
            _ -> ast_to_html_expr(Head)
        end,
    TailHTML = list_ast_to_html(Tail),
    [HeadHTML | TailHTML].

%% Extract static binary value from AST
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
