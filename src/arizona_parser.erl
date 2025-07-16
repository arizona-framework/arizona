-module(arizona_parser).
-moduledoc ~"""
Template parser for Arizona Web Framework.

Converts tokenized Arizona templates into structured records optimized for rendering.
Processes tokens from `arizona_scanner` and separates static content from dynamic expressions.

## Output Structure

```erlang
#parsed_template{
    static = [<<"<div>">>, <<" - ">>, <<"</div>">>],
    dynamic = [{1, <<"name">>}, {1, <<"age">>}]
}
```

For template `<div>{name} - {age}</div>`:
- **static**: Binary segments in template order
- **dynamic**: Expression tuples with line numbers

## Features

- **High Performance**: Records and tuples for memory efficiency
- **Separated Content**: Static and dynamic parts split for fast rendering
- **Line Tracking**: Preserves source locations for debugging
- **Comment Filtering**: Removes comment tokens
- **Opaque Type**: Encapsulated API design

## Examples

- Static: `Hello World` → `#parsed_template{static = [<<"Hello World">>], dynamic = []}`
- Dynamic: `{name}` → `#parsed_template{static = [<<"">>], dynamic = [{1, <<"name">>}]}`
- Mixed: `Hi {name}!` → `#parsed_template{static = [<<"Hi ">>, <<"!">>],
  dynamic = [{1, <<"name">>}]}`

The parser provides the foundation for Arizona's template rendering system.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_tokens/1]).
-export([static/1]).
-export([dynamic/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([parsed_template/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(parsed_template, {
    static :: [StaticContent :: binary()],
    dynamic :: [{Line :: pos_integer(), ExprText :: binary()}]
}).

-doc ~"""
Template parsing result with separated static and dynamic content.

Static parts are binary segments in template order.
Dynamic parts are expression tuples with line information.
Parse transform handles variable analysis separately.
""".
-opaque parsed_template() :: #parsed_template{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Parse tokens into template structure.

Converts tokens into a record with separated static and dynamic parts.

Returns:
- `static`: Binary segments in template order
- `dynamic`: Expression tuples with line numbers

Parse transform handles variable analysis.
""".
-spec parse_tokens(Tokens) -> ParsedTemplate when
    Tokens :: [arizona_scanner:token()],
    ParsedTemplate :: parsed_template().
parse_tokens(Tokens) ->
    {StaticParts, DynamicElements} = separate_static_dynamic(Tokens),
    #parsed_template{
        static = StaticParts,
        dynamic = DynamicElements
    }.

-doc ~"""
Get static content from parsed template.

Returns the list of static binary segments in template order.
""".
-spec static(ParsedTemplate) -> [binary()] when
    ParsedTemplate :: parsed_template().
static(#parsed_template{static = Static}) ->
    Static.

-doc ~"""
Get dynamic content from parsed template.

Returns the list of dynamic expression tuples with line numbers.
""".
-spec dynamic(ParsedTemplate) -> [{pos_integer(), binary()}] when
    ParsedTemplate :: parsed_template().
dynamic(#parsed_template{dynamic = Dynamic}) ->
    Dynamic.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Separate static and dynamic parts
separate_static_dynamic(Tokens) ->
    separate_static_dynamic(Tokens, [], [], undefined).

separate_static_dynamic([], StaticAcc, DynamicAcc, _PrevType) ->
    {lists:reverse(StaticAcc), lists:reverse(DynamicAcc)};
separate_static_dynamic(
    [{static, _Line, Text} | Rest], StaticAcc, DynamicAcc, _PrevType
) ->
    separate_static_dynamic(Rest, [Text | StaticAcc], DynamicAcc, static);
separate_static_dynamic(
    [{dynamic, Line, ExprText} | Rest], StaticAcc, DynamicAcc, PrevType
) ->
    %% Store dynamic element - parse transform will handle variable analysis
    NewDynamicAcc = [{Line, ExprText} | DynamicAcc],

    %% Add empty static part when:
    %% 1. First token is dynamic (PrevType == undefined)
    %% 2. Previous token was also dynamic (PrevType == dynamic)
    NewStaticAcc =
        case PrevType of
            % First token is dynamic
            undefined -> [~"" | StaticAcc];
            % Consecutive dynamics
            dynamic -> [~"" | StaticAcc];
            % After static, no empty needed
            static -> StaticAcc
        end,

    separate_static_dynamic(Rest, NewStaticAcc, NewDynamicAcc, dynamic);
separate_static_dynamic(
    [{comment, _Line, _Text} | Rest], StaticAcc, DynamicAcc, PrevType
) ->
    %% Skip comments - preserve previous type
    separate_static_dynamic(Rest, StaticAcc, DynamicAcc, PrevType).
