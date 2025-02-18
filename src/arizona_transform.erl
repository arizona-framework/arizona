-module(arizona_transform).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([transform/1]).
-export([transform_list/2]).

%

-ignore_xref([parse_transform/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec parse_transform(Forms0, Opts) -> Forms1 when
    Forms0 :: [erl_syntax:syntaxTree()],
    Opts :: list(),
    Forms1 :: [erl_syntax:syntaxTree()].
parse_transform(Forms0, _Opts) ->
    Forms = [transform_function(Form) || Form <- Forms0],
    % NOTE: Uncomment the function below for debugging.
    debug(Forms0, Forms),
    Forms.

transform({call, Pos1, {remote, Pos2, {atom, Pos3, Mod}, {atom, Pos4, Fun}}, Body}) ->
    {call, Pos1, {remote, Pos2, {atom, Pos3, Mod}, {atom, Pos4, Fun}},
        transform_fun_body(Mod, Fun, Body)};
transform({match, Pos, A, B}) ->
    {match, Pos, transform(A), transform(B)};
transform({clause, Pos, Pattern, Guards, Body}) ->
    {clause, Pos, Pattern, Guards, transform(Body)};
transform({'case', Pos, Cond, Clauses}) ->
    {'case', Pos, Cond, transform(Clauses)};
transform({'if', Pos, Clauses}) ->
    {'if', Pos, transform(Clauses)};
transform({map, Pos, Forms}) ->
    {map, Pos, transform(Forms)};
transform({tuple, Pos, Forms}) ->
    {tuple, Pos, transform(Forms)};
transform({cons, Pos, Form, Next}) ->
    {cons, Pos, transform(Form), transform(Next)};
transform(Forms) when is_list(Forms) ->
    [transform(Form) || Form <- Forms];
transform(Form) ->
    Form.

transform_list(Callback0, List) ->
    {'fun', Pos1, {clauses, [{clause, Pos2, Pattern, Guards, CallbackBody0}]}} = Callback0,
    [
        {call, _, {remote, _, {atom, _, arizona_render}, {atom, _, template}}, [TemplateAst]}
        | TRevBody
    ] = lists:reverse(CallbackBody0),
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, #{render_context => render}),
    CallbackBody = lists:reverse([Dynamic | TRevBody]),
    Callback = {'fun', Pos1, {clauses, [{clause, Pos2, Pattern, Guards, CallbackBody}]}},
    TemplateTuple = static_dynamic_tuple(Static, List),
    {Callback, TemplateTuple}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

% NOTE: Use this function to output the transformation to "/tmp/<module_name>.erl".
debug(Forms, NewForms) ->
    case lists:search(
        fun(Form) ->
            erl_syntax:type(Form) =:= attribute andalso
                erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= module
        end,
        Forms
    ) of
        {value, ModAttr} ->
    Mod = erl_syntax:atom_value(hd(erl_syntax:attribute_arguments(ModAttr))),
    Str = [erl_prettypr:format(Form, [{pape, 9999999}]) || Form <- NewForms],
    ok = file:write_file("/tmp/" ++ atom_to_list(Mod) ++ ".erl", Str);
        false ->
            ok
    end.

transform_function({function, Pos1, Name, Arity, [{clause, Pos2, Pattern, Guards, Body0}]}) ->
    Body = [transform(Form) || Form <- Body0],
    {function, Pos1, Name, Arity, [{clause, Pos2, Pattern, Guards, Body}]};
transform_function(Form) ->
    Form.

transform_fun_body(arizona_render, view_template, Body) ->
    [_View, TemplateAst] = Body,
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, #{}),
    TemplateTuple = static_dynamic_tuple(Static, Dynamic),
    [TemplateTuple];
transform_fun_body(arizona_render, component_template, Body) ->
    [_View, TemplateAst] = Body,
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, #{}),
    TemplateTuple = static_dynamic_tuple(Static, Dynamic),
    [TemplateTuple];
transform_fun_body(arizona_render, nested_template, Body) ->
    [_ParentView, TemplateAst] = Body,
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, #{render_context => render}),
    TemplateTuple = static_dynamic_tuple(Static, Dynamic),
    [TemplateTuple];
transform_fun_body(arizona_render, list, Body) ->
    [Callback0, List] = Body,
    {Callback, TemplateTuple} = transform_list(Callback0, List),
    ct:pal("~p~n", [{list, List, TemplateTuple}]),
    [Callback, TemplateTuple];
transform_fun_body(Mod, Fun, Body) ->
    ct:pal("[Other] ~p~n", [{Mod, Fun}]),
    Body.

% Returns {Static, Dynamic} as AST.
template_to_static_dynamic(TemplateAst, ParseOpts) ->
    ScanOpts = #{
        % Consider it a triple-quoted string that start one line below.
        % See https://www.erlang.org/eeps/eep-0064#triple-quoted-string-start
        line => line(TemplateAst) + 1,
        indentation => 4
    },
    Template = eval_template(TemplateAst),
    Tokens = arizona_scanner:scan(ScanOpts, Template),
    {StaticAst, DynamicAst} = arizona_parser:parse(Tokens, ParseOpts),
    Static = erl_syntax:list(StaticAst),
    Dynamic = erl_syntax:list(DynamicAst),
    {Static, Dynamic}.

static_dynamic_tuple(Static, Dynamic) ->
    erl_syntax:revert(erl_syntax:tuple([Static, Dynamic])).

line(Form) ->
    case erl_syntax:get_pos(Form) of
        Ln when is_integer(Ln) ->
            Ln;
        {Ln, _Col} when is_integer(Ln) ->
            Ln;
        Anno ->
            erl_anno:line(Anno)
    end.

eval_template(TemplateAst) ->
    {value, Template, _NewBindings} = erl_eval:exprs([TemplateAst], []),
    Template.
