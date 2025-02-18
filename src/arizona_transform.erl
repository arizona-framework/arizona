-module(arizona_transform).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).

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
    % debug(Forms0, Forms),
    Forms.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

% NOTE: Use this function to output the transformation to "/tmp/<module_name>.erl".
% debug(Forms, NewForms) ->
%     {value, ModAttr} = lists:search(
%         fun(Form) ->
%             erl_syntax:type(Form) =:= attribute andalso
%                 erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= module
%         end,
%         Forms
%     ),
%     Mod = erl_syntax:atom_value(hd(erl_syntax:attribute_arguments(ModAttr))),
%     Str = [erl_prettypr:format(Form, [{pape, 9999999}]) || Form <- NewForms],
%     ok = file:write_file("/tmp/" ++ atom_to_list(Mod) ++ ".erl", Str).

transform_function({function, Pos1, Name, Arity, [{clause, Pos2, Pattern, Guards, Body0}]}) ->
    Body = [transform(Form) || Form <- Body0],
    {function, Pos1, Name, Arity, [{clause, Pos2, Pattern, Guards, Body}]};
transform_function(Form) ->
    Form.

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

transform_fun_body(arizona_render, view_template, Body) ->
    [_View, TemplateAst] = Body,
    [template_to_static_dynamic(TemplateAst)];
transform_fun_body(arizona_render, component_template, Body) ->
    [_View, TemplateAst] = Body,
    [template_to_static_dynamic(TemplateAst)];
transform_fun_body(arizona_render, nested_template, Body) ->
    [_ParentView, TemplateAst] = Body,
    [template_to_static_dynamic(TemplateAst)];
transform_fun_body(_Mod, _Fun, Body) ->
    Body.

% Returns {Static, Dynamic} as AST.
template_to_static_dynamic(TemplateAst) ->
    ScanOpts = #{
        % Consider it a triple-quoted string that start one line below.
        % See https://www.erlang.org/eeps/eep-0064#triple-quoted-string-start
        line => line(TemplateAst) + 1,
        indentation => 4
    },
    Template = eval_template(TemplateAst),
    Tokens = arizona_scanner:scan(ScanOpts, Template),
    {Static, Dynamic} = arizona_parser:parse(Tokens),
    StaticList = erl_syntax:list(Static),
    DynamicList = erl_syntax:list(Dynamic),
    erl_syntax:revert(erl_syntax:tuple([StaticList, DynamicList])).

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
