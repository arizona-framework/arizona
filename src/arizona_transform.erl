-module(arizona_transform).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([transform/2]).

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

-spec transform(Form0, Bindings) -> Form1 when
    Form0 :: erl_syntax:syntaxTree(),
    Bindings :: erl_eval:binding_struct(),
    Form1 :: erl_syntax:syntaxTree().
transform(
    {call, _Pos1, {remote, _Pos2, {atom, _Pos3, Mod}, {atom, _Pos4, Fun}}, Body} = Form,
    Bindings
) ->
    case transform_fun_body(Mod, Fun, Body, Bindings) of
        {true, NewForm} ->
            NewForm;
        false ->
            Form
    end;
transform({match, Pos, A, B}, Bindings) ->
    {match, Pos, transform(A, Bindings), transform(B, Bindings)};
transform({clause, Pos, Pattern, Guards, Body}, Bindings) ->
    {clause, Pos, Pattern, Guards, transform(Body, Bindings)};
transform({'case', Pos, Cond, Clauses}, Bindings) ->
    {'case', Pos, Cond, transform(Clauses, Bindings)};
transform({'if', Pos, Clauses}, Bindings) ->
    {'if', Pos, transform(Clauses, Bindings)};
transform({map, Pos, Forms}, Bindings) ->
    {map, Pos, transform(Forms, Bindings)};
transform({tuple, Pos, Forms}, Bindings) ->
    {tuple, Pos, transform(Forms, Bindings)};
transform({cons, Pos, Form, Next}, Bindings) ->
    {cons, Pos, transform(Form, Bindings), transform(Next, Bindings)};
transform({'fun', Pos, {clauses, Clauses}}, Bindings) ->
    {'fun', Pos, {clauses, transform(Clauses, Bindings)}};
transform(Forms, Bindings) when is_list(Forms) ->
    [transform(Form, Bindings) || Form <- Forms];
transform(Form, _Bindings) ->
    Form.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

% NOTE: Use this function to output the transformation to "/tmp/<module_name>.erl".
% debug(Forms, NewForms) ->
%     case
%         lists:search(
%             fun(Form) ->
%                 erl_syntax:type(Form) =:= attribute andalso
%                     erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= module
%             end,
%             Forms
%         )
%     of
%         {value, ModAttr} ->
%             Mod = erl_syntax:atom_value(hd(erl_syntax:attribute_arguments(ModAttr))),
%             Str = [erl_prettypr:format(Form, [{pape, 9999999}]) || Form <- NewForms],
%             ok = file:write_file("/tmp/" ++ atom_to_list(Mod) ++ ".erl", Str);
%         false ->
%             ok
%     end.

transform_function({function, Pos1, Name, Arity, [{clause, Pos2, Pattern, Guards, Body0}]}) ->
    Body = [transform(Form, []) || Form <- Body0],
    {function, Pos1, Name, Arity, [{clause, Pos2, Pattern, Guards, Body}]};
transform_function(Form) ->
    Form.

transform_fun_body(arizona_render, view_template, Body, Bindings) ->
    [_View, TemplateAst] = Body,
    ParseOpts = #{},
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, Bindings, ParseOpts),
    Token = token(view_template, [Static, Dynamic]),
    {true, Token};
transform_fun_body(arizona_render, component_template, Body, Bindings) ->
    [_View, TemplateAst] = Body,
    ParseOpts = #{},
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, Bindings, ParseOpts),
    Token = token(component_template, [Static, Dynamic]),
    {true, Token};
transform_fun_body(arizona_render, nested_template, Body, Bindings) ->
    [_ParentView, TemplateAst] = Body,
    ParseOpts = #{render_context => render},
    {Static, Dynamic} = template_to_static_dynamic(TemplateAst, Bindings, ParseOpts),
    Token = token(nested_template, [Static, Dynamic]),
    {true, Token};
transform_fun_body(arizona_render, list, Body, Bindings) ->
    [Callback, List] = Body,
    {Static, Dynamic} = callback_to_static_dynamic(Callback, Bindings),
    Token = token(list_template, [Static, Dynamic, List]),
    {true, Token};
transform_fun_body(_Mod, _Fun, _Body, _Bindings) ->
    false.

callback_to_static_dynamic(Callback0, Bindings) ->
    {'fun', Pos1,
        {clauses, [
            {clause, Pos2, Pattern, Guards, [
                {call, Pos3, {remote, _, {atom, _, arizona_render}, {atom, _, nested_template}}, [
                    _Payload, TemplateAst
                ]}
            ]}
        ]}} = Callback0,
    ParseOpts = #{render_context => none},
    {Static, DynamicList0} = template_to_static_dynamic(TemplateAst, Bindings, ParseOpts),
    DynamicList = erl_syntax:set_pos(DynamicList0, Pos3),
    Callback =
        {'fun', Pos1,
            {clauses, [
                {clause, Pos2, Pattern, Guards, [DynamicList]}
            ]}},
    {Static, Callback}.

template_to_static_dynamic(TemplateAst, Bindings, ParseOpts) ->
    ScanOpts = #{
        % Consider it a triple-quoted string that start one line below.
        % See https://www.erlang.org/eeps/eep-0064#triple-quoted-string-start
        line => line(TemplateAst) + 1,
        % TODO: Indentation option
        indentation => 4
    },
    Template = eval_template(TemplateAst, Bindings),
    Tokens = arizona_scanner:scan(ScanOpts, Template),
    {StaticAst, DynamicAst} = arizona_parser:parse(Tokens, ParseOpts),
    Static = erl_syntax:list(StaticAst),
    Dynamic = erl_syntax:list(DynamicAst),
    {Static, Dynamic}.

line(Form) ->
    case erl_syntax:get_pos(Form) of
        Ln when is_integer(Ln) ->
            Ln;
        {Ln, _Col} when is_integer(Ln) ->
            Ln;
        Anno ->
            erl_anno:line(Anno)
    end.

eval_template(TemplateAst, Bindings) ->
    {value, Template, _NewBindings} = erl_eval:exprs([TemplateAst], Bindings),
    Template.

token(Name, Params) when is_atom(Name), is_list(Params) ->
    erl_syntax:revert(erl_syntax:tuple([erl_syntax:atom(Name) | Params])).
