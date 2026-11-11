-module(arizona_select_value).
-moduledoc """
Templates whose `<select>` carries a `value`: one per shape an option can reach the
select in (written literally, inside an `optgroup`, in a conditional branch, from a
list or stream `?each`, from a named `?each` callback, from an inlined local helper,
through a hoisted variable), plus `multiple`. Every one must render the option whose
value equals the select's as `selected`, and diff a change of it to that option.

The rest hold options the transform cannot see -- a component, a helper it does not
inline, a runtime template, raw markup, an option valued by runtime text -- so the
select keeps its value and the page marks the option it names.
""".
-include("arizona_stateless.hrl").

-export([literal/1]).
-export([optgroup/1]).
-export([conditional/1]).
-export([each_list/1]).
-export([each_stream/1]).
-export([each_named/1]).
-export([helper/1]).
-export([helper_each/1]).
-export([helper_returns_each/1]).
-export([helper_arg_each/1]).
-export([helper_arg_each_qualified/1]).
-export([field/1]).
-export([nodiff/1]).
-export([spelled/1]).
-export([local/1]).
-export([hoisted/1]).
-export([text_value/1]).
-export([multiple/1]).
-export([multiple_dynamic/1]).
-export([nested_each/1]).
-export([component/1]).
-export([local_component/1]).
-export([remote_helper/1]).
-export([runtime_template/1]).
-export([raw_options/1]).
-export([text_options/1]).
-export([each_opaque_item/1]).
-export([mixed/1]).
-export([multiple_component/1]).
-export([multiple_component_dynamic/1]).
-export([local_value_component/1]).
-export([option_items/1]).

-spec literal(az:bindings()) -> az:template().
literal(Bindings) ->
    ?html(
        {select, [{name, ~"s"}, {value, ?get(v)}], [
            {option, [{value, ~"a"}], [~"A"]},
            {option, [{value, ~"b"}], [~"B"]},
            {option, [{value, ~"c"}], [~"C"]}
        ]}
    ).

-spec optgroup(az:bindings()) -> az:template().
optgroup(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            {optgroup, [{label, ~"First"}], [
                {option, [{value, ~"a"}], [~"A"]},
                {option, [{value, ~"b"}], [~"B"]}
            ]},
            {optgroup, [{label, ~"Second"}], [{option, [{value, ~"c"}], [~"C"]}]}
        ]}
    ).

-spec conditional(az:bindings()) -> az:template().
conditional(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            {option, [{value, ~"a"}], [~"A"]},
            case ?get(more) of
                true -> [{option, [{value, ~"b"}], [~"B"]}, {option, [{value, ~"c"}], [~"C"]}];
                false -> <<>>
            end
        ]}
    ).

-spec each_list(az:bindings()) -> az:template().
each_list(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts))
        ]}
    ).

-spec each_stream(az:bindings()) -> az:template().
each_stream(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?each(
                fun(O, Key) ->
                    {option, [{az_key, Key}, {value, arizona_template:get(id, O)}], [
                        arizona_template:get(label, O)
                    ]}
                end,
                ?get(items)
            )
        ]}
    ).

-spec each_named(az:bindings()) -> az:template().
each_named(Bindings) ->
    ?html({select, [{value, ?get(v)}], [?each(fun option/1, ?get(opts))]}).

option(O) ->
    {option, [{value, O}], [O]}.

-spec helper(az:bindings()) -> az:template().
helper(Bindings) ->
    ?html({select, [{value, ?get(v)}], [opt(~"a"), opt(~"b"), opt(~"c")]}).

opt(V) ->
    {option, [{value, V}], [V]}.

-spec helper_each(az:bindings()) -> az:template().
helper_each(Bindings) ->
    ?html({select, [{value, ?get(v)}], [?each(fun(O) -> labeled(O, O) end, ?get(opts))]}).

labeled(V, Label) ->
    {option, [{value, V}], [Label]}.

-spec helper_returns_each(az:bindings()) -> az:template().
helper_returns_each(Bindings) ->
    ?html({select, [{value, ?get(v)}], [options(?get(opts))]}).

options(Opts) ->
    ?each(fun(O) -> {option, [{value, O}], [O]} end, Opts).

%% The `?each` is handed to a helper that puts it under the select.
-spec helper_arg_each(az:bindings()) -> az:template().
helper_arg_each(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            group(~"G", ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts)))
        ]}
    ).

group(Label, Options) ->
    {optgroup, [{label, Label}], [Options]}.

%% The same helper called as `?MODULE:group/2`, which resolves like a bare local call.
-spec helper_arg_each_qualified(az:bindings()) -> az:template().
helper_arg_each_qualified(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?MODULE:group(~"G", ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts)))
        ]}
    ).

-spec nodiff(az:bindings()) -> az:template().
nodiff(Bindings) ->
    ?html(
        {select, ['az-nodiff', {value, ?get(v)}], [
            ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts))
        ]}
    ).

%% Tag and attribute names as the browser compares them: ASCII case-insensitive.
-spec spelled(az:bindings()) -> az:template().
spelled(Bindings) ->
    ?html(
        {~"SELECT", [{<<"Value">>, ?get(v)}], [
            {'OPTION', [{'VALUE', ~"a"}], [~"A"]},
            {~"Option", [{value, ~"b"}], [~"B"]},
            {option, [{value, ~"c"}], [~"C"]}
        ]}
    ).

%% A client-owned value: the page loads on its initial, the client moves it.
-spec local(az:bindings()) -> az:template().
local(Bindings) ->
    ?html(
        {select, [{name, ?get(name, ~"choice")}, {value, ?local(choice, ~"b")}], [
            {option, [{value, ~"a"}], [~"A"]},
            {option, [{value, ~"b"}], [~"B"]}
        ]}
    ).

%% The select itself comes from a helper, its options from the helper's own `?each`.
-spec field(az:bindings()) -> az:template().
field(Bindings) ->
    ?html({form, [], [select_field(?get(v), ?get(opts))]}).

select_field(Value, Opts) ->
    {select, [{value, Value}], [?each(fun(O) -> {option, [{value, O}], [O]} end, Opts)]}.

-spec hoisted(az:bindings()) -> az:template().
hoisted(Bindings) ->
    V = ?get(v),
    Opts = ?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts)),
    ?html({select, [{value, V}], [Opts]}).

-spec text_value(az:bindings()) -> az:template().
text_value(Bindings) ->
    ?html({select, [{value, ?get(v)}], [{option, [], [~" a "]}, {option, [], [~"b"]}]}).

-spec multiple(az:bindings()) -> az:template().
multiple(Bindings) ->
    ?html(
        {select, [multiple, {value, ?get(vs)}], [
            {option, [{value, ~"a"}], [~"A"]},
            {option, [{value, ~"b"}], [~"B"]},
            {option, [{value, ~"c"}], [~"C"]}
        ]}
    ).

-spec multiple_dynamic(az:bindings()) -> az:template().
multiple_dynamic(Bindings) ->
    ?html(
        {select, [{multiple, ?get(many)}, {value, ?get(vs)}], [
            {option, [{value, ~"a"}], [~"A"]},
            {option, [{value, ~"b"}], [~"B"]}
        ]}
    ).

-spec nested_each(az:bindings()) -> az:template().
nested_each(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?each(
                fun({Label, Opts}) ->
                    {optgroup, [{label, Label}], [
                        ?each(fun(O) -> {option, [{value, O}], [O]} end, Opts)
                    ]}
                end,
                ?get(groups)
            )
        ]}
    ).

%% A component renders the options: a `?stateless` from another module, and a local one.
-spec component(az:bindings()) -> az:template().
component(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?stateless(arizona_select_value, option_items, #{opts => ?get(opts)})
        ]}
    ).

-spec local_component(az:bindings()) -> az:template().
local_component(Bindings) ->
    ?html({select, [{value, ?get(v)}], [?stateless(fun option_items/1, #{opts => ?get(opts)})]}).

-spec option_items(az:bindings()) -> az:template().
option_items(Bindings) ->
    ?html([?each(fun(O) -> {option, [{value, O}], [O]} end, ?get(opts))]).

%% A helper with two clauses is not inlined: its template is built at runtime.
-spec remote_helper(az:bindings()) -> az:template().
remote_helper(Bindings) ->
    ?html({select, [{value, ?get(v)}], [option_list(?get(opts))]}).

option_list([]) ->
    ?html([]);
option_list(Opts) ->
    ?html([?each(fun(O) -> {option, [{value, O}], [O]} end, Opts)]).

%% The options are a template the view holds as a binding.
-spec runtime_template(az:bindings()) -> az:template().
runtime_template(Bindings) ->
    ?html({select, [{value, ?get(v)}], [?get(options_tmpl)]}).

-spec raw_options(az:bindings()) -> az:template().
raw_options(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?raw(~"<option value=\"a\">A</option><option>b</option><OPTION Value='c'>C")
        ]}
    ).

%% Valued by their text, which is only known at runtime.
-spec text_options(az:bindings()) -> az:template().
text_options(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?each(fun(O) -> {option, [], [O]} end, ?get(opts))
        ]}
    ).

-spec each_opaque_item(az:bindings()) -> az:template().
each_opaque_item(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            ?each(
                fun(O) ->
                    {optgroup, [{label, O}], [?stateless(fun option_items/1, #{opts => [O]})]}
                end,
                ?get(opts)
            )
        ]}
    ).

%% Options the transform marks beside ones a component renders.
-spec mixed(az:bindings()) -> az:template().
mixed(Bindings) ->
    ?html(
        {select, [{value, ?get(v)}], [
            {option, [{value, ~"a"}], [~"A"]},
            ?stateless(fun option_items/1, #{opts => lists:delete(~"a", ?get(opts))})
        ]}
    ).

-spec multiple_component(az:bindings()) -> az:template().
multiple_component(Bindings) ->
    ?html(
        {select, [multiple, {value, ?get(vs)}], [
            ?stateless(fun option_items/1, #{opts => ?get(opts)})
        ]}
    ).

%% `multiple` decided at runtime, under a select whose options a component renders.
-spec multiple_component_dynamic(az:bindings()) -> az:template().
multiple_component_dynamic(Bindings) ->
    ?html(
        {select, [{multiple, ?get(many)}, {value, ?get(vs)}], [
            ?stateless(fun option_items/1, #{opts => ?get(opts)})
        ]}
    ).

-spec local_value_component(az:bindings()) -> az:template().
local_value_component(Bindings) ->
    ?html(
        {select, [{value, ?local(choice, ~"b")}], [
            ?stateless(fun option_items/1, #{opts => ?get(opts)})
        ]}
    ).
