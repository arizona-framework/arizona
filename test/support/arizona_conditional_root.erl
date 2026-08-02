%% Templates whose WHOLE body is a bare dynamic -- a root conditional or a
%% root child descriptor, with no enclosing element. Before the root-slot
%% anchoring fix, such a template compiled to empty statics with no marker,
%% so SSR anchored nothing for the root slot and its diff ops targeted an az
%% that existed nowhere in the DOM (the client drops such ops): a
%% conditional-only `?stateless` banner could neither appear nor disappear.
-module(arizona_conditional_root).
-include("arizona_stateless.hrl").
-export([page/1]).
-export([notice/1]).
-export([nested_page/1]).
-export([embed/1]).
-export([deep/1]).

%% Parent embedding the conditional-root child in a normal content slot.
-spec page(az:bindings()) -> az:template().
page(Bindings) ->
    ?html(
        {main, [{id, ~"app"}], [
            {h1, [], [~"Title"]},
            ?stateless(arizona_conditional_root, notice, #{notice => ?get(notice)})
        ]}
    ).

%% The probe child: the whole template is a bare conditional.
-spec notice(az:bindings()) -> az:template().
notice(Bindings) ->
    ?html(
        case ?get(notice) of
            none -> <<>>;
            N -> {p, [{class, ~"notice"}], [N]}
        end
    ).

%% Deeper nesting: the embedded child's whole template is ANOTHER child's
%% descriptor, whose whole template is a bare conditional -- two root slots
%% deep, each needing its own anchor.
-spec nested_page(az:bindings()) -> az:template().
nested_page(Bindings) ->
    ?html(
        {section, [{id, ~"wrap"}], [
            ?stateless(arizona_conditional_root, embed, #{hint => ?get(hint)})
        ]}
    ).

-spec embed(az:bindings()) -> az:template().
embed(Bindings) ->
    ?html(?stateless(arizona_conditional_root, deep, #{hint => ?get(hint)})).

-spec deep(az:bindings()) -> az:template().
deep(Bindings) ->
    ?html(
        case ?get(hint) of
            none -> <<>>;
            H -> {em, [], [H]}
        end
    ).
