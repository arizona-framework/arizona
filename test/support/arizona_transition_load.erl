-module(arizona_transition_load).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_info/2]).
-export([handle_update/3]).

%% With `auto`, the view patches to `patch` once connected, with no user input, and
%% answers the patch with the full load of `target` inside a view transition: the
%% load runs in the transition's update callback, after the reply has been applied.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"transition_load",
        patch => maps:get(patch, Init, ~"/"),
        target => maps:get(target, Init, ~"/"),
        patched => false
    },
    case maps:get(auto, Init, false) andalso ?connected of
        true ->
            {arizona_view, _, _} = ?send(go),
            ok;
        false ->
            ok
    end,
    {Bindings, #{}}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(go, Bindings) ->
    {Bindings, #{}, [arizona_js:patch(maps:get(patch, Bindings))]}.

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(_Params, Bindings, Effects) ->
    Load = arizona_js:navigate(maps:get(target, Bindings), #{full => true}),
    {Bindings#{patched => true}, #{}, Effects ++ [arizona_js:transition(Load)]}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [{id, ~"patched"}], [
                case ?get(patched) of
                    true -> ~"patched";
                    false -> ~"mounted"
                end
            ]}
        ]}
    ).
