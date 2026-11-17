-module(arizona_patch_crash).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_update/3]).

%% A page whose patch reaction crashes on a `boom` query param (a bad link, a
%% malformed value), while a fresh mount of the same URL renders fine.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    Params = maps:get(params, Bindings, []),
    {#{id => ~"patch_crash", q => proplists:get_value(~"q", Params, ~"none")}, #{}}.

-spec handle_update(map(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Params, Bindings, Effects) ->
    P = maps:get(params, Params, []),
    case proplists:is_defined(~"boom", P) of
        true -> error(bad_param);
        false -> {Bindings#{q => proplists:get_value(~"q", P, ~"none")}, #{}, Effects}
    end.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [{id, ~"q"}], [?get(q)]},
            {form, [{id, ~"f"}], [{input, [{name, ~"t"}, {id, ~"t"}]}]}
        ]}
    ).
