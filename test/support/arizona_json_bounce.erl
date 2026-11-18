-module(arizona_json_bounce).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_info/2]).
-export([unmount/1]).

%% Two ?json pages that navigate to each other on connect and crash as the
%% navigate unmounts them: every navigate fails over the socket and is answered
%% with a full navigate, which mounts fine on a fresh connection.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{id => ~"json_bounce", other => maps:get(other, Init)},
    ?connected andalso ?send(bounce),
    {Bindings, #{}}.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info(bounce, Bindings) ->
    {Bindings, #{}, [arizona_android:navigate(maps:get(other, Bindings))]}.

-spec unmount(az:bindings()) -> no_return().
unmount(_Bindings) ->
    error(crash_on_leave).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json({'Column', [{id, ?get(id)}], [{'Text', [], [?get(other)]}]}).
