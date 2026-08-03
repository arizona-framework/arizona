-module(arizona_child_stream_root).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% Root embedding arizona_child_stream and feeding it a `label` prop, so a root
%% event re-evaluates the child slot (the child's props read `label`). That is
%% the first root diff after the child patched its own stream, and it is where a
%% root snapshot left holding the child's PRE-event snapshot re-renders the
%% container the child just patched incrementally.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"csr", label => ~"L0"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            ?stateful(arizona_child_stream, #{id => ~"cs", label => ?get(label)})
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"relabel", #{~"label" := Label}, Bindings) ->
    {Bindings#{label => Label}, #{}, []}.
