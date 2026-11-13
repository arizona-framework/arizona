-module(arizona_pip_form).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

%% A form around a poppable child holding a file input: popped out, the child's
%% fields are still the form's, so its submit carries the chosen file.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {#{id => maps:get(id, Bindings, ~"pip_form")}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {form, [{id, ~"pf"}, {az_submit, arizona_js:push_event(~"sent")}], [
                {input, [{name, ~"own"}, {value, ~"1"}]},
                ?stateful(arizona_pip_file, #{id => ~"pip_file"}),
                {button, [{type, ~"submit"}], [~"Send"]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"sent", _Payload, Bindings) ->
    {Bindings, #{}, []}.
