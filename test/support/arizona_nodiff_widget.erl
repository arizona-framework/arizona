-module(arizona_nodiff_widget).
-include("arizona_stateful.hrl").
-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).

%% A client-owned widget (the `Widget` hook in `arizona_layout`) mounted in an
%% az-nodiff region. The server picks which region the slot holds; the widget owns
%% everything rendered inside it.

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"page",
        title => maps:get(title, Init, ~"NodiffWidget"),
        mode => ~"line",
        rev => 0,
        key => 0,
        count => 0
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {button, [{id, ~"line"}, {az_click, mode(~"line")}], [~"Line"]},
            {button, [{id, ~"bar"}, {az_click, mode(~"bar")}], [~"Bar"]},
            {button, [{id, ~"helper-a"}, {az_click, mode(~"helper_a")}], [~"Helper A"]},
            {button, [{id, ~"helper-b"}, {az_click, mode(~"helper_b")}], [~"Helper B"]},
            {button, [{id, ~"hide"}, {az_click, mode(~"none")}], [~"Hide"]},
            {button, [{id, ~"rerun"}, {az_click, arizona_js:push_event(~"rerun")}], [~"Rerun"]},
            {button, [{id, ~"rekey"}, {az_click, arizona_js:push_event(~"rekey")}], [~"Rekey"]},
            {button, [{id, ~"bump"}, {az_click, arizona_js:push_event(~"bump")}], [~"Bump"]},
            {p, [{id, ~"count"}], [?get(count)]},
            {'div', [{id, ~"slot"}], [
                case {?get(mode), ?get(rev)} of
                    {~"line", _} ->
                        {'div',
                            [
                                'az-nodiff',
                                {az_hook, ~"Widget"},
                                {data_kind, ~"line"},
                                {az_key, ?get(key)}
                            ],
                            [?get(rev)]};
                    {~"bar", _} ->
                        {'div',
                            [
                                'az-nodiff',
                                {az_hook, ~"Widget"},
                                {data_kind, ~"bar"},
                                {az_key, ?get(key)}
                            ],
                            [?get(rev)]};
                    {~"helper_a", _} ->
                        widget(?get(rev));
                    {~"helper_b", _} ->
                        widget(?get(rev));
                    _ ->
                        <<>>
                end
            ]}
        ]}
    ).

widget(Rev) ->
    {'div', ['az-nodiff', {az_hook, ~"Widget"}, {data_kind, ~"helper"}], [Rev]}.

mode(Mode) ->
    arizona_js:push_event(~"mode", #{~"mode" => Mode}).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"mode", #{~"mode" := Mode}, Bindings) ->
    {Bindings#{mode => Mode}, #{}, []};
handle_event(~"rerun", _Payload, #{rev := Rev} = Bindings) ->
    {Bindings#{rev => Rev + 1}, #{}, []};
handle_event(~"rekey", _Payload, #{key := Key} = Bindings) ->
    {Bindings#{key => Key + 1}, #{}, []};
handle_event(~"bump", _Payload, #{count := Count} = Bindings) ->
    {Bindings#{count => Count + 1}, #{}, []}.
