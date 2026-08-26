-module(arizona_rawtext).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Init) ->
    Bindings = #{
        id => ~"rawtext",
        v => maps:get(v, Init, ~"A"),
        pre => maps:get(pre, Init, ~"X"),
        style => maps:get(style, Init, ~"#probe-style { color: rgb(1, 2, 3); }")
    },
    {Bindings, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {button, [{az_click, arizona_js:push_event(~"bump")}], [~"bump"]},
            {button, [{az_click, arizona_js:push_event(~"evil")}], [~"evil"]},
            {button, [{az_click, arizona_js:push_event(~"only_v")}], [~"only_v"]},
            %% control: an ordinary marker-anchored slot
            {p, [{class, ~"plain"}], [?get(v)]},
            %% sole dynamic in escapable raw text
            {textarea, [{class, ~"sole"}], [?get(v)]},
            %% static text + dynamic in one raw-text element
            {textarea, [{class, ~"mixed"}], [~"Hello ", ?get(v)]},
            %% two dynamics sharing one raw-text element
            {textarea, [{class, ~"two"}], [?get(pre), ~"-", ?get(v)]},
            %% escapable raw text: title (in body, but still RCDATA-classified)
            {title, [{class, ~"ti"}], [?get(v)]},
            %% raw raw-text: style, whose CSSOM does re-parse on content change
            {style, [], [?raw(?get(style))]},
            {span, [{id, ~"probe-style"}], [~"styled"]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"bump", _Payload, Bindings) ->
    {
        Bindings#{
            v => ~"B",
            pre => ~"Y",
            style => ~"#probe-style { color: rgb(9, 8, 7); }"
        },
        #{},
        []
    };
%% Change ONLY `v`; `pre` (the sibling dynamic sharing the .two textarea) and the
%% static "Hello " / "-" must survive the patch.
handle_event(~"only_v", _Payload, Bindings) ->
    {Bindings#{v => ~"Z"}, #{}, []};
handle_event(~"evil", _Payload, Bindings) ->
    {
        Bindings#{v => ~"a & b < c </textarea><img src=x onerror=window.__pwned=1>"},
        #{},
        []
    }.
