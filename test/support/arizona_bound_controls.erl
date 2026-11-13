-module(arizona_bound_controls).
-moduledoc """
**TEST FIXTURE.** Server-owned form controls (`az-bound`) for the real-browser e2e
(`e2e/parallel/arizona_bound_controls.spec.js`).

Each control renders a server binding and proposes its edits through its own event
(or its form's), and each handler answers synchronously: some accept, some normalise,
some refuse and keep the old value. `#seq` counts handled events, so a spec waits on
it to know a proposal's reply has been applied. `#free` and `#deb` answer slowly,
so a reply arrives while the user is still typing. `#quiet` has no event at all, so it
is never reconciled; `#pick`, `#rpick`, `#fpick` and `#hpick` mark no option
selected, so each shows its first enabled one; `#prepend` patches to `?head=1`, which
inserts an option before `#hpick`'s first, and a reconnect remounts with it; `#plain`
and `#loc` are client-owned (no value / a `?local`).
""".
-include("arizona_stateful.hrl").

-export([mount/1]).
-export([render/1]).
-export([handle_event/3]).
-export([handle_update/3]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    {
        #{
            id => ~"page",
            title => maps:get(title, Bindings, ~"Bound"),
            seq => 0,
            digits => ~"12",
            upper => <<>>,
            trimmed => <<>>,
            free => ~"seed",
            num => <<>>,
            date => ~"2026-12-25",
            strict_date => ~"2026-12-25",
            agree => false,
            ctl => false,
            size1 => ~"m",
            size2 => ~"a",
            size3 => ~"x",
            color => ~"red",
            multi => [~"x"],
            pick => <<>>,
            rpick => <<>>,
            fpick => <<>>,
            hpick => <<>>,
            head => head(Bindings),
            notes => ~"first",
            deb => <<>>,
            quiet => <<>>,
            rbound => ~"server",
            revent => ~"server"
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [?get(title)]},
            {p, [], [~"seq: ", {span, [{id, ~"seq"}], [?get(seq)]}]},
            {input, [
                {id, ~"digits"}, {value, ?get(digits)}, {az_input, arizona_js:push_event(~"digits")}
            ]},
            {input, [
                {id, ~"upper"}, {value, ?get(upper)}, {az_input, arizona_js:push_event(~"upper")}
            ]},
            {input, [
                {id, ~"trimmed"},
                {value, ?get(trimmed)},
                {az_change, arizona_js:push_event(~"trimmed")}
            ]},
            {input, [
                {id, ~"free"}, {value, ?get(free)}, {az_input, arizona_js:push_event(~"free")}
            ]},
            {input, [
                {id, ~"num"},
                {type, ~"number"},
                {value, ?get(num)},
                {az_input, arizona_js:push_event(~"num")}
            ]},
            {input, [
                {id, ~"date"},
                {type, ~"date"},
                {value, ?get(date)},
                {az_input, arizona_js:push_event(~"date")}
            ]},
            {input, [
                {id, ~"strict-date"},
                {type, ~"date"},
                {value, ?get(strict_date)},
                {az_input, arizona_js:push_event(~"strict_date")}
            ]},
            {input, [
                {id, ~"deb"},
                {value, ?get(deb)},
                {'az-debounce', ~"40"},
                {az_input, arizona_js:push_event(~"deb")}
            ]},
            {p, [{id, ~"deb-echo"}], [?get(deb)]},
            {input, [
                {id, ~"agree"},
                {type, ~"checkbox"},
                {checked, ?get(agree)},
                {az_change, arizona_js:push_event(~"agree")}
            ]},
            {input, [
                {id, ~"ctl"},
                {type, ~"checkbox"},
                {checked, ?get(ctl)},
                'az-prevent-default',
                {az_click, arizona_js:push_event(~"toggle_ctl")}
            ]},
            %% A radio group proposed by its form's change event.
            {form, [{id, ~"rf1"}, {az_change, arizona_js:push_event(~"size1")}], [
                {input, [
                    {id, ~"s1-s"},
                    {type, ~"radio"},
                    {name, ~"size1"},
                    {value, ~"s"},
                    {checked, ?get(size1) =:= ~"s"}
                ]},
                {input, [
                    {id, ~"s1-m"},
                    {type, ~"radio"},
                    {name, ~"size1"},
                    {value, ~"m"},
                    {checked, ?get(size1) =:= ~"m"}
                ]},
                {input, [
                    {id, ~"s1-l"},
                    {type, ~"radio"},
                    {name, ~"size1"},
                    {value, ~"l"},
                    {checked, ?get(size1) =:= ~"l"}
                ]}
            ]},
            %% A radio group of form rf2, two of its radios outside the form element.
            {form, [{id, ~"rf2"}], [
                {input, [
                    {id, ~"s2-a"},
                    {type, ~"radio"},
                    {name, ~"size2"},
                    {value, ~"a"},
                    {checked, ?get(size2) =:= ~"a"},
                    {az_change, arizona_js:push_event(~"size2")}
                ]}
            ]},
            {input, [
                {id, ~"s2-b"},
                {type, ~"radio"},
                {form, ~"rf2"},
                {name, ~"size2"},
                {value, ~"b"},
                {checked, ?get(size2) =:= ~"b"},
                {az_change, arizona_js:push_event(~"size2")}
            ]},
            {input, [
                {id, ~"s2-c"},
                {type, ~"radio"},
                {form, ~"rf2"},
                {name, ~"size2"},
                {value, ~"c"},
                {checked, ?get(size2) =:= ~"c"},
                {az_change, arizona_js:push_event(~"size2")}
            ]},
            %% A formless radio group.
            {'div', [], [
                {input, [
                    {id, ~"s3-x"},
                    {type, ~"radio"},
                    {name, ~"size3"},
                    {value, ~"x"},
                    {checked, ?get(size3) =:= ~"x"},
                    {az_change, arizona_js:push_event(~"size3")}
                ]},
                {input, [
                    {id, ~"s3-y"},
                    {type, ~"radio"},
                    {name, ~"size3"},
                    {value, ~"y"},
                    {checked, ?get(size3) =:= ~"y"},
                    {az_change, arizona_js:push_event(~"size3")}
                ]},
                {input, [
                    {id, ~"s3-z"},
                    {type, ~"radio"},
                    {name, ~"size3"},
                    {value, ~"z"},
                    {checked, ?get(size3) =:= ~"z"},
                    {az_change, arizona_js:push_event(~"size3")}
                ]}
            ]},
            {select, [{id, ~"color"}, {az_change, arizona_js:push_event(~"color")}], [
                {option, [{value, ~"red"}, {selected, ?get(color) =:= ~"red"}], [~"Red"]},
                {option, [{value, ~"green"}, {selected, ?get(color) =:= ~"green"}], [~"Green"]},
                {option, [{value, ~"blue"}, {selected, ?get(color) =:= ~"blue"}], [~"Blue"]}
            ]},
            {select, [{id, ~"multi"}, multiple, {az_change, arizona_js:push_event(~"multi")}], [
                {option, [{value, ~"x"}, {selected, lists:member(~"x", ?get(multi))}], [~"X"]},
                {option, [{value, ~"y"}, {selected, lists:member(~"y", ?get(multi))}], [~"Y"]},
                {option, [{value, ~"z"}, {selected, lists:member(~"z", ?get(multi))}], [~"Z"]}
            ]},
            %% No option selected: the browser shows the first enabled one.
            {select, [{id, ~"pick"}, {az_change, arizona_js:push_event(~"pick")}], [
                {option, [{value, ~"one"}, {selected, ?get(pick) =:= ~"one"}], [~"One"]},
                {option, [{value, ~"two"}, {selected, ?get(pick) =:= ~"two"}], [~"Two"]},
                {option, [{value, ~"three"}, {selected, ?get(pick) =:= ~"three"}], [~"Three"]}
            ]},
            %% A form proposing its select with no option selected, which it refuses.
            {form, [{id, ~"pform"}, {az_change, arizona_js:push_event(~"pform")}], [
                {input, [{id, ~"pnote"}, {name, ~"pnote"}]},
                {select, [{id, ~"fpick"}, {name, ~"fpick"}], [
                    {option, [{value, ~"x"}, {selected, ?get(fpick) =:= ~"x"}], [~"X"]},
                    {option, [{value, ~"y"}, {selected, ?get(fpick) =:= ~"y"}], [~"Y"]}
                ]}
            ]},
            {select, [{id, ~"hpick"}, {az_change, arizona_js:push_event(~"hpick")}], [
                case ?get(head) of
                    true -> {option, [{value, ~"a"}, {selected, ?get(hpick) =:= ~"a"}], [~"A"]};
                    false -> <<>>
                end,
                {option, [{value, ~"b"}, {selected, ?get(hpick) =:= ~"b"}], [~"B"]},
                {option, [{value, ~"c"}, {selected, ?get(hpick) =:= ~"c"}], [~"C"]}
            ]},
            {button, [{id, ~"prepend"}, {az_click, arizona_js:patch(~"/bound-controls?head=1")}], [
                ~"Prepend"
            ]},
            {textarea, [{id, ~"notes"}], [?get(notes)]},
            {button, [{id, ~"set-notes"}, {az_click, arizona_js:push_event(~"set_notes")}], [
                ~"Set notes"
            ]},
            %% No event of its own or on a form: never proposed, so never reconciled.
            {input, [{id, ~"quiet"}, {value, ?get(quiet)}]},
            %% Client-owned: no server value at all, and a `?local`.
            {input, [{id, ~"plain"}, {name, ~"plain"}]},
            {input, [{id, ~"loc"}, {value, ?local(~"loc", ~"local")}]},
            %% A form with no az-change: a reconnect replays only a field's own event.
            {form, [{id, ~"rform"}], [
                {input, [{id, ~"rbound"}, {name, ~"rbound"}, {value, ?get(rbound)}]},
                {input, [{id, ~"runbound"}, {name, ~"runbound"}]},
                {input, [
                    {id, ~"revent"},
                    {name, ~"revent"},
                    {value, ?get(revent)},
                    {az_change, arizona_js:push_event(~"revent")}
                ]},
                {select,
                    [
                        {id, ~"rpick"},
                        {name, ~"rpick"},
                        {az_change, arizona_js:push_event(~"rpick")}
                    ],
                    [
                        {option, [{value, ~"a"}, disabled, {selected, ?get(rpick) =:= ~"a"}], [
                            ~"A"
                        ]},
                        {option, [{value, ~"b"}, {selected, ?get(rpick) =:= ~"b"}], [~"B"]},
                        {option, [{value, ~"c"}, {selected, ?get(rpick) =:= ~"c"}], [~"C"]}
                    ]}
            ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"digits", #{~"value" := V}, Bindings) ->
    case digits(V) of
        true -> handled(Bindings#{digits => V});
        false -> handled(Bindings)
    end;
handle_event(~"upper", #{~"value" := V}, Bindings) ->
    handled(Bindings#{upper => string:uppercase(V)});
handle_event(~"trimmed", #{~"value" := V}, Bindings) ->
    handled(Bindings#{trimmed => string:uppercase(string:trim(V))});
handle_event(~"free", #{~"value" := V}, Bindings) ->
    ok = latency(),
    handled(Bindings#{free => V});
%% An unparseable number or a partial date reads as "": `num` and `strict_date`
%% refuse it and keep their value, `date` takes it.
handle_event(~"num", #{~"value" := V}, Bindings) ->
    handled(refuse(num, V, <<>>, Bindings));
handle_event(~"date", #{~"value" := V}, Bindings) ->
    handled(Bindings#{date => V});
handle_event(~"strict_date", #{~"value" := V}, Bindings) ->
    handled(refuse(strict_date, V, <<>>, Bindings));
handle_event(~"deb", #{~"value" := V}, Bindings) ->
    ok = latency(),
    handled(Bindings#{deb => V});
handle_event(~"agree", _Payload, Bindings) ->
    handled(Bindings);
handle_event(~"toggle_ctl", _Payload, #{ctl := Ctl} = Bindings) ->
    handled(Bindings#{ctl => not Ctl});
handle_event(~"size1", #{~"size1" := V}, Bindings) ->
    handled(refuse(size1, V, ~"l", Bindings));
handle_event(~"size2", #{~"value" := V}, Bindings) ->
    handled(refuse(size2, V, ~"c", Bindings));
handle_event(~"size3", #{~"value" := V}, Bindings) ->
    handled(refuse(size3, V, ~"z", Bindings));
handle_event(~"color", #{~"value" := V}, Bindings) ->
    handled(refuse(color, V, ~"blue", Bindings));
handle_event(~"multi", #{~"value" := Vs}, Bindings) ->
    case lists:member(~"z", Vs) of
        true -> handled(Bindings);
        false -> handled(Bindings#{multi => Vs})
    end;
handle_event(~"pick", #{~"value" := V}, Bindings) ->
    handled(refuse(pick, V, ~"two", Bindings));
handle_event(~"rpick", #{~"value" := V}, Bindings) ->
    handled(Bindings#{rpick => V});
handle_event(~"pform", _Payload, Bindings) ->
    handled(Bindings);
handle_event(~"hpick", #{~"value" := V}, Bindings) ->
    handled(Bindings#{hpick => V});
handle_event(~"set_notes", _Payload, Bindings) ->
    handled(Bindings#{notes => ~"From the server"});
handle_event(~"revent", #{~"value" := V}, Bindings) ->
    handled(Bindings#{revent => string:uppercase(V)}).

-spec handle_update(az:bindings(), az:bindings(), az:effects()) -> az:handle_update_ret().
handle_update(Params, Bindings, Effects) ->
    {Bindings#{head => head(Params)}, #{}, Effects}.

head(Bindings) ->
    proplists:is_defined(~"head", maps:get(params, Bindings, [])).

%% A slow reply, so echoes of older text land while newer text is typed.
latency() ->
    timer:sleep(80).

handled(#{seq := Seq} = Bindings) ->
    {Bindings#{seq => Seq + 1}, #{}, []}.

refuse(_Key, Refused, Refused, Bindings) ->
    Bindings;
refuse(Key, V, _Refused, Bindings) ->
    Bindings#{Key => V}.

digits(V) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(V)).
