-module(arizona_mixed_children).
-include("arizona_stateful.hrl").
-export([mount/1, render/1, handle_event/3]).
-export([render_card/1]).

%% Reproduces a bug where a parent template mixes stateless children
%% (arizona:stateless) with regular dynamic children (arizona:get).
%%
%% After an event changes both `card_data` and `message`, the rack/message
%% dynamic op may target the wrong az slot because the stateless child
%% shifts the az numbering.
%%
%% Layout:
%%   <div id="mixed" az-view>
%%     <section class="left">
%%       <!-- stateless child: render_card -->
%%     </section>
%%     <section az="N" class="DYN_CLASS">   <- rack_class(arizona:get(visible))
%%       DYN_TEXT                             <- content(arizona:get(data))
%%     </section>
%%     <p az="M">DYN_TEXT</p>               <- arizona:get(message)
%%     <div class="strip">                  <- mixed siblings + plain-list each
%%       <div class="item static-a">...</div>
%%       <div class="item static-b">...</div>
%%       <!--az:STRIP-->EACH<!--/az-->        <- ?each(rows): must use OP_TEXT so
%%     </div>                                    the static .item siblings survive
%%     <div class="refs">                   <- sole-child each control
%%       <!--az:REFS-->EACH<!--/az-->          <- ?each(rows): still correct
%%     </div>
%%     <div class="actions">
%%       <button az-click="show" ...>Show</button>
%%       <button az-click="hide">Hide</button>
%%       <button az-click="update_card" ...>Update Card</button>
%%       <button az-click="toggle_rows">Toggle Rows</button>
%%     </div>
%%   </div>

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {
        #{
            id => ~"mixed",
            visible => false,
            data => undefined,
            card_class => ~"card",
            card_label => ~"Initial",
            message => ~"Hello",
            a => ~"A",
            b => ~"B",
            rows => []
        },
        #{}
    }.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            {section, [{class, ~"left"}], [
                ?stateless(render_card, #{
                    id => ~"card1",
                    class => ?get(card_class),
                    label => ?get(card_label)
                })
            ]},
            {section, [{class, section_class(?get(visible))}], [
                content(?get(data))
            ]},
            {p, [], [?get(message)]},
            %% Mixed siblings: two static-ish .item divs (dynamic text inside)
            %% sharing the content slot with a plain-list ?each. Toggling `rows`
            %% must patch only the each's marker span; the static .item siblings
            %% must survive. Before the OP_TEXT fix, the each emitted OP_UPDATE
            %% which innerHTML-wiped the strip's static siblings.
            {'div', [{class, ~"strip"}], [
                {'div', [{class, ~"item static-a"}], [{span, [], [?get(a)]}]},
                {'div', [{class, ~"item static-b"}], [{span, [], [?get(b)]}]},
                ?each(
                    fun(#{k := K, v := V}) ->
                        {'div', [{class, ~"item each-item"}], [
                            {span, [{class, ~"k"}], [K]}, {span, [{class, ~"v"}], [V]}
                        ]}
                    end,
                    ?get(rows)
                )
            ]},
            %% Sole-child control: the each is the only child of .refs, so the
            %% slot az coincides with the parent. Must keep working after the fix.
            {'div', [{class, ~"refs"}], [
                ?each(
                    fun(#{k := K, v := V}) ->
                        {'div', [{class, ~"ref-item"}], [
                            {span, [], [K]}, {span, [], [V]}
                        ]}
                    end,
                    ?get(rows)
                )
            ]},
            {'div', [{class, ~"actions"}], [
                {button, [{az_click, arizona_js:push_event(~"show", #{~"text" => ~"World"})}],
                    ~"Show"},
                {button, [{az_click, arizona_js:push_event(~"hide")}], ~"Hide"},
                {button,
                    [
                        {az_click,
                            arizona_js:push_event(~"update_card", #{
                                ~"class" => ~"card-updated", ~"label" => ~"Updated"
                            })}
                    ],
                    ~"Update Card"},
                {button, [{az_click, arizona_js:push_event(~"toggle_rows")}], ~"Toggle Rows"}
            ]}
        ]}
    ).

-spec render_card(az:bindings()) -> az:template().
render_card(Props) ->
    ?html(
        {'div', [{class, maps:get(class, Props)}], [
            {span, [], [maps:get(label, Props)]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"show", #{~"text" := Text}, Bindings) ->
    {
        Bindings#{
            visible => true,
            data => #{name => Text},
            message => <<"Showing: ", Text/binary>>
        },
        #{},
        []
    };
handle_event(~"hide", _Payload, Bindings) ->
    {
        Bindings#{
            visible => false,
            data => undefined,
            message => ~"Hidden"
        },
        #{},
        []
    };
handle_event(~"update_card", #{~"class" := C, ~"label" := L}, Bindings) ->
    {Bindings#{card_class => C, card_label => L}, #{}, []};
handle_event(~"toggle_rows", _Payload, Bindings) ->
    Rows =
        case maps:get(rows, Bindings) of
            [] -> [#{k => ~"k1", v => ~"v1"}, #{k => ~"k2", v => ~"v2"}];
            _ -> []
        end,
    {Bindings#{rows => Rows}, #{}, []};
handle_event(~"connected", _Payload, Bindings) ->
    {Bindings, #{}, []}.

section_class(true) -> ~"rack present";
section_class(false) -> ~"rack absent".

content(undefined) -> ~"Empty";
content(#{name := Name}) -> Name.
