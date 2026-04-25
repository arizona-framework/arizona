-module(arizona_mixed_children).
-include("arizona_view.hrl").
-export([mount/2, render/1, handle_event/3]).
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
%%     <div class="actions">
%%       <button az-click="show" ...>Show</button>
%%       <button az-click="hide">Hide</button>
%%       <button az-click="update_card" ...>Update Card</button>
%%     </div>
%%   </div>

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    B = maps:merge(
        #{
            id => ~"mixed",
            visible => false,
            data => undefined,
            card_class => ~"card",
            card_label => ~"Initial",
            message => ~"Hello"
        },
        Bindings
    ),
    {B, #{}}.

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
                    ~"Update Card"}
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
handle_event(~"connected", _Payload, Bindings) ->
    {Bindings, #{}, []}.

section_class(true) -> ~"rack present";
section_class(false) -> ~"rack absent".

content(undefined) -> ~"Empty";
content(#{name := Name}) -> Name.
