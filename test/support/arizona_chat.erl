-module(arizona_chat).
-moduledoc """
Pubsub cross-tab chat fixture, scoped to the `:room` path segment.

The channel is `{chat, Room}`, never a bare `chat`: the e2e server is
started once for a whole Playwright run (and reused across runs locally),
so a globally-named channel is shared by every `/chat` view the server has
ever handed out. Any second client -- another test, a second Playwright
process reusing the server, a page left open -- then broadcasts straight
into the assertions of whatever else is on the channel. Keying the channel
by room gives each test its own, which is what makes the spec deterministic
(serializing the tests never isolated anything; the channel outlives them).
""".
-include("arizona_stateful.hrl").
-export([mount/1, unmount/1, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    %% `room` arrives as a binary-keyed path binding (extract/1).
    Room = maps:get(~"room", Bindings, ~"lobby"),
    ?connected andalso ?subscribe({chat, Room}),
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end),
    {
        #{
            id => ~"page",
            room => Room,
            title => maps:get(title, Bindings, ~"Chat"),
            messages => Stream
        },
        #{}
    }.

-spec unmount(az:bindings()) -> ok.
unmount(Bindings) ->
    _ = ?unsubscribe({chat, maps:get(room, Bindings)}),
    ok.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {main, [{id, ?get(id)}], [
            {h1, [], [~"Chat"]},
            {ul, [{id, ~"messages"}], [
                ?each(
                    fun(#{id := Id, text := Text, owner := Owner}, Key) ->
                        {li, [{az_key, Key}], [
                            {span, [], [Text]},
                            case Owner =:= self() of
                                true ->
                                    ?html(
                                        {button,
                                            [
                                                {az_click,
                                                    arizona_js:push_event(
                                                        ~"delete", #{~"id" => Id}
                                                    )}
                                            ],
                                            [
                                                ~"x"
                                            ]}
                                    );
                                false ->
                                    ~""
                            end
                        ]}
                    end,
                    ?get(messages)
                )
            ]},
            {form, [{id, ~"chat-form"}, {az_submit, arizona_js:push_event(~"send")}, az_form_reset],
                [
                    {input,
                        [
                            {name, ~"text"},
                            {type, ~"text"},
                            {placeholder, ~"Type a message..."}
                        ],
                        []},
                    {button, [{type, ~"submit"}], [~"Send"]}
                ]}
        ]}
    ).

-spec handle_event(az:event_name(), az:event_payload(), az:bindings()) ->
    az:handle_event_ret().
handle_event(~"send", Payload, Bindings) ->
    case maps:get(~"text", Payload, ~"") of
        ~"" ->
            {Bindings, #{}, []};
        Text ->
            MsgId = integer_to_binary(erlang:unique_integer([positive])),
            Msg = #{id => MsgId, text => Text, owner => self()},
            Channel = {chat, maps:get(room, Bindings)},
            arizona_pubsub:broadcast_from(self(), Channel, {chat_msg, Msg}),
            S = arizona_stream:insert(maps:get(messages, Bindings), Msg),
            {Bindings#{messages => S}, #{}, []}
    end;
handle_event(~"delete", #{~"id" := Id}, Bindings) ->
    Stream = maps:get(messages, Bindings),
    #{owner := Owner} = arizona_stream:get(Stream, Id),
    case Owner =:= self() of
        true ->
            Channel = {chat, maps:get(room, Bindings)},
            arizona_pubsub:broadcast_from(self(), Channel, {chat_delete, Id}),
            S = arizona_stream:delete(Stream, Id),
            {Bindings#{messages => S}, #{}, []};
        false ->
            {Bindings, #{}, []}
    end.

-spec handle_info(term(), az:bindings()) -> az:handle_info_ret().
handle_info({chat_msg, Msg}, Bindings) ->
    S = arizona_stream:insert(maps:get(messages, Bindings), Msg),
    {Bindings#{messages => S}, #{}, []};
handle_info({chat_delete, Id}, Bindings) ->
    S = arizona_stream:delete(maps:get(messages, Bindings), Id),
    {Bindings#{messages => S}, #{}, []}.
