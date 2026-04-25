-module(arizona_chat).
-include("arizona_view.hrl").
-export([mount/2, unmount/1, render/1, handle_event/3, handle_info/2]).

-spec mount(az:bindings(), az:request()) -> az:mount_ret().
mount(Bindings, _Req) ->
    ?connected andalso ?subscribe(chat),
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end),
    {maps:merge(#{id => ~"page", messages => Stream}, Bindings), #{}}.

-spec unmount(az:bindings()) -> ok.
unmount(_Bindings) ->
    _ = ?unsubscribe(chat),
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
            arizona_pubsub:broadcast_from(self(), chat, {chat_msg, Msg}),
            S = arizona_stream:insert(maps:get(messages, Bindings), Msg),
            {Bindings#{messages => S}, #{}, []}
    end;
handle_event(~"delete", #{~"id" := Id}, Bindings) ->
    Stream = maps:get(messages, Bindings),
    #{owner := Owner} = arizona_stream:get(Stream, Id),
    case Owner =:= self() of
        true ->
            arizona_pubsub:broadcast_from(self(), chat, {chat_delete, Id}),
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
