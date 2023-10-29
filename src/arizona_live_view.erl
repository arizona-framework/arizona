%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc LiveView.

%% Copyright 2023 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(arizona_live_view).

%% API
-export([ init/4, mount/3, render/2, handle_event/4 ]).

%% Types
-export_type([ bindings/0, render_state/0 ]).

-type bindings() :: arizona_template_adapter:bindings().
-type render_state() :: arizona_template_adapter:state().
-type event() :: binary().
-type payload() :: map().
-type params() :: arizona_server_adapter:params().
-type socket() :: arizona_socket:t().

%% Callbacks
-optional_callbacks([ mount/2, handle_event/3 ]).

-callback mount(Params, Socket) -> {ok, Socket}
    when Params :: params()
       , Socket :: socket()
       .

-callback render(Bindings) -> {Bindings, RenderState}
    when Bindings :: bindings()
       , RenderState :: render_state()
       .

-callback handle_event(Event, Payload, Socket) -> {ok, Socket}
    when Event :: event()
       , Payload :: payload()
       , Socket :: socket()
       .

%%%=====================================================================
%%% API
%%%=====================================================================

init(View, #{template := Template}, Params, Socket) ->
    init_template(Template, View, Params, Socket);
init(View, _Opts, Params, Socket) ->
    init_view(View, Params, Socket).

mount(View, Params, Socket) ->
    io:format("[LiveView] ~w: ~p~n", [View, [mount, Params]]),
    case erlang:function_exported(View, mount, 2) of
        true ->
            View:mount(Params, Socket);
        false ->
            {ok, Socket}
    end.

render(View, Socket0) ->
    io:format("[LiveView] ~w: ~p~n", [View, [render, Socket0]]),
    Socket = arizona_socket:bind(self, View, Socket0),
    Bindings = arizona_socket:get_bindings(Socket),
    View:render(Bindings).

handle_event(View, Event, Payload, Socket) ->
    io:format("[LiveView] ~w: ~p~n", [View, [event, Event, Payload]]),
    View:handle_event(Event, Payload, Socket).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

init_view(View, Params, Socket0) ->
    {ok, Socket} = mount(View, Params, Socket0),
    RenderState = render(View, Socket),
    arizona_socket:set_render_state(RenderState, Socket).

init_template(Template, View, Params, Socket0) ->
    Socket1 = arizona_socket:bind(parent, Template, Socket0),
    Socket2 = init_view(View, Params, Socket1),
    ViewRenderState = arizona_socket:get_render_state(Socket2),
    InnerContent = arizona_template:render(ViewRenderState),
    {ok, Socket3} = mount(Template, Params, Socket2),
    Socket = arizona_socket:bind(#{
        view => View,
        inner_content => InnerContent
    }, Socket3),
    RenderState = render(Template, Socket),
    arizona_socket:set_render_state(RenderState, Socket).
