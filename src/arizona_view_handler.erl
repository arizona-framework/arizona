-module(arizona_view_handler).
-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) callbacks
%% --------------------------------------------------------------------

-spec init(Req0, State) -> {ok, Req1, State} when
    Req0 :: cowboy_req:req(),
    State :: {Mod, Assigns},
    Mod :: module(),
    Assigns :: arizona_view:assigns(),
    Req1 :: cowboy_req:req().
init(Req0, {Mod, Assigns} = State) when is_atom(Mod), is_map(Assigns) ->
    Socket = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket),
    Token = arizona_view:render(Mod, View0),
    {View, _Socket} = arizona_render:render(Token, View0, View0, Socket),
    Html = arizona_view:rendered_to_iolist(View),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.
