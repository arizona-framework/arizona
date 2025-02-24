-module(arizona_view_handler).
-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type state() :: {
    Mod :: module(),
    Bindings :: arizona_view:bindings(),
    Opts :: opts()
}.
-export_type([state/0]).

-type opts() :: #{
    layout => module()
}.
-export_type([opts/0]).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) callbacks
%% --------------------------------------------------------------------

-spec init(Req0, State) -> {ok, Req1, State} when
    Req0 :: cowboy_req:req(),
    State :: state(),
    Req1 :: cowboy_req:req().
init(Req0, {Mod, Bindings, Opts} = State) when is_atom(Mod), is_map(Bindings), is_map(Opts) ->
    Socket = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Bindings, Socket),
    Token = arizona_view:render(View0),
    {View1, _Socket} = arizona_renderer:render(Token, View0, View0, Socket),
    View = maybe_render_layout(View1, Socket, Token, Bindings, Opts),
    Html = arizona_view:rendered_to_iolist(View),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

maybe_render_layout(View, Socket, ViewToken, Bindings, Opts) ->
    case Opts of
        #{layout := LayoutMod} ->
            {LayoutView, _Socket} = arizona_renderer:render_layout(
                LayoutMod, Bindings, ViewToken, Socket
            ),
            LayoutView;
        #{} ->
            View
    end.
