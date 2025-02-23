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
    Assigns :: arizona_view:assigns(),
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
init(Req0, {Mod, Assigns, Opts} = State) when is_atom(Mod), is_map(Assigns), is_map(Opts) ->
    Socket = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket),
    Token = arizona_view:render(View0),
    {View1, _Socket} = arizona_renderer:render(Token, View0, View0, Socket),
    View = maybe_render_layout(View1, Socket, Token, Assigns, Opts),
    Html = arizona_view:rendered_to_iolist(View),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

maybe_render_layout(View, Socket, ViewToken, Assigns, Opts) ->
    case Opts of
        #{layout := LayoutMod} ->
            {LayoutView, _Socket} = arizona_renderer:render_layout(
                LayoutMod, Assigns, ViewToken, Socket
            ),
            LayoutView;
        #{} ->
            View
    end.
