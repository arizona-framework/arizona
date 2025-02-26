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
    PathParams = cowboy_req:bindings(Req0),
    QueryString = cowboy_req:qs(Req0),
    Socket = arizona_socket:new(render),
    View = maybe_render_layout(Opts, Mod, PathParams, QueryString, Bindings, Socket),
    Html = arizona_view:rendered_to_iolist(View),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

maybe_render_layout(Opts, ViewMod, PathParams, QueryString, Bindings, Socket) ->
    case Opts of
        #{layout := LayoutMod} ->
            {LayoutView, _Socket} = arizona_renderer:render_layout(
                LayoutMod, ViewMod, PathParams, QueryString, Bindings, Socket
            ),
            LayoutView;
        #{} ->
            {View, _Socket} = arizona_view:init_root(
                ViewMod, PathParams, QueryString, Bindings, Socket
            ),
            View
    end.
