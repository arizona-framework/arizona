-module(arizona_view_handler).
-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type opts() :: #{
    layout => module()
}.
-export_type([opts/0]).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) callbacks
%% --------------------------------------------------------------------

-spec init(Req0, State) -> {ok, Req1, State} when
    Req0 :: cowboy_req:req(),
    State :: {Mod, Assigns, Opts},
    Mod :: module(),
    Assigns :: arizona_view:assigns(),
    Opts :: opts(),
    Req1 :: cowboy_req:req().
init(Req0, {Mod, Assigns, Opts} = State) when is_atom(Mod), is_map(Assigns), is_map(Opts) ->
    Socket0 = arizona_socket:new(render),
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket0),
    TemplateToken = arizona_view:render(Mod, View0),
    {View1, Token} = maybe_render_layout(View0, TemplateToken, Opts),
    {View, _Socket} = arizona_render:render(Token, View1, View1, Socket0),
    Html = arizona_view:rendered_to_iolist(View),
    Headers = #{~"content-type" => ~"text/html"},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

maybe_render_layout(View, TemplateToken, Opts) ->
    case Opts of
        #{layout := Mod} ->
            Assigns = arizona_view:assigns(View),
            LayoutView = arizona_view:new(Assigns#{
                inner_content => TemplateToken
            }),
            Token = arizona_component:render(Mod, render, LayoutView),
            {LayoutView, Token};
        #{} ->
            {View, TemplateToken}
    end.
