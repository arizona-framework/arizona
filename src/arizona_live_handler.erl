-module(arizona_live_handler).
-moduledoc false.
-behaviour(cowboy_handler).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) exports
%% --------------------------------------------------------------------

-export([init/2]).

%% --------------------------------------------------------------------
%% Behaviour (cowboy_handler) callbacks
%% --------------------------------------------------------------------

-spec init(Req, State1) -> {ok, Req, State2}
    when Req :: cowboy_req:req(),
         State1 :: {Mod, Fun, Opts},
         State2 :: {Mod, Fun, Opts},
         Mod :: module(),
         Fun :: atom(),
         Opts :: arizona:route_opts().
init(Req0, {Mod, Fun, Opts} = State) ->
    Macros = maps:get(macros, Opts, #{}),
    {ok, Block} = arizona_template_compiler:compile(Mod, Fun, Macros),
    Assigns = maps:get(assigns, Opts, #{}),
    Html = arizona_template_renderer:client_render(Block, Assigns),
    Headers = #{<<"content-type">> => <<"text/html">>},
    Req = cowboy_req:reply(200, Headers, Html, Req0),
    {ok, Req, State}.
