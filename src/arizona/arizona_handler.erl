%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Handler.

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
-module(arizona_handler).

%% API
-export([ init/1, handle/3 ]).

%%%=====================================================================
%%% API
%%%=====================================================================

init(Req) ->
    {ok, Req}.

handle(Method, Path0, Req0) ->
    Path = arizona_server:normalize_path(Path0),
    {Handler, Opts} = arizona_router:match(Method, Path),
    case resolve_opts(proplists:from_map(Opts), Req0) of
        {continue, Req} ->
            do_handle(Handler, Req);
        {halt, Req} ->
            {ok, Req}
    end.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_handle({live, View, Opts}, Req0) ->
    Params = cowboy_req:parse_qs(Req0),
    Socket0 = arizona_socket:new(),
    Socket = arizona_live_view:init(View, Opts, Params, Socket0),
    RenderState = arizona_socket:get_render_state(Socket),
    HTML = arizona_template:render(RenderState),
    Req1 = arizona_server:set_headers(#{
        <<"content-type">> => <<"text/html">>
    }, Req0),
    Req2 = arizona_server:set_body(HTML, Req1),
    Req = arizona_server:set_status_code(200, Req2),
    {ok, Req};
do_handle({Controller, Fun, Args}, Req) ->
    Controller:Fun(Args, Req).

%% Request options

resolve_opts([Opt | T], Req0) ->
    case resolve_opt(Opt, Req0) of
        {continue, Req} ->
            resolve_opts(T, Req);
        {halt, Req} ->
            {halt, Req}
    end;
resolve_opts([], Req) ->
    {continue, Req}.

resolve_opt({middlewares, Middlewares}, Req) ->
    resolve_middlewares(Middlewares, Req).

resolve_middlewares([{Middleware, Args} | T], Req0) ->
    case Middleware(Args, Req0) of
        {continue, Req} ->
            resolve_middlewares(T, Req);
        {halt, Req} ->
            {halt, Req}
    end;
resolve_middlewares([], Req) ->
    {continue, Req}.
