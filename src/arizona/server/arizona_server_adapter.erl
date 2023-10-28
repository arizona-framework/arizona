%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc Server adapter.

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
-module(arizona_server_adapter).

%% Types
-export_type([ method/0, path/0, status_code/0, body/0, params/0 ]).

-type request() :: term().
-type headers() :: #{binary() => binary()}.
-type method() :: get
                | post
                | patch
                | delete
                | put
                | connect
                | head
                | options
                | trace
                .
-type path() :: [binary()].
-type status_code() :: 200..500.
-type body() :: binary().
-type params() :: [{binary(), binary()}].

%% Callbacks
-optional_callbacks([]).

-callback start(Args) -> ok | {error, term()}
    when Args :: map().

-callback stop(State) -> ok
    when State :: term().

-callback get_headers(Req) -> Headers
    when Req :: request()
       , Headers :: headers()
       .

-callback set_headers(Headers, Req) -> Req
    when Headers :: headers()
       , Req :: request()
       .

-callback set_status_code(StatusCode, Req) -> Req
    when StatusCode :: status_code()
       , Req :: request()
       .
-callback get_body(Req) -> {Body, Req}
    when Req :: request()
       , Body :: body()
       .

-callback set_body(Body, Req) -> Req
    when Body :: body()
       , Req :: request()
       .
