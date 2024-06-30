%%
%% %CopyrightBegin%
%%
%% Copyright 2024 William Fank Thomé
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
%%
%% %CopyrightEnd%
%%
-module(arizona_js).
-moduledoc """
Javascript support.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([send/1]).
-export([send/2]).
-ignore_xref([send/2]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

-spec send(EventName) -> Sent
    when EventName :: binary(),
         Sent :: binary().
send(EventName) ->
    <<"arizona.send.bind(this)('", EventName/binary, "')"/utf8>>.

-spec send(EventName, Payload) -> Sent
    when EventName :: binary(),
         Payload :: json:encode_value(),
         Sent :: binary().
send(EventName, Payload) ->
    <<"arizona.send.bind(this)('", EventName/binary, "', ", (safe(Payload))/binary, ")"/utf8>>.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

safe(Term) ->
    iolist_to_binary(json:encode(Term)).

