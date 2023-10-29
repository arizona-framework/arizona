%% @author William Fank Thomé <willilamthome@hotmail.com>
%% @copyright 2023 William Fank Thomé
%% @doc LiveView helpers.

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

%% Macros
%% @todo Eval arizona_template:compile/1 via parse_transform.
%% @todo Mechanism to merge templates in eel and remove the regex.
-define(LV(Bin), arizona_template:bind(
    Bindings, arizona_template:compile(
        case maps:find(inner_content, Bindings) of
            {ok, InnerContent} ->
                re:replace(
                    Bin,
                    % @note Compiled version of
                    %       $ re:compile(<<"<%=\\s+@inner_content\\s+.%>">>).
                    {re_pattern,0,0,0, <<
                        69,82,67,80,114,0,0,0,0,0,0,0,81,0,0,0,255,255,255,255,255,255,
                        255,255,60,0,62,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,131,0,46,29,60,29,37,29,61,95,9,29,
                        64,29,105,29,110,29,110,29,101,29,114,29,95,29,99,29,111,29,110,
                        29,116,29,101,29,110,29,116,87,9,12,29,37,29,62,120,0,46,0
                    >>},
                    InnerContent,
                    [{return, binary}]
                );
            error ->
                Bin
        end
    )
)).
