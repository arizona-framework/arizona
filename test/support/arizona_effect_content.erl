-module(arizona_effect_content).
-include("arizona_stateless.hrl").
-export([text/1]).
-export([nodiff/1]).
-export([raw/1]).
-export([local/1]).
-export([text_list/1]).
-export([item/1]).
-export([script/1]).
-export([json/1]).

%% Fixture: an effect command reaching a content slot through each path a content
%% value takes. The command comes from the `v` binding (or `items`), so compile time
%% cannot see it and the render must refuse it. Each template sits on one line, the
%% line its slot's error is located at.

-spec text(az:bindings()) -> az:template().
text(Bindings) -> ?html({p, [], [?get(v)]}).

-spec nodiff(az:bindings()) -> az:template().
nodiff(Bindings) -> ?html({p, [az_nodiff], [?get(v)]}).

-spec raw(az:bindings()) -> az:template().
raw(Bindings) -> ?html({p, [], [?raw(?get(v))]}).

-spec local(az:bindings()) -> az:template().
local(Bindings) -> ?html({p, [], [?local(~"k", ?get(v))]}).

-spec text_list(az:bindings()) -> az:template().
text_list(Bindings) -> ?html({p, [], [[~"a: ", ?get(v)]]}).

-spec item(az:bindings()) -> az:template().
item(Bindings) -> ?html({ul, [], [?each(fun(V) -> {li, [], [V]} end, ?get(items))]}).

-spec script(az:bindings()) -> az:template().
script(Bindings) -> ?html({script, [], [?raw(?get(v))]}).

-spec json(az:bindings()) -> az:template().
json(Bindings) -> ?json({'Text', [], [?get(v)]}).
