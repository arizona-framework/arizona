-module(arizona_nav_card_shown).
-include("arizona_stateful.hrl").
-export([mount/1, render/1]).

%% Mounts WITH the shared card, so its page payload references the card's
%% fingerprint -- bare, once the socket has already shipped those statics.

-spec mount(az:bindings()) -> az:mount_ret().
mount(_Bindings) ->
    {#{id => ~"ncb"}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {'div', [{id, ?get(id)}], [
            ?stateless(arizona_nav_card, render, #{label => ~"next"})
        ]}
    ).
