-module(arizona_request_reader).
-include("arizona_view.hrl").
-export([mount/1, render/1]).

-spec mount(az:bindings()) -> az:mount_ret().
mount(Bindings) ->
    %% The `put_request` middleware exposed the whole request under `request`;
    %% this is the escape-hatch path -- read it lazily in mount.
    {Params, _Req1} = arizona_req:params(?get(request)),
    Locale = proplists:get_value(~"locale", Params, ~"none"),
    {#{id => ~"reqreader", locale => Locale}, #{}}.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html({'div', [{id, ?get(id)}], [~"Locale: ", ?get(locale)]}).
