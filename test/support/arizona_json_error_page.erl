-module(arizona_json_error_page).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?json` error page, for asserting that a listener's custom error page is
%% served under its own render target's media type rather than HTML.

-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    ?json({'Text', [], [~"Server error"]}).
