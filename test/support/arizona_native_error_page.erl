-module(arizona_native_error_page).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?native` error page, for asserting that a listener's custom error page is
%% served under its own render target's media type rather than HTML.

-spec render(az:bindings()) -> az:template().
render(_Bindings) ->
    ?native({'Text', [], [~"Server error"]}).
