-module(arizona_native_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?native` layout: wraps a native page in a Column. Paired with an HTML page
%% it drives the cross-target layout refusal in arizona_render_SUITE.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?native({'Column', [], [?inner_content]}).
