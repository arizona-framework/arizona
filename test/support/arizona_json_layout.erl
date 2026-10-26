-module(arizona_json_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

%% A `?json` layout: wraps a JSON page in a Column. Paired with an HTML page
%% it drives the cross-target layout refusal in arizona_render_SUITE.

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?json({'Column', [], [?inner_content]}).
