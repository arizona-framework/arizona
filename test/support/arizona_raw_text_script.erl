-module(arizona_raw_text_script).
-moduledoc """
Fixture for the raw-text (script/style) close-tag breakout mitigation (P9).

A dynamic content slot in a `<script>` renders verbatim (the browser decodes
nothing there), so a value carrying `</script>` would otherwise close the element
and drop into HTML parsing -- the classic JSON-in-script XSS. The backend's
`raw_text/1` neutralizes the breakout while leaving benign content untouched.
""".
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {script, [{type, ~"application/json"}], [?get(json)]}
    ).
