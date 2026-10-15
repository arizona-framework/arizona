-module(arizona_raw_text_script).
-moduledoc """
Fixture for the raw-text (script/style) close-tag breakout mitigation (P9).

A dynamic content slot in a `<script>` renders verbatim (the browser decodes
nothing there), so a value carrying `</script>` would otherwise close the element
and drop into HTML parsing -- the classic JSON-in-script XSS. The backend's
`raw_text/4` neutralizes the breakout while leaving benign content untouched.

The slot carries the `?raw` opt-out because a raw-text slot admits nothing else
(an unmarked value is a compile error): the mitigation must therefore reach
*through* the opt-out, which is exactly what this fixture pins.

The `lead*`/`trail`/`style_lead` variants put template static text right beside
the slot, so a value can complete a sequence the static began (`'<` then
`/script>`) or the static can complete one the value began.
""".
-include("arizona_stateless.hrl").
-export([render/1]).
-export([lead/1]).
-export([lead_nodiff/1]).
-export([lead_close/1]).
-export([lead_name/1]).
-export([trail/1]).
-export([style_lead/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html(
        {script, [{type, ~"application/json"}], [?raw(?get(json))]}
    ).

-spec lead(az:bindings()) -> az:template().
lead(Bindings) ->
    ?html({script, [], [~"var s = '<", ?raw(?get(v)), ~"';"]}).

%% Render-once: the content is not folded into a nested template, so the static
%% before the slot shares a buffer with the element's own open tag.
-spec lead_nodiff(az:bindings()) -> az:template().
lead_nodiff(Bindings) ->
    ?html({script, ['az-nodiff'], [~"var s = '<", ?raw(?get(v)), ~"';"]}).

-spec lead_close(az:bindings()) -> az:template().
lead_close(Bindings) ->
    ?html({script, [], [~"h += '</", ?raw(?get(v)), ~"';"]}).

-spec lead_name(az:bindings()) -> az:template().
lead_name(Bindings) ->
    ?html({script, [], [~"h += '</script", ?raw(?get(v)), ~"';"]}).

-spec trail(az:bindings()) -> az:template().
trail(Bindings) ->
    ?html({script, [], [~"h = '", ?raw(?get(v)), ~"ipt>';"]}).

-spec style_lead(az:bindings()) -> az:template().
style_lead(Bindings) ->
    ?html({style, [], [~"a::after{content:'<", ?raw(?get(v)), ~"'}"]}).
