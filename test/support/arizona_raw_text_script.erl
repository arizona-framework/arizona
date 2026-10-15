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
-export([const_lead/1]).
-export([cond_lead/1]).
-export([split/1]).
-export([lead_two/1]).
-export([three/1]).
-export([style_const_lead/1]).
-export([trail_static/1]).

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

%% The slots below sit next to OTHER slots, whose bytes are unknown at compile time:
%% a value's end must be judged without knowing what follows it.
-spec const_lead(az:bindings()) -> az:template().
const_lead(Bindings) ->
    ?html({script, [], [?raw(~"var s='<"), ?raw(?get(v)), ~"';"]}).

-spec cond_lead(az:bindings()) -> az:template().
cond_lead(Bindings) ->
    ?html(
        {script, [], [
            ?raw(
                case ?get(m) of
                    a -> ~"var s='<";
                    _ -> ~"var s='"
                end
            ),
            ?raw(?get(v)),
            ~"';"
        ]}
    ).

-spec split(az:bindings()) -> az:template().
split(Bindings) ->
    ?html({script, [], [~"x='", ?raw(?get(a)), ~"i", ?raw(?get(b)), ~"';"]}).

-spec lead_two(az:bindings()) -> az:template().
lead_two(Bindings) ->
    ?html({script, [], [~"x='<", ?raw(?get(a)), ?raw(?get(b)), ~"';"]}).

-spec three(az:bindings()) -> az:template().
three(Bindings) ->
    ?html({script, [], [~"x='", ?raw(?get(a)), ?raw(?get(b)), ?raw(?get(c)), ~"';"]}).

-spec style_const_lead(az:bindings()) -> az:template().
style_const_lead(Bindings) ->
    ?html({style, [], [?raw(~"a{content:'<"), ?raw(?get(v)), ~"'}"]}).

%% A value ending in the start of a sequence the static after it completes.
-spec trail_static(az:bindings()) -> az:template().
trail_static(Bindings) ->
    ?html({script, [], [~"x='", ?raw(?get(v)), ~"--<script>'"]}).
