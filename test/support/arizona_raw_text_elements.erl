-module(arizona_raw_text_elements).
-moduledoc """
Fixture for the HTML elements whose content the parser reads as RAWTEXT besides
`<style>`: `<xmp>`, `<iframe>`, `<noembed>` and `<noframes>`. A comment marker or
an entity there is literal content, so a slot in one must be markerless and
verbatim, and its value must not be able to close the element.

`<noscript>` is here as the deliberate exception: its content only renders where
scripting is off, which parses it as ordinary HTML.
""".
-include("arizona_stateless.hrl").
-export([xmp/1]).
-export([iframe/1]).
-export([noembed/1]).
-export([noframes/1]).
-export([noscript/1]).
-export([xmp_lead/1]).

-spec xmp(az:bindings()) -> az:template().
xmp(Bindings) ->
    ?html({xmp, [], [?raw(?get(v))]}).

-spec iframe(az:bindings()) -> az:template().
iframe(Bindings) ->
    ?html({iframe, [], [?raw(?get(v))]}).

-spec noembed(az:bindings()) -> az:template().
noembed(Bindings) ->
    ?html({noembed, [], [?raw(?get(v))]}).

-spec noframes(az:bindings()) -> az:template().
noframes(Bindings) ->
    ?html({noframes, [], [?raw(?get(v))]}).

%% Static text ending in `<` right before the slot.
-spec xmp_lead(az:bindings()) -> az:template().
xmp_lead(Bindings) ->
    ?html({xmp, [], [~"a<", ?raw(?get(v))]}).

-spec noscript(az:bindings()) -> az:template().
noscript(Bindings) ->
    ?html({noscript, [], [{p, [], [?get(v)]}]}).
