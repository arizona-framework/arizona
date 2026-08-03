-module(arizona_inner_content_layouts).
-moduledoc """
**TEST FIXTURE.** One layout module per `?inner_content` placement, so the
opacity rules documented at `az:inner_content/1` are pinned by tests.

`branch/1` and `prop/1` are the placements that hand the page to a sub-context;
`raw/1` and `sibling/1` are the placements the opaque value rejects.
""".
-include("arizona_stateless.hrl").

-export([branch/1]).
-export([prop/1]).
-export([maps_get/1]).
-export([attr/1]).
-export([measure/1]).
-export([raw/1]).
-export([sibling/1]).

%% The page returned from a `case` tail -- a value slot, so on iodata it was
%% HTML-escaped; an opaque nested template renders through.
-spec branch(az:bindings()) -> az:template().
branch(Bindings) ->
    ?html([
        ~"<b>",
        case ?get(hide, false) of
            true -> ~"";
            false -> ?inner_content
        end,
        ~"</b>"
    ]).

%% The page handed to a stateless child as a prop -- same story, one level down.
-spec prop(az:bindings()) -> az:template().
prop(Bindings) ->
    ?html([~"<p>", ?stateless(fun body/1, #{content => ?inner_content}), ~"</p>"]).

-spec body(az:bindings()) -> az:template().
body(Bindings) ->
    ?html({'main', [], [?get(content)]}).

%% The raw binding read the `?inner_content` macro exists to replace: the parse
%% transform cannot see it as a block, so it compiles to an escaping value slot.
-spec maps_get(az:bindings()) -> az:template().
maps_get(Bindings) ->
    ?html([~"<g>", maps:get(inner_content, Bindings), ~"</g>"]).

%% The page as an attribute value.
-spec attr(az:bindings()) -> az:template().
attr(Bindings) ->
    ?html({'div', [{'data-page', ?inner_content}], [~"x"]}).

%% The page measured as if it were iodata, from inside a real layout render.
-spec measure(az:bindings()) -> az:template().
measure(Bindings) ->
    Size = iolist_size(az:inner_content(Bindings)),
    ?html([~"<m>", integer_to_binary(Size), ~"</m>"]).

%% The escape opt-out around an already-opaque value: nothing to unwrap.
-spec raw(az:bindings()) -> az:template().
raw(Bindings) ->
    ?html([~"<r>", ?raw(?inner_content), ~"</r>"]).

%% The page as one item of a content slot holding several values.
-spec sibling(az:bindings()) -> az:template().
sibling(Bindings) ->
    ?html([~"<s>", [~"<hr>", ?inner_content], ~"</s>"]).
