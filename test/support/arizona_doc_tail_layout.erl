-module(arizona_doc_tail_layout).
%% Layouts placing the page's `<body>` in each way a document can hold one, for
%% where the proven-names embed lands (arizona_static_SUITE, arizona_http_SUITE).
-include("arizona_stateless.hrl").
-export([plain/1]).
-export([decoys/1]).
-export([after_body/1]).
-export([two_bodies/1]).
-export([section/1]).
-export([inner_body/1]).
-export([helper_body/1]).
-export([stateless_body/1]).
-export([nodiff_body/1]).
-export([svg_body/1]).
-export([twice/1]).
-export([ignore/1]).

-spec plain(az:bindings()) -> az:template().
plain(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [{head, [], []}, {body, [], [?inner_content]}]}
    ]).

%% `</body>` as text, in a comment, in raw-text and RCDATA elements and in trusted
%% markup, all inside the real body.
-spec decoys(az:bindings()) -> az:template().
decoys(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [
            {head, [], []},
            {body, [], [
                ~"<!-- </body></html> -->",
                {p, [], [?get(text, ~"</body>")]},
                {textarea, [], [~"</body></html>"]},
                {script, [{type, ~"application/json"}], [?raw(~"{\"s\":\"</body>\"}")]},
                ?raw(?get(markup, ~"<p></body></html></p>")),
                ?inner_content
            ]}
        ]}
    ]).

%% Something rendered after `</body>` that the parser would place after the embed.
-spec after_body(az:bindings()) -> az:template().
after_body(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [
            {body, [], [?inner_content]},
            ?raw(
                ?get(markup, ~"<script type=\"application/json\" id=\"arizona-attrs\">[]</script>")
            )
        ]}
    ]).

-spec two_bodies(az:bindings()) -> az:template().
two_bodies(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [{body, [], [?inner_content]}, {body, [], [~"second"]}]}
    ]).

-spec section(az:bindings()) -> az:template().
section(Bindings) ->
    ?html({section, [], [?inner_content]}).

%% An inner layout with a body of its own, nested in `plain/1`.
-spec inner_body(az:bindings()) -> az:template().
inner_body(Bindings) ->
    ?html({body, [], [{main, [], [?inner_content]}]}).

-spec helper_body(az:bindings()) -> az:template().
helper_body(Bindings) ->
    ?html([~"<!DOCTYPE html>", {html, [], [doc_body(Bindings)]}]).

doc_body(Bindings) ->
    {body, [{class, ~"page"}], [?inner_content]}.

-spec stateless_body(az:bindings()) -> az:template().
stateless_body(Bindings) ->
    ?html([~"<!DOCTYPE html>", ?stateless(fun shell/1, #{page => ?inner_content})]).

shell(Bindings) ->
    ?html({html, [], [{body, [], [{header, [], [~"top"]}, ?get(page)]}]}).

-spec nodiff_body(az:bindings()) -> az:template().
nodiff_body(Bindings) ->
    ?html({html, ['az-nodiff'], [{body, [], [?inner_content]}]}).

%% A `<body>` inside `<svg>` is foreign content, not the page's body.
-spec svg_body(az:bindings()) -> az:template().
svg_body(Bindings) ->
    ?html([{svg, [], [{body, [], []}]}, {main, [], [?inner_content]}]).

-spec twice(az:bindings()) -> az:template().
twice(Bindings) ->
    ?html([?inner_content, ?inner_content]).

%% Drops the page.
-spec ignore(az:bindings()) -> az:template().
ignore(Bindings) when is_map(Bindings) ->
    ?html({p, [], [~"x"]}).
