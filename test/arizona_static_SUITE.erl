-module(arizona_static_SUITE).
-include_lib("stdlib/include/assert.hrl").
%% Feeds generate/2 a spec whose Opts violates opts() on purpose.
-dialyzer({nowarn_function, collects_non_map_spec_opts/1}).
%% Hands embed_attrs/2 an improper iolist on purpose, a shape iodata allows.
-dialyzer({nowarn_function, html_embed_reads_the_end_of_any_iolist/1}).

-export([all/0]).
-export([groups/0]).

-export([writes_file_and_returns_path/1]).
-export([applies_bindings/1]).
-export([applies_layout/1]).
-export([creates_nested_dirs/1]).
-export([applies_on_mount/1]).
-export([multiple_specs_return_all_paths/1]).
-export([default_opts_apply_to_every_spec/1]).
-export([spec_opts_override_defaults/1]).
-export([collects_failures/1]).
-export([failure_carries_render_location/1]).
-export([failed_entries_cover_every_spec_shape/1]).
-export([collects_write_errors/1]).
-export([collects_non_map_spec_opts/1]).
-export([embeds_compile_time_proven_attrs/1]).
-export([embeds_render_time_proven_attrs/1]).
-export([page_without_events_embeds_no_attrs/1]).
-export([page_without_body_gets_the_names_after_it/1]).
-export([html_embed_never_searches_the_document/1]).
-export([html_embed_reads_the_end_of_any_iolist/1]).
-export([layout_dropping_or_repeating_the_page/1]).
-export([target_decides_how_a_document_carries_names/1]).
-export([target_without_embed_attrs_embeds_nothing/1]).
-export([leaves_the_collector_disarmed/1]).
-export([names_are_the_bodys_last_child/1]).
-export([body_close_in_content_does_not_move_the_names/1]).
-export([content_after_the_body_gets_the_names_after_it/1]).
-export([names_go_before_the_last_of_two_bodies/1]).
-export([nested_layouts_put_the_names_in_the_outermost_body/1]).
-export([body_from_a_helper_a_component_or_nodiff/1]).
-export([document_not_ending_in_body_gets_names_after/1]).
-export([page_rendering_its_own_body/1]).
-export([raw_attr_quote_stays_in_the_value/1]).

all() ->
    [{group, generate}].

groups() ->
    [
        {generate, [parallel], [
            writes_file_and_returns_path,
            applies_bindings,
            applies_layout,
            creates_nested_dirs,
            applies_on_mount,
            multiple_specs_return_all_paths,
            default_opts_apply_to_every_spec,
            spec_opts_override_defaults,
            collects_failures,
            failure_carries_render_location,
            failed_entries_cover_every_spec_shape,
            collects_write_errors,
            collects_non_map_spec_opts,
            embeds_compile_time_proven_attrs,
            embeds_render_time_proven_attrs,
            page_without_events_embeds_no_attrs,
            page_without_body_gets_the_names_after_it,
            html_embed_never_searches_the_document,
            html_embed_reads_the_end_of_any_iolist,
            layout_dropping_or_repeating_the_page,
            target_decides_how_a_document_carries_names,
            target_without_embed_attrs_embeds_nothing,
            leaves_the_collector_disarmed,
            names_are_the_bodys_last_child,
            body_close_in_content_does_not_move_the_names,
            content_after_the_body_gets_the_names_after_it,
            names_go_before_the_last_of_two_bodies,
            nested_layouts_put_the_names_in_the_outermost_body,
            body_from_a_helper_a_component_or_nodiff,
            document_not_ending_in_body_gets_names_after,
            page_rendering_its_own_body,
            raw_attr_quote_stays_in_the_value
        ]}
    ].

%% A `"` in a literal `?raw` attribute value would end the value; the page holds it
%% as `&quot;`, the author's `"` as text.
raw_attr_quote_stays_in_the_value(Config) when is_list(Config) ->
    Dir = subdir(Config, "raw_attr_quote"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_raw_attr_page, ~"index.html"}]),
    ?assertMatch(
        {_, _},
        binary:match(read(Out), ~"title=\"a&quot;bv\" data-r=\"c&quot;d\"")
    ).

writes_file_and_returns_path(Config) when is_list(Config) ->
    Dir = subdir(Config, "writes"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_page, ~"index.html"}]),
    ?assert(filelib:is_regular(Out)),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"static_page")),
    ?assertMatch({_, _}, binary:match(Content, ~"Static")).

applies_bindings(Config) when is_list(Config) ->
    Dir = subdir(Config, "bindings"),
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{bindings => #{title => ~"Hello"}}}]
    ),
    ?assertMatch({_, _}, binary:match(read(Out), ~"Hello")).

applies_layout(Config) when is_list(Config) ->
    Dir = subdir(Config, "layout"),
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{layouts => [{arizona_layout, render}]}}]
    ),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"<!DOCTYPE html>")),
    ?assertMatch({_, _}, binary:match(Content, ~"static_page")).

creates_nested_dirs(Config) when is_list(Config) ->
    Dir = subdir(Config, "nested"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_static_page, ~"deep/page.html"}]),
    ?assert(filelib:is_regular(Out)).

applies_on_mount(Config) when is_list(Config) ->
    Dir = subdir(Config, "on_mount"),
    Hook = fun(B) -> B#{title => ~"FromHook"} end,
    {[Out], []} = arizona_static:generate(
        Dir, [{arizona_static_page, ~"index.html", #{on_mount => [Hook]}}]
    ),
    ?assertMatch({_, _}, binary:match(read(Out), ~"FromHook")).

multiple_specs_return_all_paths(Config) when is_list(Config) ->
    Dir = subdir(Config, "batch"),
    {[A, B], []} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"a.html"},
        {arizona_static_page, ~"sub/b.html", #{bindings => #{title => ~"Bee"}}}
    ]),
    ?assert(filelib:is_regular(A)),
    ?assertMatch({_, _}, binary:match(read(B), ~"Bee")).

default_opts_apply_to_every_spec(Config) when is_list(Config) ->
    Dir = subdir(Config, "defaults"),
    {[P1, P2], []} = arizona_static:generate(
        Dir,
        [{arizona_static_page, ~"a.html"}, {arizona_static_page, ~"b.html"}],
        #{layouts => [{arizona_layout, render}]}
    ),
    %% the shared layout wraps both pages
    ?assertMatch({_, _}, binary:match(read(P1), ~"<!DOCTYPE html>")),
    ?assertMatch({_, _}, binary:match(read(P2), ~"<!DOCTYPE html>")).

spec_opts_override_defaults(Config) when is_list(Config) ->
    Dir = subdir(Config, "override"),
    {[Out], []} = arizona_static:generate(
        Dir,
        [{arizona_static_page, ~"index.html", #{bindings => #{title => ~"Override"}}}],
        #{bindings => #{title => ~"Default"}}
    ),
    Content = read(Out),
    ?assertMatch({_, _}, binary:match(Content, ~"Override")),
    ?assertEqual(nomatch, binary:match(Content, ~"Default")).

collects_failures(Config) when is_list(Config) ->
    Dir = subdir(Config, "failures"),
    %% A good spec is written; a spec whose handler does not exist is collected
    %% in Failed (its `mount/1` raises `undef`) without stopping the batch.
    {[Good], [{BadSpec, Failure}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {nonexistent_handler, ~"bad.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual({nonexistent_handler, ~"bad.html"}, BadSpec),
    %% Class AND stacktrace, not a bare `undef`: one failing page in a 200-page
    %% build has to say what and where, so the entry keeps the whole exception.
    ?assertMatch(
        {error, undef, [{nonexistent_handler, mount, [_Bindings], _Info} | _]}, Failure
    ),
    ?assertNot(filelib:is_regular(filename:join(Dir, "bad.html"))).

failure_carries_render_location(Config) when is_list(Config) ->
    Dir = subdir(Config, "failure_location"),
    %% A render-time crash: the `arizona_loc` wrapper `arizona_render` attaches
    %% (the template module + line that produced the bad value) must survive
    %% into the `Failed` entry, alongside the frame that raised.
    {[], [{_BadSpec, Failure}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"bad.html", #{bindings => #{title => [<<255>>]}}}
    ]),
    ?assertMatch(
        {error, {arizona_loc, {arizona_static_page, _Line}, {non_utf8_render, 255, 0}}, [
            {arizona_template, to_bin, _Args, _Info} | _
        ]},
        Failure
    ).

failed_entries_cover_every_spec_shape(Config) when is_list(Config) ->
    Dir = subdir(Config, "every_shape"),
    Collide = filename:join(Dir, "collide"),
    ok = filelib:ensure_dir(Collide),
    ok = file:write_file(Collide, <<>>),
    %% One batch with every failing shape at once: a crashing 2-tuple spec, a
    %% crashing 3-tuple spec, and a write error. `Failed` must hold all three --
    %% a reporter that matches the SPEC shape or a single failure kind drops the
    %% ones it does not match and silently under-reports a broken build.
    {[Good], Failed} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {nonexistent_handler, ~"crash2.html"},
        {arizona_static_page, ~"crash3.html", #{bindings => #{title => [<<255>>]}}},
        {arizona_static_page, ~"collide/page.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual(3, length(Failed)),
    %% The pair shape the documented reporter destructures, for every entry...
    Outfiles = [element(2, Spec) || {Spec, _Failure} <:- Failed],
    ?assertEqual([~"crash2.html", ~"crash3.html", ~"collide/page.html"], Outfiles),
    %% ...and each failure is one of the two documented kinds.
    ?assertMatch(
        [{error, undef, [_ | _]}, {error, {arizona_loc, _, _}, [_ | _]}, {error, _Posix}],
        [Failure || {_Spec, Failure} <:- Failed]
    ).

collects_write_errors(Config) when is_list(Config) ->
    Dir = subdir(Config, "write_errors"),
    %% Put a regular file where a spec needs a parent directory: filelib:ensure_dir
    %% *returns* {error, _} (it does not raise), so this exercises generate's
    %% error-return path -- distinct from a handler crash, which is caught. The
    %% batch still continues and the other spec is written.
    Collide = filename:join(Dir, "collide"),
    ok = filelib:ensure_dir(Collide),
    ok = file:write_file(Collide, <<>>),
    {[Good], [{BadSpec, _Reason}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"ok.html"},
        {arizona_static_page, ~"collide/page.html"}
    ]),
    ?assert(filelib:is_regular(Good)),
    ?assertEqual({arizona_static_page, ~"collide/page.html"}, BadSpec),
    ?assertNot(filelib:is_regular(filename:join(Dir, "collide/page.html"))).

collects_non_map_spec_opts(Config) when is_list(Config) ->
    Dir = subdir(Config, "non_map_opts"),
    %% A 3-tuple spec is well-formed, so a non-map Opts in it is a per-page
    %% failure like any other: collected, with the specs around it still written
    %% and returned rather than lost to a batch-wide crash.
    {[First, Last], [{BadSpec, Failure}]} = arizona_static:generate(Dir, [
        {arizona_static_page, ~"first.html"},
        {arizona_static_page, ~"bad.html", not_a_map},
        {arizona_static_page, ~"last.html"}
    ]),
    ?assert(filelib:is_regular(First)),
    ?assert(filelib:is_regular(Last)),
    ?assertEqual({arizona_static_page, ~"bad.html", not_a_map}, BadSpec),
    ?assertMatch({error, {badmap, not_a_map}, [_ | _]}, Failure),
    ?assertNot(filelib:is_regular(filename:join(Dir, "bad.html"))).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% The static client delegates exactly the names the page carries: a counter's
%% `az-click` is proven by the compiler and lands as the body's last child.
embeds_compile_time_proven_attrs(Config) when is_list(Config) ->
    Content = generate_with_layout(Config, "proven_compile", arizona_counter),
    ?assertEqual([~"az-click"], embedded_attrs(Content)),
    ?assert(ends_with(Content, <<(embed(~"[\"az-click\"]"))/binary, "</body></html>">>)).

%% A command built at render time is invisible to the compiler: only the armed
%% collector proves `az-dblclick`.
embeds_render_time_proven_attrs(Config) when is_list(Config) ->
    Content = generate_with_layout(Config, "proven_render", arizona_static_runtime_cmd),
    ?assertEqual([~"az-dblclick"], embedded_attrs(Content)).

%% A page with no names still carries an empty set: the client reads the last
%% embed, and without one that is an element the page content holds.
page_without_events_embeds_no_attrs(Config) when is_list(Config) ->
    Content = generate_with_layout(Config, "no_events", arizona_static_page),
    ?assertEqual([], embedded_attrs(Content)).

%% A page with no `</body>` (no layout) is a body's content to the parser, so the
%% names follow it like any other page's, which stays exactly what rendering
%% produced.
page_without_body_gets_the_names_after_it(Config) when is_list(Config) ->
    Dir = subdir(Config, "no_body"),
    {[Out], []} = arizona_static:generate(Dir, [{arizona_counter, ~"index.html"}]),
    Content = read(Out),
    Page = iolist_to_binary(arizona_render:render_view_to_iolist(arizona_counter, #{})),
    ?assertMatch(<<Page:(byte_size(Page))/binary, "<script", _/binary>>, Content),
    ?assertEqual([~"az-click"], embedded_attrs(Content)).

%% How a document carries the names is its target's: HTML puts a script before
%% the `</body>` that ends the document, else after the document; a JSON document
%% is one value, left whole.
target_decides_how_a_document_carries_names(Config) when is_list(Config) ->
    Names = [~"az-click"],
    Script = embed(~"[\"az-click\"]"),
    Html = fun(Doc) -> iolist_to_binary(arizona_html:embed_attrs(Doc, Names)) end,
    ?assertEqual(
        <<"<html><body><p>x</p>", Script/binary, "</body></html>">>,
        Html(~"<html><body><p>x</p></body></html>")
    ),
    %% Case and whitespace after `</body>` as the parser reads them.
    ?assertEqual(
        <<"<HTML><BODY><p>x</p>", Script/binary, "</BODY>\n</HTML>\n">>,
        Html(~"<HTML><BODY><p>x</p></BODY>\n</HTML>\n")
    ),
    ?assertEqual(
        <<"<body><p>x</p>", Script/binary, "</Body> \t\r\n">>,
        Html(~"<body><p>x</p></Body> \t\r\n")
    ),
    %% No `</body>` at the end: a fragment, or content after it.
    ?assertEqual(<<"<p>x</p>", Script/binary>>, Html(~"<p>x</p>")),
    ?assertEqual(
        <<"<body><p>x</p></body><!-- c --></html>", Script/binary>>,
        Html(~"<body><p>x</p></body><!-- c --></html>")
    ),
    ?assertEqual(
        <<"<body></body></html></html>", Script/binary>>,
        Html(~"<body></body></html></html>")
    ),
    ?assertEqual(<<"<body></body x>", Script/binary>>, Html(~"<body></body x>")),
    Json = ~"{\"type\":\"Text\",\"children\":[\"</body>\"]}",
    ?assertEqual(Json, iolist_to_binary(arizona_json:embed_attrs(Json, Names))).

%% The end of the document is read from the right of the iolist, whatever its
%% shape: `</body></html>` split across leaves, nested lists, a byte, an improper
%% tail, a document shorter than the window, and a page longer than it.
html_embed_reads_the_end_of_any_iolist(Config) when is_list(Config) ->
    Script = embed(~"[]"),
    Html = fun(Doc) -> iolist_to_binary(arizona_html:embed_attrs(Doc, [])) end,
    Expected = <<"<body><p>x</p>", Script/binary, "</body></html>">>,
    [
        ?assertEqual(Expected, Html(Doc))
     || Doc <- [
            [~"<body><p>x</p></bo", ~"dy></ht", ~"ml>"],
            [[~"<body>", [~"<p>x</p><", [$/, ~"body>"]]], [], [[~"</html", $>]]],
            [~"<body><p>x</p></body></ht" | ~"ml>"],
            [~"<body><p>x</p>", [], ~"</body>", [[]], ~"</html>", []]
        ]
    ],
    Long = binary:copy(~"<p>x</p>", 1000),
    ?assertEqual(
        <<"<body>", Long/binary, Script/binary, "</body>">>,
        Html([~"<body>", Long, ~"</", ~"body>"])
    ),
    ?assertEqual(Script, Html(<<>>)),
    ?assertEqual(<<Script/binary, "</body>">>, Html("</body>")),
    %% Whitespace past the window: the `</body>` is not at the end it reads.
    Spaces = binary:copy(~" ", 64),
    ?assertEqual(
        <<"<body></body>", Spaces/binary, Script/binary>>,
        Html([~"<body></body>", Spaces])
    ).

%% `embed_attrs/2` is optional: a target without it has no client script to
%% read the names, so its page is written exactly as rendered.
target_without_embed_attrs_embeds_nothing(Config) when is_list(Config) ->
    Dir = subdir(Config, "no_embed_attrs"),
    Mod = arizona_text_page:load(),
    {[Out], []} = arizona_static:generate(Dir, [{Mod, ~"index.txt"}]),
    Page = iolist_to_binary(arizona_render:render_view_to_iolist(Mod, #{})),
    ?assertEqual(Page, read(Out)).

%% Only the end of the document is read: a `</body>` anywhere before it -- text,
%% a data island -- is never cut into.
html_embed_never_searches_the_document(Config) when is_list(Config) ->
    Script = embed(~"[\"az-click\"]"),
    Island = ~"<main></main><script type=\"application/json\">{\"s\":\"</body>\"}</script>",
    ?assertEqual(
        <<Island/binary, Script/binary>>,
        iolist_to_binary(arizona_html:embed_attrs(Island, [~"az-click"]))
    ),
    Mid = ~"<body><p></body></p><p>x</p></body></html>",
    ?assertEqual(
        <<"<body><p></body></p><p>x</p>", Script/binary, "</body></html>">>,
        iolist_to_binary(arizona_html:embed_attrs(Mid, [~"az-click"]))
    ).

%% The collector is armed for each page's render only, so the caller's process is
%% not left recording what it renders later.
leaves_the_collector_disarmed(Config) when is_list(Config) ->
    _Content = generate_with_layout(Config, "disarmed", arizona_counter),
    ok = arizona_event_attrs:observe_attr(~"az-later"),
    ?assertEqual({[], []}, arizona_event_attrs:drain()).

%% The embed is the body's last child, in a valid document: no parse-error
%% recovery places it.
names_are_the_bodys_last_child(Config) when is_list(Config) ->
    Content = generate_in(Config, "last_child", arizona_counter, [
        {arizona_doc_tail_layout, plain}
    ]),
    ?assertMatch(<<"<!DOCTYPE html><html><head></head><body>", _/binary>>, Content),
    ?assert(ends_with(Content, <<"</div>", (embed(~"[\"az-click\"]"))/binary, "</body></html>">>)),
    ?assertEqual(1, embed_count(Content)).

%% `</body>` in text, a comment, a raw-text or RCDATA element or `?raw` markup
%% before the end is never read: only the document's last bytes are.
body_close_in_content_does_not_move_the_names(Config) when is_list(Config) ->
    Content = generate_in(Config, "decoys", arizona_counter, [{arizona_doc_tail_layout, decoys}]),
    ?assert(ends_with(Content, <<"</div>", (embed(~"[\"az-click\"]"))/binary, "</body></html>">>)),
    ?assertEqual(1, embed_count(Content)).

%% Content after `</body>` is placed after the embed by the parser, so the embed
%% goes after the whole document instead and stays the last one -- even when that
%% content is a forged embed.
content_after_the_body_gets_the_names_after_it(Config) when is_list(Config) ->
    Content = generate_in(Config, "after_body", arizona_counter, [
        {arizona_doc_tail_layout, after_body}
    ]),
    ?assert(ends_with(Content, <<"[]</script></html>", (embed(~"[\"az-click\"]"))/binary>>)),
    ?assertEqual([~"az-click"], last_embedded_attrs(Content)).

%% A second body's content reaches the parser's body after the first one closes,
%% and the `</body>` ending the document is still the one the names go before:
%% nothing follows them.
names_go_before_the_last_of_two_bodies(Config) when is_list(Config) ->
    Content = generate_in(Config, "two_bodies", arizona_counter, [
        {arizona_doc_tail_layout, two_bodies}
    ]),
    ?assert(
        ends_with(
            Content, <<"<body>second", (embed(~"[\"az-click\"]"))/binary, "</body></html>">>
        )
    ),
    ?assertEqual(1, embed_count(Content)).

%% The outermost layout's `</body>` ends the document; an inner layout's own body
%% is not read.
nested_layouts_put_the_names_in_the_outermost_body(Config) when is_list(Config) ->
    Script = embed(~"[\"az-click\"]"),
    Section = generate_in(Config, "nested_section", arizona_counter, [
        {arizona_doc_tail_layout, plain}, {arizona_doc_tail_layout, section}
    ]),
    ?assert(ends_with(Section, <<"</div></section>", Script/binary, "</body></html>">>)),
    Inner = generate_in(Config, "nested_body", arizona_counter, [
        {arizona_doc_tail_layout, plain}, {arizona_doc_tail_layout, inner_body}
    ]),
    ?assert(ends_with(Inner, <<"</div></main></body>", Script/binary, "</body></html>">>)),
    ?assertEqual(1, embed_count(Inner)).

%% However the body was written -- an inlined helper, a `?stateless` component,
%% an az-nodiff root -- only the bytes it renders are read.
body_from_a_helper_a_component_or_nodiff(Config) when is_list(Config) ->
    Script = embed(~"[\"az-click\"]"),
    [
        ?assert(
            ends_with(
                generate_in(Config, Name, arizona_counter, [Layout]),
                <<Before/binary, Script/binary, "</body></html>">>
            )
        )
     || {Name, Layout, Before} <- [
            {"helper_body", {arizona_doc_tail_layout, helper_body}, ~"</div>"},
            %% The component's page prop is a content slot, so its markers close first.
            {"stateless_body", {arizona_doc_tail_layout, stateless_body}, ~"</div><!--/az-->"},
            {"nodiff_body", {arizona_doc_tail_layout, nodiff_body}, ~"</div>"}
        ]
    ].

%% A document that does not end with `</body>` gets the names after it, wherever
%% a `<body>` sits inside it (here in `<svg>`).
document_not_ending_in_body_gets_names_after(Config) when is_list(Config) ->
    Content = generate_in(Config, "svg_body", arizona_counter, [
        {arizona_doc_tail_layout, svg_body}
    ]),
    ?assert(ends_with(Content, <<"</div></main>", (embed(~"[\"az-click\"]"))/binary>>)).

%% A page rendering the whole document itself gets the names in its body too.
page_rendering_its_own_body(Config) when is_list(Config) ->
    Content = generate_in(Config, "own_body", arizona_doc_tail_page, []),
    ?assert(
        ends_with(Content, <<"</button>", (embed(~"[\"az-click\"]"))/binary, "</body></html>">>)
    ).

%% A layout that drops the page or renders it twice changes nothing: the end of
%% the document is read, so the names go before the last `</body>` once, or after
%% a document with none, and the page stays valid UTF-8.
layout_dropping_or_repeating_the_page(Config) when is_list(Config) ->
    Script = embed(~"[\"az-click\"]"),
    Twice = generate_in(Config, "twice", arizona_doc_tail_page, [{arizona_doc_tail_layout, twice}]),
    ?assert(ends_with(Twice, <<"</button>", Script/binary, "</body></html>">>)),
    ?assertEqual(1, embed_count(Twice)),
    ?assertEqual(Twice, unicode:characters_to_binary(Twice)),
    Ignore = generate_in(Config, "ignore", arizona_doc_tail_page, [
        {arizona_doc_tail_layout, ignore}
    ]),
    ?assertEqual(<<"<p>x</p>", Script/binary>>, Ignore).

generate_in(Config, Name, Handler, Layouts) ->
    Dir = subdir(Config, Name),
    {[Out], []} = arizona_static:generate(Dir, [{Handler, ~"index.html", #{layouts => Layouts}}]),
    read(Out).

embed(Json) ->
    <<"<script type=\"application/json\" id=\"arizona-attrs\">", Json/binary, "</script>">>.

embed_count(Content) ->
    length(binary:matches(Content, ~"id=\"arizona-attrs\"")).

last_embedded_attrs(Content) ->
    {match, Jsons} = re:run(
        Content,
        ~"<script type=\"application/json\" id=\"arizona-attrs\">(.*?)</script>",
        [global, {capture, all_but_first, binary}]
    ),
    [Json] = lists:last(Jsons),
    json:decode(Json).

ends_with(Bin, Suffix) ->
    Size = byte_size(Bin) - byte_size(Suffix),
    Size >= 0 andalso binary:part(Bin, Size, byte_size(Suffix)) =:= Suffix.

generate_with_layout(Config, Name, Handler) ->
    Dir = subdir(Config, Name),
    {[Out], []} = arizona_static:generate(
        Dir, [{Handler, ~"index.html", #{layouts => [{arizona_layout, render}]}}]
    ),
    read(Out).

embedded_attrs(Content) ->
    {match, [Json]} = re:run(
        Content,
        ~"<script type=\"application/json\" id=\"arizona-attrs\">(.*?)</script>",
        [{capture, all_but_first, binary}]
    ),
    json:decode(Json).

subdir(Config, Name) ->
    filename:join(proplists:get_value(priv_dir, Config), Name).

read(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.
