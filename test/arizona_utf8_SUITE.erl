-module(arizona_utf8_SUITE).
-include_lib("stdlib/include/assert.hrl").

%% Every render target writes UTF-8, but a binary is emitted as the bytes it
%% holds, and checking each one on the render path was measured too expensive.
%% The contract is enforced where it costs nothing: a compile-time warning for the
%% usual source (a non-ASCII `<<"...">>` that is not a folded template static), a
%% named `non_utf8_render` where a JSON encoder already refused the bytes, and in
%% dev mode a once-per-response check that locates the template line.

-export([all/0]).
-export([groups/0]).
-export([warns_latin1_literal_outside_template_statics/1]).
-export([warns_latin1_literal_in_unfolded_list/1]).
-export([no_warning_for_utf8_ascii_or_byte_literals/1]).
-export([folded_statics_render_utf8_at_ssr_and_in_diff/1]).
-export([native_ssr_names_non_utf8_at_its_line/1]).
-export([native_command_prop_names_non_utf8/1]).
-export([refuse_renames_only_the_encoder_utf8_error/1]).
-export([chardata_wrapping_non_utf8_binary/1]).
-export([format_error_names_the_byte_and_the_fix/1]).
-export([to_bin_matches_the_classify_first_reference/1]).
-export([escape_classifies_a_value_once/1]).
-export([prod_http_serves_bytes_unchecked/1]).
-export([dev_http_locates_every_shape/1]).
-export([dev_http_serves_the_located_error_page/1]).
-export([dev_http_passes_valid_utf8/1]).
-export([dev_http_mounts_once/1]).
-export([dev_render_matches_prod_bytes/1]).
-export([effect_command_names_non_utf8/1]).
-export([dev_child_restricted_key_error_matches_prod/1]).
-export([connect_frame_failure_log_names_the_view/1]).
-export([socket_frame_failure_is_named/1]).
-export([dev_socket_frame_failure_is_located/1]).

all() ->
    [{group, contract}, {group, global}].

groups() ->
    [
        {contract, [parallel], [
            warns_latin1_literal_outside_template_statics,
            warns_latin1_literal_in_unfolded_list,
            no_warning_for_utf8_ascii_or_byte_literals,
            folded_statics_render_utf8_at_ssr_and_in_diff,
            native_ssr_names_non_utf8_at_its_line,
            native_command_prop_names_non_utf8,
            refuse_renames_only_the_encoder_utf8_error,
            chardata_wrapping_non_utf8_binary,
            format_error_names_the_byte_and_the_fix,
            to_bin_matches_the_classify_first_reference,
            effect_command_names_non_utf8,
            dev_child_restricted_key_error_matches_prod
        ]},
        %% Sequential: these toggle the dev signal (the `arizona_reload_url`
        %% persistent_term), set a global call-count trace pattern, or add a
        %% logger handler.
        {global, [], [
            escape_classifies_a_value_once,
            prod_http_serves_bytes_unchecked,
            dev_http_locates_every_shape,
            dev_http_serves_the_located_error_page,
            dev_http_passes_valid_utf8,
            dev_http_mounts_once,
            dev_render_matches_prod_bytes,
            socket_frame_failure_is_named,
            connect_frame_failure_log_names_the_view,
            dev_socket_frame_failure_is_located
        ]}
    ].

-define(VIEW, arizona_non_utf8_view).

%% --------------------------------------------------------------------
%% Compile time
%% --------------------------------------------------------------------

%% The literal lines marked `warn` hold a bare non-ASCII `<<"...">>` whose bytes
%% reach the output (or a comparison) as Latin-1: outside any template, a `?get`
%% default, a conditional branch value, a hoisted variable inlined into a slot, a
%% `?raw` payload, a `?stateless` prop. The folded statics on the line marked
%% `static` are not warned about: the transform encodes those as UTF-8.
warns_latin1_literal_outside_template_statics(Config) when is_list(Config) ->
    Lines = [
        ~"-module(arizona_utf8_warn_probe).",
        ~"-include(\"arizona_stateful.hrl\").",
        ~"-export([mount/1, render/1, handle_event/3, title/0]).",
        ~"mount(Bindings) ->",
        ~"    {#{id => maps:get(id, Bindings, ~\"w\"), t => <<\"Ação\">>}, #{}}. %% warn",
        ~"render(Bindings) ->",
        ~"    Hoisted = <<\"içado\">>, %% warn",
        ~"    ?html({'div', [{id, ?get(id)}], [",
        ~"        {p, [{title, <<\"atributo é\">>}], [<<\"café\">>]}, %% static",
        ~"        {p, [], [?get(x, <<\"padrão\">>)]}, %% warn",
        ~"        case ?get(y, a) of a -> <<\"ramo é\">>; _ -> ~\"\" end, %% warn",
        ~"        Hoisted,",
        ~"        ?raw(<<\"<b>é</b>\">>), %% warn",
        ~"        ?stateless(fun child/1, #{t => <<\"prop é\">>}) %% warn",
        ~"    ]}).",
        ~"child(Bindings) -> ?html({i, [], [?get(t)]}).",
        ~"handle_event(_E, _P, B) -> {B#{t => <<\"novo ü\">>}, #{}, []}. %% warn",
        ~"title() -> <<\"Ação\">>. %% warn"
    ],
    {ok, Warnings} = compile_probe(Config, Lines),
    ?assertEqual(marked_lines(Lines, ~"%% warn"), warned_lines(Warnings)),
    Msg = arizona_parse_transform:format_error(latin1_binary_literal),
    [
        ?assertNotEqual(nomatch, string:find(Msg, Part), Part)
     || Part <- [
            "non-ASCII binary literal is encoded as latin1; write ~\"...\" or <<\"...\"/utf8>>",
            "<<16#89, \"PNG\">>",
            "<<\"\\351\":8>>"
        ]
    ],
    %% The bytes the warning's own spellings stand for: \x89 P N G \r \n \x1a \n.
    ?assertEqual(<<"\x89PNG\r\n\x1a\n">>, <<16#89, "PNG\r\n", 16#1A, "\n">>),
    %% Written as escaped strings they still warn: the warning has no opt-out, and
    %% its text is what names the spellings that do not.
    Bytes = [
        ~"-module(arizona_utf8_bytes_probe).",
        ~"-include(\"arizona_stateless.hrl\").",
        ~"-export([png/0, e/0]).",
        ~"png() -> <<\"\\x89PNG\\r\\n\\x1a\\n\">>. %% warn",
        ~"e() -> <<\"\\351\">>. %% warn"
    ],
    {ok, BytesWarnings} = compile_probe(Config, Bytes),
    ?assertEqual(marked_lines(Bytes, ~"%% warn"), warned_lines(BytesWarnings)).

%% A literal list folds only as a whole. Probing one walks its binaries before a
%% later dynamic (or the raw-text rule) gives up on it, and that probe must not
%% count them as folded: the list stays a runtime value carrying Latin-1 bytes.
%% The lines marked `static` fold whole, or (holding a `?raw` block) compile as a
%% fragment whose binaries are template statics, and stay unwarned.
warns_latin1_literal_in_unfolded_list(Config) when is_list(Config) ->
    Lines = [
        ~"-module(arizona_utf8_list_probe).",
        ~"-include(\"arizona_stateless.hrl\").",
        ~"-export([render/1]).",
        ~"render(Bindings) ->",
        ~"    ?html({'div', [], [",
        ~"        {p, [], [[<<\"café \">>, ?get(x)]]}, %% warn",
        ~"        {p, [], [[?get(x), <<\"café \">>]]}, %% warn",
        ~"        {p, [], [[<<\"café \">>, ?raw(?get(x))]]}, %% static",
        ~"        {p, [{title, [<<\"atributo é\">>, ?get(x)]}], []}, %% warn",
        ~"        {title, [], [[<<\"título é\">>, ~\"x\"]]}, %% warn",
        ~"        {p, [], [[<<\"lista é \">>, ~\"x\"]]}, %% static",
        ~"        {p, [{title, [<<\"atributo é \">>, ~\"x\"]}], []} %% static",
        ~"    ]})."
    ],
    {ok, Warnings} = compile_probe(Config, Lines),
    ?assertEqual(marked_lines(Lines, ~"%% warn"), warned_lines(Warnings)).

no_warning_for_utf8_ascii_or_byte_literals(Config) when is_list(Config) ->
    Lines = [
        ~"-module(arizona_utf8_clean_probe).",
        ~"-include(\"arizona_stateless.hrl\").",
        ~"-export([render/1, data/0]).",
        ~"render(Bindings) ->",
        ~"    ?html({p, [{title, <<\"é\">>}], [<<\"café\">>, ?get(x, ~\"padrão\"),",
        ~"        <<\"utf é\"/utf8>>, <<\"ascii\">>, ~\"\"\"",
        ~"        triple é",
        ~"        \"\"\"]}).",
        ~"data() -> {~\"é\", ~b\"é\", <<\"é\"/utf8>>, <<\"ascii\">>,",
        ~"    <<233>>, <<\"a\", 16#E9>>, \"é\",",
        ~"    <<16#89, \"PNG\\r\\n\", 16#1A, \"\\n\">>, <<\"\\351\":8>>, <<\"é\":8>>}."
    ],
    ?assertEqual({ok, []}, compile_probe(Config, Lines)).

%% A non-ASCII literal that is a template static is folded as UTF-8 text, in
%% content and attribute values, and in a conditional branch's static content, so
%% it renders UTF-8 at SSR and ships UTF-8 in the diff. A charlist is chardata.
folded_statics_render_utf8_at_ssr_and_in_diff(Config) when is_list(Config) ->
    View = arizona_utf8_static_view,
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(View, #{bindings => #{open => true}})
    ),
    ?assert(is_binary(unicode:characters_to_binary(HTML))),
    [
        ?assertNotEqual(nomatch, binary:match(HTML, Part), Part)
     || Part <- [
            ~"title=\"atributo é\"",
            ~"café",
            ~"title=\"lista é\"",
            ~"ação",
            ~"class=\"ramo ü\"",
            ~"aberto ü"
        ]
    ],
    {ok, Pid} = arizona_live:start_link(View, #{}, undefined, []),
    {ok, _, _} = arizona_live:mount(Pid),
    {ok, Ops, [], _} = arizona_live:handle_event(Pid, ~"u8s", ~"open", #{}),
    Frame = iolist_to_binary(json:encode(Ops)),
    ?assertNotEqual(nomatch, binary:match(Frame, ~"aberto ü")),
    ?assertNotEqual(nomatch, binary:match(Frame, ~"ramo ü")),
    ok = gen_server:stop(Pid).

%% --------------------------------------------------------------------
%% Free renames on the failure paths
%% --------------------------------------------------------------------

%% `?native` escapes through the JSON encoder, which already refuses the bytes;
%% the refusal used to surface as the encoder's `unexpected_end`.
native_ssr_names_non_utf8_at_its_line(Config) when is_list(Config) ->
    Line = line_of(~"?get(label)"),
    [
        ?assertEqual(
            {Key, {arizona_loc, {?VIEW, Line}, {non_utf8_render, 16#E9, 3}}},
            {Key, raised(fun() -> arizona_render:render_to_iolist(?VIEW:native(Bindings)) end)}
        )
     || {Key, Bindings} <- [
            {text, #{label => ~"ok", text => latin1()}},
            {label, #{label => latin1(), text => ~"ok"}}
        ]
    ].

%% A command prop is a structured JSON value; the string nested in its payload
%% gets the same name.
native_command_prop_names_non_utf8(Config) when is_list(Config) ->
    ?assertEqual(
        {arizona_loc, {?VIEW, line_of(~"?get(cmd)")}, {non_utf8_render, 16#E9, 3}},
        raised(fun() -> arizona_render:render_to_iolist(?VIEW:native_cmd(#{cmd => latin1()})) end)
    ),
    ?assertError(
        {non_utf8_render, 16#E9, 3},
        arizona_native:render_attr(
            ~"on_tap", arizona_android:push_event(~"go", #{~"v" => [latin1()]})
        )
    ).

refuse_renames_only_the_encoder_utf8_error(Config) when is_list(Config) ->
    Term = #{~"o" => [[1, ~"az", <<"a", 255, "b">>]]},
    {Reason, ST} = caught(fun() -> json:encode(Term) end),
    ?assertError({non_utf8_render, 255, 1}, arizona_utf8:refuse(Term, Reason, ST)),
    %% Another failure keeps its own reason, even beside bad bytes.
    ?assertError(boom, arizona_utf8:refuse(Term, boom, [])),
    %% The encoder's reason with no bad bytes to name is not renamed either.
    ?assertError(unexpected_end, arizona_utf8:refuse(#{~"k" => ~"ok"}, unexpected_end, [])).

%% A list wrapping non-UTF-8 bytes is the same mistake as the binary alone, so it
%% gets the same name; an integer that is not a code point keeps
%% `bad_template_value`.
chardata_wrapping_non_utf8_binary(Config) when is_list(Config) ->
    ?assertError({non_utf8_render, 16#E9, 5}, arizona_template:to_bin([~"ab", latin1()])),
    ?assertError({bad_template_value, [$a, 16#110000]}, arizona_template:to_bin([$a, 16#110000])),
    ?assertEqual(~"日本 é", arizona_template:to_bin([~"日本", " é"])).

format_error_names_the_byte_and_the_fix(Config) when is_list(Config) ->
    {Reason, ST} =
        try
            arizona_utf8:refuse(latin1(), unexpected_end, [])
        catch
            error:R:S -> {R, S}
        end,
    ?assertEqual({non_utf8_render, 16#E9, 3}, Reason),
    [{arizona_utf8, _Fun, [Value], Info} | _] = ST,
    ?assertEqual(latin1(), Value),
    ?assertEqual(#{module => arizona_utf8}, proplists:get_value(error_info, Info)),
    Located = {arizona_loc, {my_view, 12}, Reason},
    Parts = [
        ~"not valid UTF-8",
        ~"16#E9 at offset 3",
        ~"<<99,97,102,233>>",
        ~"~\"...\"",
        ~"/utf8",
        ~"unicode:characters_to_binary"
    ],
    Missing = [
        {R, Part}
     || R <- [Reason, Located],
        Formatted <- [unicode:characters_to_binary(erl_error:format_exception(error, R, ST))],
        Part <- Parts,
        binary:match(Formatted, Part) =:= nomatch
    ],
    ?assertEqual([], Missing),
    LocatedMsg = unicode:characters_to_binary(erl_error:format_exception(error, Located, ST)),
    ?assertNotEqual(nomatch, binary:match(LocatedMsg, ~"my_view:12")).

%% --------------------------------------------------------------------
%% One classification per escaped value
%% --------------------------------------------------------------------

%% to_bin/1 matches the trusted shapes in its clause heads instead of asking
%% classify_trusted/1; for every shape its answer is the one the classify-first
%% implementation gave, and so is escape_value/2's on every backend.
to_bin_matches_the_classify_first_reference(Config) when is_list(Config) ->
    Inputs = [
        ~"é<&>",
        <<>>,
        42,
        -1,
        1.5,
        foo,
        true,
        {arizona_esc, "x<"},
        {arizona_esc, {arizona_raw, ~"<i>"}},
        {arizona_raw, "<b>é</b>"},
        {arizona_raw, {arizona_raw, ~"a"}},
        {arizona_raw, 7},
        arizona_js:toggle(~"#m"),
        [arizona_js:toggle(~"#a"), arizona_js:hide(~"#b")],
        "café",
        [~"a", "b", $c, [~"日本"]],
        [~"a-", true],
        [1114112],
        [],
        make_ref(),
        {x},
        #{}
    ],
    [
        ?assertEqual(
            {V, outcome(fun reference_to_bin/1, V)}, {V, outcome(fun arizona_template:to_bin/1, V)}
        )
     || V <- Inputs
    ],
    [
        ?assertEqual(
            {B, V, outcome(fun(X) -> reference_escape_value(B, X) end, V)},
            {B, V, outcome(fun(X) -> arizona_template:escape_value(B, X) end, V)}
        )
     || B <- [arizona_html, arizona_terminal, arizona_native], V <- Inputs
    ].

%% On main, escaping a list value classified it twice: escape_value/2 asked, then
%% to_bin/1 asked again. A rendered attribute value took the same detour.
escape_classifies_a_value_once(Config) when is_list(Config) ->
    MFA = {arizona_template, classify_trusted, 1},
    1 = erlang:trace_pattern(MFA, true, [local, call_count]),
    try
        Count = fun(Fun) ->
            1 = erlang:trace_pattern(MFA, restart, [local, call_count]),
            ?assert(is_binary(Fun())),
            {call_count, N} = erlang:trace_info(MFA, call_count),
            N
        end,
        ?assertEqual(1, Count(fun() -> arizona_template:escape_value(arizona_html, "café") end)),
        ?assertEqual(1, Count(fun() -> arizona_html:render_attr(~"title", "café") end)),
        ?assertEqual(0, Count(fun() -> arizona_template:escape_marked(arizona_html, "café") end))
    after
        erlang:trace_pattern(MFA, false, [local, call_count])
    end.

%% --------------------------------------------------------------------
%% Dev boundary check (HTTP)
%% --------------------------------------------------------------------

%% Production runs none of the check: the bytes are served as they are.
prod_http_serves_bytes_unchecked(Config) when is_list(Config) ->
    persistent_term:erase(arizona_reload_url),
    Req = arizona_req_test_adapter:new(#{}),
    {ok, 200, _Type, Page, _} = arizona_http:render(?VIEW, Req, #{
        bindings => #{content => latin1()}
    }),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Page), latin1())).

dev_http_locates_every_shape(Config) when is_list(Config) ->
    with_dev(fun() ->
        [
            ?assertEqual(
                {Key, {arizona_loc, {?VIEW, line_of(Needle)}, {non_utf8_render, 16#E9, 3}}},
                {Key,
                    raised(fun() ->
                        arizona_utf8:render_view(?VIEW, #{bindings => bad_bindings(Key)})
                    end)}
            )
         || {Key, Needle} <- [
                {content, ~"?get(content)"},
                {attr, ~"?get(attr)"},
                {raw, ~"?get(raw))"},
                {raw_attr, ~"?get(raw_attr)"},
                {items, ~"?get(items)"},
                {child, ~"{em, [], [?get(text)]}"}
            ]
        ],
        %% A `?stateful` child's value, located in the child's own template.
        ?assertEqual(
            {arizona_loc,
                {arizona_utf8_mount_child, line_of(arizona_utf8_mount_child, ~"?get(text)")},
                {non_utf8_render, 16#E9, 3}},
            raised(fun() ->
                arizona_utf8:render_view(arizona_utf8_mount_page, #{
                    bindings => #{notify => self(), child_text => latin1()}
                })
            end)
        )
    end).

%% The dev check renders the page once, values checked as they render, so a
%% failing page's mount/1 and its stateful child's run exactly once, as in
%% production -- a re-render to locate the bytes would run their side effects
%% twice.
dev_http_mounts_once(Config) when is_list(Config) ->
    Page = {mounted, arizona_utf8_mount_page},
    Child = {mounted, arizona_utf8_mount_child},
    with_dev(fun() ->
        [
            begin
                Req = arizona_req_test_adapter:new(#{}),
                Result = arizona_http:render(arizona_utf8_mount_page, Req, #{
                    bindings => Bindings#{notify => self()}
                }),
                ?assertEqual({Bindings, Status}, {Bindings, element(2, Result)}),
                ?assertEqual({Bindings, Mounts}, {Bindings, drain_mounts()})
            end
         || {Bindings, Status, Mounts} <- [
                %% The page's own slot fails first, so the child is never reached.
                {#{text => latin1()}, 500, [Page]},
                {#{child_text => latin1()}, 500, [Page, Child]},
                {#{}, 200, [Page, Child]}
            ]
        ]
    end).

%% Checking the values changes nothing they render to: the stand-in for a stateful
%% child still mounts it and marks it a view boundary.
dev_render_matches_prod_bytes(Config) when is_list(Config) ->
    [
        ?assertEqual(
            {Mod, iolist_to_binary(element(2, arizona_render:render_view(Mod, Opts)))},
            {Mod, iolist_to_binary(element(2, arizona_utf8:render_view(Mod, Opts)))}
        )
     || {Mod, Opts} <- [
            {?VIEW, #{}},
            {arizona_utf8_mount_page, #{bindings => #{notify => self()}}},
            {arizona_utf8_static_view, #{bindings => #{open => true}}}
        ]
    ],
    ?assertEqual(
        [{mounted, arizona_utf8_mount_page}, {mounted, arizona_utf8_mount_child}] ++
            [{mounted, arizona_utf8_mount_page}, {mounted, arizona_utf8_mount_child}],
        drain_mounts()
    ).

dev_http_serves_the_located_error_page(Config) when is_list(Config) ->
    with_dev(fun() ->
        Req = arizona_req_test_adapter:new(#{}),
        {error, 500, _Type, Body, _} = arizona_http:render(?VIEW, Req, #{
            bindings => #{content => latin1()}
        }),
        Bin = iolist_to_binary(Body),
        ?assertNotEqual(nomatch, binary:match(Bin, ~"not valid UTF-8")),
        ?assertNotEqual(nomatch, binary:match(Bin, ~"unicode:characters_to_binary"))
    end).

dev_http_passes_valid_utf8(Config) when is_list(Config) ->
    with_dev(fun() ->
        Req = arizona_req_test_adapter:new(#{}),
        {ok, 200, _Type, Page, _} = arizona_http:render(?VIEW, Req, #{}),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Page), ~"日本"))
    end).

%% --------------------------------------------------------------------
%% WebSocket frames
%% --------------------------------------------------------------------

%% The diff ships values raw, so the socket's JSON encoder is the first to see
%% them. Its refusal is renamed and the session closes 4500 as before.
socket_frame_failure_is_named(Config) when is_list(Config) ->
    persistent_term:erase(arizona_reload_url),
    [
        ?assertEqual({Key, {non_utf8_render, 16#E9, 3}}, {Key, socket_crash_reason(Key)})
     || Key <- [~"content", ~"effect", ~"live"]
    ].

%% In dev mode the live views are rendered again to name the template line.
dev_socket_frame_failure_is_located(Config) when is_list(Config) ->
    with_dev(fun() ->
        [
            ?assertEqual(
                {Key, {arizona_loc, {?VIEW, line_of(Needle)}, {non_utf8_render, 16#E9, 3}}},
                {Key, socket_crash_reason(Key)}
            )
         || {Key, Needle} <- [
                {~"content", ~"?get(content)"},
                {~"attr", ~"?get(attr)"},
                {~"items", ~"?get(items)"},
                {~"child", ~"{em, [], [?get(text)]}"},
                {~"effect", ~"?get(effect)"},
                %% Rendered only when connected: the re-render must see the live
                %% process's connection state, not the socket process's. A value
                %% from a conditional is located at the conditional's slot.
                {~"live", ~"case ?connected of"}
            ]
        ]
    end).

%% A reconnect's connect frame is encoded inside init/4, before the socket holds a
%% live pid to locate the line from; the production log still names the view.
connect_frame_failure_log_names_the_view(Config) when is_list(Config) ->
    persistent_term:erase(arizona_reload_url),
    HandlerId = arizona_utf8_suite_init_log,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => error, config => #{pid => self()}
    }),
    try
        Req = arizona_req_test_adapter:new(),
        ?assertMatch(
            {close, 4501, ~"mount failed", _},
            arizona_socket:init(?VIEW, #{content => latin1()}, Req, #{reconnect => true})
        ),
        Text =
            receive
                {arizona_test_log_handler, #{msg := {Format, Args}}} ->
                    unicode:characters_to_binary(io_lib:format(Format, Args))
            after 1000 -> error(no_crash_log)
            end,
        [
            ?assertNotEqual(nomatch, binary:match(Text, Part), Part)
         || Part <- [atom_to_binary(?VIEW), ~"non_utf8_render", ~"not valid UTF-8"]
        ]
    after
        ok = logger:remove_handler(HandlerId)
    end.

%% --------------------------------------------------------------------
%% Effect commands
%% --------------------------------------------------------------------

%% An effect command is JSON-encoded where it is rendered (an attribute at SSR, an
%% op value); a string argument that is not UTF-8 is named like any other refusal.
effect_command_names_non_utf8(Config) when is_list(Config) ->
    Cmd = arizona_js:push_event(~"x", #{v => <<233>>}),
    ?assertError({non_utf8_render, 233, 0}, arizona_template:to_bin(Cmd)),
    ?assertError({non_utf8_render, 233, 0}, arizona_effect:encode_json(Cmd)),
    ?assertError({non_utf8_render, 233, 0}, arizona_html:render_attr(~"az-click", Cmd)),
    ?assertEqual(
        {arizona_loc, {?VIEW, line_of(~"?get(effect)")}, {non_utf8_render, 16#E9, 3}},
        raised(fun() ->
            arizona_render:render_view_to_iolist(?VIEW, #{bindings => #{effect => latin1()}})
        end)
    ).

%% The dev render stands in for a stateful child's handler; a child that rewrites
%% its restricted `id` must fail exactly as it does in production, naming the child.
dev_child_restricted_key_error_matches_prod(Config) when is_list(Config) ->
    Page = arizona_utf8_rekey_page,
    Prod = caught(fun() -> arizona_render:render_view(Page, #{}) end),
    Dev = caught(fun() -> arizona_utf8:render_view(Page, #{}) end),
    ?assertMatch({{arizona_loc, {Page, _}, restricted_key_modified}, _}, Prod),
    ?assertEqual(element(1, Prod), element(1, Dev)),
    [ProdTop | _] = element(2, Prod),
    [DevTop | _] = element(2, Dev),
    ?assertMatch({arizona_eval, _, [id, arizona_utf8_rekey_child], _}, ProdTop),
    ?assertEqual(ProdTop, DevTop).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

latin1() ->
    ?VIEW:latin1().

bad_bindings(items) -> #{items => [~"ok", latin1()]};
bad_bindings(Key) -> #{Key => latin1()}.

raised(Fun) ->
    try Fun() of
        Result -> error({no_crash, Result})
    catch
        error:Reason -> Reason
    end.

caught(Fun) ->
    try Fun() of
        Result -> error({no_crash, Result})
    catch
        error:Reason:ST -> {Reason, ST}
    end.

outcome(Fun, V) ->
    try Fun(V) of
        Bin -> {ok, Bin}
    catch
        error:Reason -> {error, Reason}
    end.

%% to_bin/1 as it was before it matched the trusted shapes itself.
reference_to_bin(V) when is_binary(V) -> V;
reference_to_bin(V) when is_integer(V) -> integer_to_binary(V);
reference_to_bin(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
reference_to_bin(V) when is_atom(V) -> atom_to_binary(V);
reference_to_bin({arizona_esc, V}) ->
    reference_to_bin(V);
reference_to_bin(V0) ->
    case arizona_template:classify_trusted(V0) of
        {raw, V} ->
            reference_to_bin(V);
        {effect, Cmd} ->
            arizona_effect:encode(Cmd);
        value when is_list(V0) ->
            try unicode:characters_to_binary(V0) of
                Bin when is_binary(Bin) -> Bin;
                _ -> error({bad_template_value, V0})
            catch
                error:badarg -> error({bad_template_value, V0})
            end;
        value ->
            error({bad_template_value, V0})
    end.

reference_escape_value(Backend, V0) ->
    case arizona_template:classify_trusted(V0) of
        {raw, V} -> reference_to_bin(V);
        {effect, Cmd} -> reference_to_bin(Cmd);
        value -> Backend:escape(reference_to_bin(V0))
    end.

line_of(Needle) ->
    line_of(?VIEW, Needle).

line_of(Mod, Needle) ->
    Source = proplists:get_value(source, Mod:module_info(compile)),
    {ok, Src} = file:read_file(Source),
    Lines = binary:split(Src, ~"\n", [global]),
    [Line | _] = [
        N
     || {N, L} <- lists:zip(lists:seq(1, length(Lines)), Lines), binary:match(L, Needle) =/= nomatch
    ],
    Line.

marked_lines(Lines, Marker) ->
    [
        N
     || {N, L} <- lists:zip(lists:seq(1, length(Lines)), Lines), binary:match(L, Marker) =/= nomatch
    ].

warned_lines(Warnings) ->
    lists:usort([
        L
     || {_File, Ws} <- Warnings, {{L, _Col}, arizona_parse_transform, latin1_binary_literal} <- Ws
    ]).

compile_probe(Config, Lines) ->
    Dir = proplists:get_value(priv_dir, Config),
    [ModLine | _] = Lines,
    {match, [Name]} = re:run(ModLine, "-module\\((\\w+)\\)", [{capture, all_but_first, list}]),
    File = filename:join(Dir, Name ++ ".erl"),
    ok = file:write_file(File, lists:join(~"\n", Lines)),
    Include = filename:join(code:lib_dir(arizona), "include"),
    case compile:file(File, [binary, return, {i, Include}]) of
        {ok, _Mod, _Bin, Warnings} -> {ok, Warnings};
        Error -> Error
    end.

drain_mounts() ->
    receive
        {mounted, _} = Mounted -> [Mounted | drain_mounts()]
    after 0 -> []
    end.

with_dev(Fun) ->
    persistent_term:put(arizona_reload_url, ~"/arizona/reload"),
    try
        Fun()
    after
        persistent_term:erase(arizona_reload_url)
    end.

%% The reason the socket logs when the frame for `break`ing `Key` fails to encode.
socket_crash_reason(Key) ->
    HandlerId = arizona_utf8_suite_log,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => error, config => #{pid => self()}
    }),
    try
        Req = arizona_req_test_adapter:new(),
        {reply, _, Socket0} = arizona_socket:init(?VIEW, #{}, Req, #{}),
        Frame = iolist_to_binary(json:encode([~"nu8", ~"break", #{~"key" => Key}])),
        ?assertMatch({close, 4500, ~"server crash", _}, arizona_socket:handle_in(Frame, Socket0)),
        receive
            {arizona_test_log_handler, #{msg := {_Format, [Reason | _]}}} -> Reason
        after 1000 -> error(no_crash_log)
        end
    after
        ok = logger:remove_handler(HandlerId)
    end.
