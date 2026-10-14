-module(arizona_non_utf8_SUITE).
-include_lib("stdlib/include/assert.hrl").

%% Every render target writes UTF-8 -- HTML is served as `text/html; charset=utf-8`,
%% a WebSocket frame and a `?native` document are JSON -- so a binary in another
%% encoding is refused where it is rendered, as `{non_utf8_render, Byte, Offset}`,
%% located at its template line. Once named per backend (verbatim mojibake for
%% `?html`/`?terminal`, a json `unexpected_end` for `?native`), and on the live path
%% a crash inside the socket's JSON encoder, far from the template.

-export([all/0, groups/0]).
-export([
    ssr_rejects_every_html_shape/1,
    ssr_rejects_terminal/1,
    ssr_rejects_native/1,
    ssr_accepts_valid_utf8/1,
    live_mount_rejects_every_shape/1,
    live_diff_rejects_every_shape/1,
    live_render_rejects_other_targets/1,
    escape_reports_offset_past_metacharacters/1,
    escape_keeps_valid_utf8_byte_for_byte/1,
    escape_value_rejects_both_legs/1,
    chardata_wrapping_non_utf8_binary/1,
    format_error_names_the_fix/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [parallel], [
            ssr_rejects_every_html_shape,
            ssr_rejects_terminal,
            ssr_rejects_native,
            ssr_accepts_valid_utf8,
            live_mount_rejects_every_shape,
            live_diff_rejects_every_shape,
            live_render_rejects_other_targets,
            escape_reports_offset_past_metacharacters,
            escape_keeps_valid_utf8_byte_for_byte,
            escape_value_rejects_both_legs,
            chardata_wrapping_non_utf8_binary,
            format_error_names_the_fix
        ]}
    ].

-define(VIEW, arizona_non_utf8_view).

%% `<<"caf", 16#E9>>`: the first byte that does not decode is 0xE9, at offset 3.
-define(REASON(Case), {arizona_loc, {?VIEW, ?VIEW:line(Case)}, {non_utf8_render, 16#E9, 3}}).

html_cases() ->
    [content, attr, raw, raw_attr, items].

%% --------------------------------------------------------------------
%% SSR
%% --------------------------------------------------------------------

ssr_rejects_every_html_shape(Config) when is_list(Config) ->
    [
        ?assertEqual({Case, ?REASON(Case)}, {Case, ssr_reason(bad_bindings(Case))})
     || Case <- html_cases()
    ].

ssr_rejects_terminal(Config) when is_list(Config) ->
    ?assertEqual(
        ?REASON(terminal),
        raised(fun() -> arizona_render:render_to_iolist(?VIEW:terminal(bad())) end)
    ).

%% The JSON encoder already refused these bytes, but as `unexpected_end`, which
%% names neither the value nor the cause.
ssr_rejects_native(Config) when is_list(Config) ->
    [
        ?assertEqual(
            {Fun, ?REASON(Fun)},
            {Fun, raised(fun() -> arizona_render:render_to_iolist(?VIEW:Fun(bad())) end)}
        )
     || Fun <- [native, native_attr]
    ].

%% Multibyte text (2-, 3- and 4-byte sequences) renders byte for byte on every
%% target, escaped where the target escapes.
ssr_accepts_valid_utf8(Config) when is_list(Config) ->
    HTML = iolist_to_binary(arizona_render:render_view_to_iolist(?VIEW, #{})),
    [
        ?assertNotEqual(nomatch, binary:match(HTML, Part), Part)
     || Part <- [~"café", ~"title=\"naïve\"", ~"<b>ünï</b>", ~"href=\"/ü\"", ~"日本"]
    ],
    Text = ~"é 日本 😀 <&>",
    %% The terminal line closes with its own reset and newline after the text.
    Terminal = iolist_to_binary(arizona_render:render_to_iolist(?VIEW:terminal(#{text => Text}))),
    ?assertMatch(<<"é 日本 😀 <&>"/utf8, _/binary>>, Terminal),
    Native = iolist_to_binary(arizona_render:render_to_iolist(?VIEW:native(#{text => Text}))),
    ?assertMatch(#{~"children" := [#{~"children" := [Text]}]}, json:decode(Native)),
    NativeAttr = iolist_to_binary(
        arizona_render:render_to_iolist(?VIEW:native_attr(#{text => Text}))
    ),
    ?assertMatch(#{~"label" := Text}, json:decode(NativeAttr)).

%% --------------------------------------------------------------------
%% Live
%% --------------------------------------------------------------------

%% The live mount evaluates every slot without rendering a document (a connected
%% mount ships the fingerprint payload), and the diff ships values raw for the
%% client to text-node, so no escaper sees them before the socket's JSON encoder.
%% The live process raises the SSR error, at the same line, rather than a crash in
%% that encoder after the frame left the template behind.
live_mount_rejects_every_shape(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    [
        ?assertEqual(
            {Case, ?REASON(Case)},
            {Case, live_reason(bad_bindings(Case), fun(_Pid) -> ok end)}
        )
     || Case <- html_cases()
    ].

live_diff_rejects_every_shape(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    [
        ?assertEqual(
            {Case, ?REASON(Case)},
            {Case,
                live_reason(#{}, fun(Pid) ->
                    arizona_live:handle_event(
                        Pid, ~"nu8", ~"break", #{~"key" => atom_to_binary(Case)}
                    )
                end)}
        )
     || Case <- html_cases()
    ].

%% The terminal and native live renders, mounted and diffed.
live_render_rejects_other_targets(Config) when is_list(Config) ->
    [
        begin
            ?assertEqual(
                {Fun, ?REASON(Fun)},
                {Fun, raised(fun() -> arizona_render:render(?VIEW:Fun(bad()), #{}) end)}
            ),
            {_HTML, Snap, _Views} = arizona_render:render(?VIEW:Fun(#{text => ~"ok"}), #{}),
            ?assertEqual(
                {Fun, ?REASON(Fun)},
                {Fun, raised(fun() -> arizona_diff:diff(?VIEW:Fun(bad()), Snap, #{}) end)}
            )
        end
     || Fun <- [terminal, native, native_attr]
    ].

%% --------------------------------------------------------------------
%% Escapers
%% --------------------------------------------------------------------

%% The escapers stop their ASCII scan at the first non-ASCII byte and check the
%% rest in one pass, so the offset counts from the value's start however much
%% valid text, multibyte or escaped, precedes the bad byte.
escape_reports_offset_past_metacharacters(Config) when is_list(Config) ->
    %% "a<é" + 0xFF: 'a' '<' then a 2-byte é, so 0xFF sits at offset 4.
    Value = <<"a<", 16#C3, 16#A9, 16#FF, "tail">>,
    [
        ?assertError({non_utf8_render, 16#FF, 4}, Backend:escape(Value))
     || Backend <- [arizona_html, arizona_terminal, arizona_native]
    ],
    %% A metacharacter after the bad byte, and one in a long value that takes the
    %% sliced-runs path: still found, still at its offset.
    Long = <<(binary:copy(~"x", 40))/binary, "&", 16#80, (binary:copy(~"y", 40))/binary>>,
    [
        ?assertError({non_utf8_render, 16#80, 41}, Backend:escape(Long))
     || Backend <- [arizona_html, arizona_terminal, arizona_native]
    ],
    %% Truncated, overlong and surrogate sequences are all refused.
    [
        ?assertError({non_utf8_render, Byte, 1}, arizona_html:escape(<<"a", Bad/binary>>))
     || {Byte, Bad} <- [
            {16#E6, <<16#E6, 16#97>>},
            {16#C0, <<16#C0, 16#80>>},
            {16#ED, <<16#ED, 16#A0, 16#80>>}
        ]
    ].

escape_keeps_valid_utf8_byte_for_byte(Config) when is_list(Config) ->
    Clean = ~"naïve 日本 😀",
    ?assertEqual(Clean, arizona_html:escape(Clean)),
    ?assertEqual(Clean, arizona_terminal:escape(Clean)),
    ?assertEqual(~"é&lt;日&amp;😀&gt;", arizona_html:escape(~"é<日&😀>")),
    ?assertEqual(~"é日😀", arizona_terminal:escape(<<"é"/utf8, 27, "日"/utf8, 7, "😀"/utf8>>)),
    %% Integers need no check and pass the boundary as before.
    ?assertEqual(~"42", arizona_template:escape_value(arizona_html, 42)).

escape_value_rejects_both_legs(Config) when is_list(Config) ->
    Bad = bad_value(),
    [
        begin
            ?assertError({non_utf8_render, 16#E9, 3}, arizona_template:escape_value(Backend, Bad)),
            ?assertError(
                {non_utf8_render, 16#E9, 3},
                arizona_template:escape_value(Backend, arizona_template:raw(Bad))
            ),
            ?assertError(
                {non_utf8_render, 16#E9, 3}, arizona_template:escape_marked(Backend, Bad)
            )
        end
     || Backend <- [arizona_html, arizona_terminal, arizona_native]
    ],
    ?assertError({non_utf8_render, 16#E9, 3}, arizona_html:render_attr(~"title", Bad)),
    ?assertError(
        {non_utf8_render, 16#E9, 3}, arizona_html:render_attr(~"href", arizona_template:raw(Bad))
    ),
    ?assertError({non_utf8_render, 16#E9, 3}, arizona_native:render_attr(~"label", Bad)).

%% A list wrapping the same bytes is the same mistake, so it gets the same name;
%% an integer that is not a code point is not an encoding problem and keeps
%% `bad_template_value`.
chardata_wrapping_non_utf8_binary(Config) when is_list(Config) ->
    ?assertError({non_utf8_render, 16#E9, 5}, arizona_template:to_bin([~"ab", bad_value()])),
    ?assertError({bad_template_value, [$a, 16#110000]}, arizona_template:to_bin([$a, 16#110000])).

format_error_names_the_fix(Config) when is_list(Config) ->
    Stack =
        try arizona_html:escape(bad_value()) of
            _ -> ct:fail(expected_non_utf8_render)
        catch
            error:{non_utf8_render, _, _}:ST -> ST
        end,
    [{arizona_template, _Fun, [Value | _], Info} | _] = Stack,
    ?assertEqual(bad_value(), Value),
    ?assertEqual(#{module => arizona_template}, proplists:get_value(error_info, Info)),
    Formatted = unicode:characters_to_binary(
        erl_error:format_exception(error, {non_utf8_render, 16#E9, 3}, Stack)
    ),
    [
        ?assertNotEqual(nomatch, binary:match(Formatted, Part), Part)
     || Part <- [
            ~"not valid UTF-8",
            ~"16#E9 at offset 3",
            ~"<<99,97,102,233>>",
            ~"~\"...\"",
            ~"/utf8",
            ~"unicode:characters_to_binary"
        ]
    ].

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

bad_value() ->
    ?VIEW:latin1().

bad() ->
    #{text => bad_value()}.

bad_bindings(items) -> #{items => [~"ok", bad_value()]};
bad_bindings(Case) -> #{Case => bad_value()}.

ssr_reason(Bindings) ->
    raised(fun() -> arizona_render:render_view_to_iolist(?VIEW, #{bindings => Bindings}) end).

raised(Fun) ->
    try Fun() of
        Result -> error({no_crash, Result})
    catch
        error:Reason -> Reason
    end.

%% The reason the live process exits with once `Then` runs after mount, with the
%% stacktrace dropped.
live_reason(Bindings, Then) ->
    {ok, Pid} = arizona_live:start_link(?VIEW, Bindings, undefined, []),
    Reason =
        try
            {ok, _, _} = arizona_live:mount(Pid),
            Then(Pid)
        of
            Result -> error({no_crash, Result})
        catch
            exit:{{R, ST}, {gen_server, call, _}} when is_list(ST) -> R
        end,
    receive
        {'EXIT', Pid, _} -> Reason
    after 1000 -> ct:fail(no_exit)
    end.
