-module(arizona_dev_mcp_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    tools_list/1,
    route_bounds_sessions/1,
    list_routes_lists_mcp_route/1,
    list_routes_formats_shapes/1,
    list_routes_env_unset/1,
    get_logs_returns_captured_output/1,
    get_logs_rejects_unknown_level/1,
    get_logs_rejects_non_integer_tail/1,
    get_logs_rejects_non_string_grep/1,
    get_logs_rejects_invalid_grep/1,
    get_logs_reports_no_matches/1,
    get_logs_emits_a_cursor_naming_its_argument/1,
    get_logs_rejects_negative_since/1,
    describe_component_stateful/1,
    describe_component_stateless/1,
    describe_component_other/1,
    describe_component_not_loaded/1,
    describe_component_non_ascii_docs/1,
    get_module_docs/1,
    get_function_docs/1,
    get_docs_unknown_function/1,
    get_source_location_module/1,
    get_source_location_function/1,
    get_source_location_no_line_info/1,
    get_source_location_unknown_function/1,
    reloader_status_ok/1,
    reloader_status_reports_stale_modules/1,
    reload_ok_reports_no_agent/1,
    reload_syncs_stale_module/1,
    app_info_reports_version/1,
    eval_returns_value/1,
    eval_appends_dot/1,
    eval_bindings_persist/1,
    eval_error_is_in_band/1,
    eval_result_is_utf8/1,
    eval_result_utf8_over_crash_band/1,
    eval_source_is_decoded_as_utf8/1,
    eval_source_roundtrips_non_ascii/1,
    render_component_stateful/1,
    render_component_stateless/1,
    render_component_other/1,
    render_component_bad_bindings/1,
    render_component_unknown_key/1
]).

all() ->
    [
        tools_list,
        route_bounds_sessions,
        list_routes_lists_mcp_route,
        list_routes_formats_shapes,
        list_routes_env_unset,
        get_logs_returns_captured_output,
        get_logs_rejects_unknown_level,
        get_logs_rejects_non_integer_tail,
        get_logs_rejects_non_string_grep,
        get_logs_rejects_invalid_grep,
        get_logs_reports_no_matches,
        get_logs_emits_a_cursor_naming_its_argument,
        get_logs_rejects_negative_since,
        describe_component_stateful,
        describe_component_stateless,
        describe_component_other,
        describe_component_not_loaded,
        describe_component_non_ascii_docs,
        get_module_docs,
        get_function_docs,
        get_docs_unknown_function,
        get_source_location_module,
        get_source_location_function,
        get_source_location_no_line_info,
        get_source_location_unknown_function,
        reloader_status_ok,
        reloader_status_reports_stale_modules,
        reload_ok_reports_no_agent,
        reload_syncs_stale_module,
        app_info_reports_version,
        eval_returns_value,
        eval_appends_dot,
        eval_bindings_persist,
        eval_error_is_in_band,
        eval_result_is_utf8,
        eval_result_utf8_over_crash_band,
        eval_source_is_decoded_as_utf8,
        eval_source_roundtrips_non_ascii,
        render_component_stateful,
        render_component_stateless,
        render_component_other,
        render_component_bad_bindings,
        render_component_unknown_key
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(_Config) ->
    _ = application:stop(arizona),
    ok.

init_per_testcase(_Case, Config) ->
    %% list_routes reads the `server` app env; seed it with the dev server routes.
    ok = application:set_env(arizona, server, #{routes => arizona_test_server:routes()}),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = application:unset_env(arizona, server),
    ok.

%% --------------------------------------------------------------------
%% tools
%% --------------------------------------------------------------------

%% All tools are advertised, including eval -- it is always available; the dev
%% route's localhost-only gate (not a per-tool switch) is what keeps it safe.
tools_list(_Config) ->
    {ok, _, _, DefaultState} = arizona_dev_mcp:init(#{}),
    Names = [maps:get(name, Tool) || Tool <- arizona_dev_mcp:tools(DefaultState)],
    Expected = [
        ~"list_routes",
        ~"describe_component",
        ~"get_docs",
        ~"get_source_location",
        ~"reloader_status",
        ~"get_logs",
        ~"app_info",
        ~"render_component",
        ~"eval",
        ~"reload"
    ],
    ?assertEqual(lists:sort(Expected), lists:sort(Names)).

%% The dev route bounds concurrent sessions (M2) so a client cannot pin unbounded
%% memory by looping `initialize`, and keeps session mode on; an explicit
%% override wins.
route_bounds_sessions(_Config) ->
    ?assertMatch(
        {mcp, ~"/mcp", arizona_dev_mcp, #{
            sessions := true, max_sessions := 32, allow_remote_access := false
        }},
        arizona_dev_mcp:route(~"/mcp")
    ),
    {mcp, _, _, Opts} = arizona_dev_mcp:route(~"/mcp", #{max_sessions => 4}),
    ?assertEqual(4, maps:get(max_sessions, Opts)).

%% --------------------------------------------------------------------
%% list_routes
%% --------------------------------------------------------------------

list_routes_lists_mcp_route(_Config) ->
    {reply, Text, _} = call(~"list_routes", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_dev_mcp")),
    ?assertMatch({_, _}, binary:match(Text, ~"/mcp")).

list_routes_formats_shapes(_Config) ->
    {reply, Text, _} = call(~"list_routes", #{}),
    %% A 4-tuple (live), and the two 3-tuple shapes (ws, asset).
    ?assertMatch({_, _}, binary:match(Text, ~"live")),
    ?assertMatch({_, _}, binary:match(Text, ~"ws")),
    ?assertMatch({_, _}, binary:match(Text, ~"asset")).

list_routes_env_unset(_Config) ->
    ok = application:unset_env(arizona, server),
    ?assertMatch({reply, ~"(no routes configured)", _}, call(~"list_routes", #{})).

%% --------------------------------------------------------------------
%% describe_component
%% --------------------------------------------------------------------

describe_component_stateful(_Config) ->
    {reply, Text, _} = call(~"describe_component", #{~"module" => ~"arizona_counter"}),
    ?assertMatch({_, _}, binary:match(Text, ~"stateful")),
    ?assertMatch({_, _}, binary:match(Text, ~"mount/1")),
    ?assertMatch({_, _}, binary:match(Text, ~"render/1")).

describe_component_stateless(_Config) ->
    {reply, Text, _} = call(~"describe_component", #{~"module" => ~"arizona_layout"}),
    ?assertMatch({_, _}, binary:match(Text, ~"stateless")),
    ?assertMatch({_, _}, binary:match(Text, ~"render/1")),
    ?assertEqual(nomatch, binary:match(Text, ~"mount")).

describe_component_other(_Config) ->
    %% arizona_pubsub is a plain module (no mount/1 or render/1).
    {reply, Text, _} = call(~"describe_component", #{~"module" => ~"arizona_pubsub"}),
    ?assertMatch({_, _}, binary:match(Text, ~"other")).

describe_component_not_loaded(_Config) ->
    Args = #{~"module" => ~"arizona_no_such_module_xyzzy"},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"describe_component", Args)).

describe_component_non_ascii_docs(_Config) ->
    %% describe_component renders the moduledoc through `~ts`, so its codepoints
    %% pass through fmt/2. Emitted as latin1 they become bare high bytes and the
    %% reply fails to encode -- the widest blast radius of the bug, since it
    %% needs only a module documented in a language with accents. (`get_docs`
    %% returns the doc binary verbatim and was never affected.)
    Args = #{~"module" => ~"arizona_accented_docs"},
    {reply, Text, _} = call(~"describe_component", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"Relatório")),
    ?assertMatch(<<_/binary>>, encode_json(Text)).

%% --------------------------------------------------------------------
%% get_docs
%% --------------------------------------------------------------------

get_module_docs(_Config) ->
    %% arizona_mcp has a substantial moduledoc mentioning MCP.
    {reply, Text, _} = call(~"get_docs", #{~"module" => ~"arizona_mcp"}),
    ?assertMatch({_, _}, binary:match(Text, ~"MCP")).

get_function_docs(_Config) ->
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"progress"},
    ?assertMatch({reply, Bin, _} when is_binary(Bin) andalso Bin =/= <<>>, call(~"get_docs", Args)).

get_docs_unknown_function(_Config) ->
    %% A function name that isn't a known atom reports no docs, in-band.
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"zzz_no_such_fn_zzz"},
    ?assertMatch({reply, ~"(no documentation for that function)", _}, call(~"get_docs", Args)).

%% --------------------------------------------------------------------
%% get_source_location
%% --------------------------------------------------------------------

get_source_location_module(_Config) ->
    {reply, Text, _} = call(~"get_source_location", #{~"module" => ~"arizona_counter"}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_counter.erl")).

get_source_location_function(_Config) ->
    %% arizona_mcp has a doc chunk, so the function's line is resolved.
    Args = #{~"module" => ~"arizona_mcp", ~"function" => ~"progress"},
    {reply, Text, _} = call(~"get_source_location", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_mcp.erl")),
    ?assertMatch({_, _}, binary:match(Text, ~"line")).

get_source_location_no_line_info(_Config) ->
    %% A known atom that isn't a function of the module: source file, no line.
    Args = #{~"module" => ~"arizona_counter", ~"function" => ~"start_link"},
    {reply, Text, _} = call(~"get_source_location", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_counter.erl")),
    ?assertMatch({_, _}, binary:match(Text, ~"no line info")).

get_source_location_unknown_function(_Config) ->
    %% A function name that isn't a known atom still returns the source file.
    Args = #{~"module" => ~"arizona_counter", ~"function" => ~"zzz_no_such_fn_zzz"},
    {reply, Text, _} = call(~"get_source_location", Args),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_counter.erl")),
    ?assertMatch({_, _}, binary:match(Text, ~"no such function")).

%% --------------------------------------------------------------------
%% reloader_status / app_info
%% --------------------------------------------------------------------

reloader_status_ok(_Config) ->
    ok = arizona_reloader:clear_error(),
    {reply, Text, _} = call(~"reloader_status", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"ok")),
    %% The ok leg vouches for both signals: no compile error AND no drift.
    ?assertMatch({_, _}, binary:match(Text, ~"no stale modules")).

%% A node serving stale code (loaded code differs from the beam on disk -- e.g.
%% a rebuild outside the watcher's eye) must be reported, not read "ok"; an
%% agent asking the dev node otherwise debugs against code that isn't running.
reloader_status_reports_stale_modules(_Config) ->
    ok = arizona_reloader:clear_error(),
    Dir = tmp_dir(),
    Mod = az_dev_mcp_stale,
    try
        {Mod, Beam} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
        %% Rewrite the beam on disk to a different version WITHOUT reloading.
        Bin2 = compile_only(Mod, versioned_src(Mod, 2), Dir),
        ok = file:write_file(Beam, Bin2),
        {reply, Text, _} = call(~"reloader_status", #{}),
        ?assertMatch({_, _}, binary:match(Text, ~"stale")),
        ?assertMatch({_, _}, binary:match(Text, ~"az_dev_mcp_stale")),
        %% Re-sync loaded code to disk so later tests see no drift.
        {module, Mod} = code:load_binary(Mod, Beam, Bin2),
        {reply, OkText, _} = call(~"reloader_status", #{}),
        ?assertMatch({_, _}, binary:match(OkText, ~"no stale modules"))
    after
        _ = file:del_dir_r(Dir)
    end.

%% CT does not run under `rebar3 shell`, so the forced sync reports the
%% source-recompilation gap (no rebar_agent) while confirming loaded code is
%% already in sync with the beams on disk.
reload_ok_reports_no_agent(_Config) ->
    {reply, Text, _} = call(~"reload", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"ok")),
    ?assertMatch({_, _}, binary:match(Text, ~"no rebar_agent")).

%% The thing an agent wants after reloader_status reports drift: force the
%% loaded code back in sync with the beams on disk, and say what was reloaded.
reload_syncs_stale_module(_Config) ->
    Dir = tmp_dir(),
    Mod = az_dev_mcp_reload,
    try
        {Mod, Beam} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
        ?assertEqual(1, Mod:value()),
        Bin2 = compile_only(Mod, versioned_src(Mod, 2), Dir),
        ok = file:write_file(Beam, Bin2),
        {reply, Text, _} = call(~"reload", #{}),
        ?assertMatch({_, _}, binary:match(Text, ~"reloaded")),
        ?assertMatch({_, _}, binary:match(Text, ~"az_dev_mcp_reload")),
        %% The disk version is now the running code.
        ?assertEqual(2, Mod:value()),
        %% And the status tool agrees the drift is gone.
        {reply, StatusText, _} = call(~"reloader_status", #{}),
        ?assertMatch({_, _}, binary:match(StatusText, ~"no stale modules"))
    after
        _ = file:del_dir_r(Dir)
    end.

%% The tool an agent reaches for when a live view crashed: it has no terminal,
%% so this is the only path to the report that names the module and line.
get_logs_returns_captured_output(_Config) ->
    ok = arizona_dev_log:install(),
    Token = ~"devmcp-logs-token-end",
    Self = self(),
    %% From an unmarked process: handle_tool/4 marks its caller, and a marked
    %% writer is excluded by design.
    _Pid = spawn(fun() ->
        logger:error("boom ~ts", [Token]),
        Self ! logged
    end),
    receive
        logged -> ok
    after 5000 -> ct:fail(never_logged)
    end,
    {reply, Text, _} = call(~"get_logs", #{~"grep" => Token}),
    ?assertMatch({_, _}, binary:match(Text, Token)).

get_logs_rejects_unknown_level(_Config) ->
    %% Agent input, so it answers in-band and names the accepted set rather than
    %% crashing the dispatch.
    {error, Message, _} = call(~"get_logs", #{~"level" => ~"loud"}),
    ?assertMatch({_, _}, binary:match(Message, ~"unknown level")),
    ?assertMatch({_, _}, binary:match(Message, ~"emergency")).

get_logs_rejects_non_integer_tail(_Config) ->
    %% A silently-ignored bad argument is worse than a rejected one: the agent
    %% would get the default 50 entries and no signal that its `tail` was
    %% dropped. JSON has no integer/string distinction agents reliably honour.
    {error, Message, _} = call(~"get_logs", #{~"tail" => ~"50"}),
    ?assertMatch({_, _}, binary:match(Message, ~"positive integer")).

get_logs_rejects_non_string_grep(_Config) ->
    {error, Message, _} = call(~"get_logs", #{~"grep" => 7}),
    ?assertMatch({_, _}, binary:match(Message, ~"must be a string")).

get_logs_rejects_invalid_grep(_Config) ->
    {error, Message, _} = call(~"get_logs", #{~"grep" => ~"[oops"}),
    ?assertMatch({_, _}, binary:match(Message, ~"invalid grep pattern")).

get_logs_reports_no_matches(_Config) ->
    %% An empty reply would read as a broken tool; say it in words instead. The
    %% cursor still rides along so a polling loop keeps advancing.
    {reply, Text, _} = call(~"get_logs", #{~"grep" => ~"zzz-no-such-entry-zzz"}),
    ?assertMatch({0, _}, binary:match(Text, ~"(no matching log entries)")),
    ?assertMatch({_, _}, binary:match(Text, ~"[cursor: ")).

get_logs_emits_a_cursor_naming_its_argument(_Config) ->
    %% The reply is the only place an agent can discover the cursor exists, so it
    %% has to name the argument that consumes it.
    {reply, Text, _} = call(~"get_logs", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"[cursor: ")),
    ?assertMatch({_, _}, binary:match(Text, ~"since")).

get_logs_rejects_negative_since(_Config) ->
    {error, Message, _} = call(~"get_logs", #{~"since" => -1}),
    ?assertMatch({_, _}, binary:match(Message, ~"non-negative integer")).

app_info_reports_version(_Config) ->
    {reply, Text, _} = call(~"app_info", #{}),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona")),
    ?assertMatch({_, _}, binary:match(Text, ~"OTP")).

%% --------------------------------------------------------------------
%% eval
%% --------------------------------------------------------------------

eval_returns_value(_Config) ->
    ?assertMatch({reply, ~"3", _}, call(~"eval", #{~"code" => ~"1 + 2."})).

eval_appends_dot(_Config) ->
    %% Code without a trailing dot still evaluates.
    ?assertMatch({reply, ~"3", _}, call(~"eval", #{~"code" => ~"1 + 2"})).

eval_bindings_persist(_Config) ->
    {reply, _, State1} = call(~"eval", #{~"code" => ~"X = 41."}, state()),
    ?assertMatch({reply, ~"42", _}, call(~"eval", #{~"code" => ~"X + 1."}, State1)).

eval_error_is_in_band(_Config) ->
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"eval", #{~"code" => ~"1 +."})).

eval_result_is_utf8(_Config) ->
    %% A correctly-encoded binary comes back as valid UTF-8. Emitted as latin1 it
    %% would be the lone byte 0xE1, which `json:encode/1` rejects on the response
    %% path -- closing the connection instead of replying.
    Code = ~"unicode:characters_to_binary([225]).",
    ?assertMatch({reply, ~"<<\"á\"/utf8>>", _}, call(~"eval", #{~"code" => Code})).

eval_result_utf8_over_crash_band(_Config) ->
    %% `~tp` renders a codepoint as a string character when it is =< 255
    %% (`io:printable_range()` defaults to latin1), so 128..255 is the whole band
    %% that used to emit a bare high byte. Every one of them must survive the
    %% encoder that the response path runs the result through.
    lists:foreach(
        fun(Codepoint) ->
            ListCode = <<"[", (integer_to_binary(Codepoint))/binary, "].">>,
            BinCode = <<"<<", (integer_to_binary(Codepoint))/binary, ">>.">>,
            {reply, ListText, _} = call(~"eval", #{~"code" => ListCode}),
            {reply, BinText, _} = call(~"eval", #{~"code" => BinCode}),
            ?assertMatch(<<_/binary>>, encode_json(ListText)),
            ?assertMatch(<<_/binary>>, encode_json(BinText))
        end,
        lists:seq(128, 255)
    ).

eval_source_is_decoded_as_utf8(_Config) ->
    %% The request body is UTF-8. Scanned as bytes, each non-ASCII character of a
    %% string literal is double-encoded: `byte_size(~"Diário")` answers 9, not 7.
    Code = ~"byte_size(~\"Diário\").",
    ?assertMatch({reply, ~"7", _}, call(~"eval", #{~"code" => Code})).

eval_source_roundtrips_non_ascii(_Config) ->
    %% End to end: a non-ASCII literal in, the same literal out.
    ?assertMatch({reply, ~"<<\"Diário\"/utf8>>", _}, call(~"eval", #{~"code" => ~"~\"Diário\"."})).

%% --------------------------------------------------------------------
%% render_component
%% --------------------------------------------------------------------

render_component_stateful(_Config) ->
    Args = #{~"module" => ~"arizona_counter", ~"bindings" => #{~"id" => ~"c1", ~"count" => 7}},
    {reply, Html, _} = call(~"render_component", Args),
    ?assertMatch({_, _}, binary:match(Html, ~"Count")).

render_component_stateless(_Config) ->
    Bindings = #{~"title" => ~"Hi", ~"inner_content" => ~"<p>Body</p>"},
    Args = #{~"module" => ~"arizona_layout", ~"bindings" => Bindings},
    {reply, Html, _} = call(~"render_component", Args),
    ?assertMatch({_, _}, binary:match(Html, ~"Body")).

render_component_other(_Config) ->
    Args = #{~"module" => ~"arizona_pubsub", ~"bindings" => #{}},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"render_component", Args)).

render_component_bad_bindings(_Config) ->
    %% A layout without inner_content fails to render; the try/catch makes it in-band.
    Args = #{~"module" => ~"arizona_layout", ~"bindings" => #{~"title" => ~"Hi"}},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"render_component", Args)).

render_component_unknown_key(_Config) ->
    %% A binding key that isn't an existing atom is rejected in-band (no atom leak).
    Args = #{~"module" => ~"arizona_counter", ~"bindings" => #{~"no_such_binding_xyzzy" => 1}},
    ?assertMatch({error, Bin, _} when is_binary(Bin), call(~"render_component", Args)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

state() ->
    {ok, _Info, _Caps, State} = arizona_dev_mcp:init(#{}),
    State.

ctx() ->
    #{token => undefined, to => undefined}.

call(Tool, Args) ->
    call(Tool, Args, state()).

call(Tool, Args, State) ->
    arizona_dev_mcp:handle_tool(Tool, Args, ctx(), State).

%% The exact step the response path takes with a tool's reply, and the one that
%% used to raise -- taking the connection with it -- on a latin1 binary.
encode_json(Term) ->
    iolist_to_binary(json:encode(Term)).

%% --- stale-module fixtures (the arizona_reloader_SUITE consistency pattern) ---

tmp_dir() ->
    Dir = filename:join(
        "/tmp", "arizona_dev_mcp_ct_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    Dir.

%% Compile Src to a real .beam under Dir and load it from that path, so
%% code:which/1 returns a readable beam (what the staleness check reads).
compile_and_load(Name, Src, Dir) ->
    Erl = filename:join(Dir, atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(Erl, Src),
    {ok, Name, Bin} = compile:file(Erl, [binary, return_errors]),
    Beam = filename:join(Dir, atom_to_list(Name) ++ ".beam"),
    ok = file:write_file(Beam, Bin),
    _ = code:purge(Name),
    {module, Name} = code:load_binary(Name, Beam, Bin),
    {Name, Beam}.

compile_only(Name, Src, Dir) ->
    Erl = filename:join(Dir, atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(Erl, Src),
    {ok, Name, Bin} = compile:file(Erl, [binary, return_errors]),
    Bin.

versioned_src(Name, Value) ->
    io_lib:format(
        "-module(~p).\n"
        "-export([value/0]).\n"
        "value() -> ~b.\n",
        [Name, Value]
    ).
