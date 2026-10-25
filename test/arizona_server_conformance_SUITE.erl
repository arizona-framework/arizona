-module(arizona_server_conformance_SUITE).
-moduledoc """
Black-box server conformance: the HTTP/WS behaviour every Arizona server adapter
must reproduce.

Each adapter routes natively, so this suite is what keeps them in step. It boots
listeners through an `arizona_conformance_driver` -- `arizona_conformance_roadrunner`,
`arizona_conformance_cowboy`, and `arizona_conformance_cowboy_mounted` (Arizona
mounted on a Cowboy listener another framework owns) by default, one run each;
an adapter repo runs
it with its own driver via `ARIZONA_CONFORMANCE_DRIVER=<module>` -- and asserts
ONLY what crosses the wire: raw HTTP/1.1 and WebSocket over `gen_tcp`
(`arizona_wire_client`). No case may call into an adapter module or inspect its
internal state.
""".

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include("arizona_wire.hrl").
-include_lib("arizona/include/arizona_effect.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Routing
-export([live_get_is_200_html/1]).
-export([live_head_has_the_get_status_and_no_body/1]).
-export([live_path_answers_other_methods_with_405/1]).
-export([unknown_path_is_404/1]).
-export([same_path_verbs_reach_their_own_controllers/1]).
-export([allow_is_the_union_across_overlapping_patterns/1]).
-export([get_controller_answers_head/1]).
-export([get_controller_answers_other_methods_with_405/1]).
-export([match_any_answers_a_custom_method/1]).
-export([path_params_are_percent_decoded/1]).
-export([empty_segments_and_trailing_slash_are_ignored/1]).
-export([first_declared_route_wins/1]).
-export([wildcard_matches_zero_and_many_segments/1]).
-export([fully_shadowed_route_fails_to_start/1]).
-export([duplicate_start_keeps_the_running_listener/1]).
-export([capability_routes_are_served_or_refused/1]).
%% Assets
-export([asset_get_serves_the_file/1]).
-export([asset_head_has_no_body/1]).
-export([asset_answers_post_with_405/1]).
-export([asset_missing_file_is_404/1]).
-export([asset_traversal_is_not_served/1]).
-export([asset_symlink_escape_is_not_served/1]).
-export([asset_cache_control_rides_a_200/1]).
%% CSRF
-export([cross_origin_post_is_403/1]).
-export([same_origin_post_is_allowed/1]).
-export([post_without_origin_is_allowed/1]).
-export([null_origin_is_403/1]).
-export([check_origin_false_accepts_cross_origin/1]).
-export([csrf_origins_allowlist_accepts/1]).
-export([forwarded_https_demands_an_https_origin/1]).
-export([cross_origin_ws_upgrade_is_403/1]).
%% CORS
-export([cors_preflight_is_204_with_allow_origin/1]).
-export([cors_plain_options_is_204_with_allow/1]).
-export([cors_405_lists_options/1]).
%% Halts
-export([halt_redirect_carries_location_and_cookie/1]).
-export([bare_halt_is_403/1]).
-export([halt_with_status_uses_that_status/1]).
-export([ws_upgrade_halted_by_redirect_gets_the_redirect/1]).
%% Session / flash
-export([controller_session_write_sets_a_decodable_cookie/1]).
-export([controller_session_clear_sends_a_clearing_cookie/1]).
-export([flash_round_trips_through_a_redirect/1]).
-export([ws_upgrade_carries_middleware_cookie_and_session/1]).
-export([cookie_written_twice_sends_its_latest_value_last/1]).
-export([store_session_write_lands_in_the_store/1]).
-export([store_session_clear_deletes_the_entry/1]).
%% WebSocket
-export([ws_upgrade_to_live_path_opens_with_names/1]).
-export([ws_upgrade_to_unknown_path_is_404/1]).
-export([ws_navigate_to_param_route_replaces/1]).
-export([ws_navigate_to_non_live_path_is_a_full_navigate/1]).
-export([ws_patch_same_handler_diffs_in_place/1]).
-export([ws_patch_other_handler_replaces/1]).
-export([ws_navigate_into_redirecting_gate_replaces_history/1]).
%% Transforms
-export([etag_tags_and_revalidates/1]).
-export([default_compress_gzips_a_big_live_page/1]).
-export([etag_revalidates_through_compression/1]).
-export([security_headers_are_added/1]).
-export([listener_compress_off_leaves_pages_identity/1]).
-export([explicit_compress_beats_listener_default_off/1]).
%% Controller replies
-export([stream_reply_arrives_in_chunks/1]).
-export([sendfile_reply_sends_the_range/1]).
-export([loop_reply_pushes_until_it_stops/1]).
-export([loop_hears_the_client_go_away/1]).
-export([loop_keeps_a_linked_process_exit_meaning/1]).
%% Render
-export([mount_crash_is_500/1]).
-export([native_page_is_json/1]).
%% Drain
-export([drain_runs_handle_drain_then_closes_1001/1]).
-export([drain_lets_a_request_in_flight_finish/1]).
-export([notify_drain_winds_sessions_down_and_keeps_serving/1]).
-export([drains_answer_not_found_for_no_listener/1]).

-define(IN_TREE_DRIVERS, [
    arizona_conformance_roadrunner, arizona_conformance_cowboy, arizona_conformance_cowboy_mounted
]).
-define(SECRET, ~"conformance-secret-key-0123456789").
-define(EVIL, ~"https://evil.example").
-define(TRUSTED, ~"https://trusted.example").
-define(CACHE_CONTROL, ~"public, max-age=60").
%% The registered name a held loop reports what it receives to.
-define(LOOP_WATCHER, arizona_conformance_loop_watcher).

all() ->
    [{group, Driver} || Driver <- drivers()].

%% The suite runs once per driver: the in-tree adapters by default, or the one
%% `ARIZONA_CONFORMANCE_DRIVER` names (an adapter repo's own).
features() ->
    [
        {group, routing},
        {group, assets},
        {group, csrf},
        {group, cors},
        {group, halts},
        {group, session},
        {group, session_store},
        {group, websocket},
        {group, transforms},
        {group, compress_off},
        {group, controller_replies},
        {group, render},
        {group, drain}
    ].

groups() ->
    [{Driver, [], features()} || Driver <- drivers()] ++
        [
            {routing, [], [
                live_get_is_200_html,
                live_head_has_the_get_status_and_no_body,
                live_path_answers_other_methods_with_405,
                unknown_path_is_404,
                same_path_verbs_reach_their_own_controllers,
                allow_is_the_union_across_overlapping_patterns,
                get_controller_answers_head,
                get_controller_answers_other_methods_with_405,
                match_any_answers_a_custom_method,
                path_params_are_percent_decoded,
                empty_segments_and_trailing_slash_are_ignored,
                first_declared_route_wins,
                wildcard_matches_zero_and_many_segments,
                fully_shadowed_route_fails_to_start,
                duplicate_start_keeps_the_running_listener,
                capability_routes_are_served_or_refused
            ]},
            {assets, [], [
                asset_get_serves_the_file,
                asset_head_has_no_body,
                asset_answers_post_with_405,
                asset_missing_file_is_404,
                asset_traversal_is_not_served,
                asset_symlink_escape_is_not_served,
                asset_cache_control_rides_a_200
            ]},
            {csrf, [], [
                cross_origin_post_is_403,
                same_origin_post_is_allowed,
                post_without_origin_is_allowed,
                null_origin_is_403,
                check_origin_false_accepts_cross_origin,
                csrf_origins_allowlist_accepts,
                forwarded_https_demands_an_https_origin,
                cross_origin_ws_upgrade_is_403
            ]},
            {cors, [], [
                cors_preflight_is_204_with_allow_origin,
                cors_plain_options_is_204_with_allow,
                cors_405_lists_options
            ]},
            {halts, [], [
                halt_redirect_carries_location_and_cookie,
                bare_halt_is_403,
                halt_with_status_uses_that_status,
                ws_upgrade_halted_by_redirect_gets_the_redirect
            ]},
            {session, [], [
                controller_session_write_sets_a_decodable_cookie,
                controller_session_clear_sends_a_clearing_cookie,
                flash_round_trips_through_a_redirect,
                ws_upgrade_carries_middleware_cookie_and_session,
                cookie_written_twice_sends_its_latest_value_last
            ]},
            %% Store mode is read per request from the `session_store` app env, so it
            %% is switched on around this group only.
            {session_store, [], [
                store_session_write_lands_in_the_store,
                store_session_clear_deletes_the_entry
            ]},
            {websocket, [], [
                ws_upgrade_to_live_path_opens_with_names,
                ws_upgrade_to_unknown_path_is_404,
                ws_navigate_to_param_route_replaces,
                ws_navigate_to_non_live_path_is_a_full_navigate,
                ws_patch_same_handler_diffs_in_place,
                ws_patch_other_handler_replaces,
                ws_navigate_into_redirecting_gate_replaces_history
            ]},
            {transforms, [], [
                etag_tags_and_revalidates,
                default_compress_gzips_a_big_live_page,
                etag_revalidates_through_compression,
                security_headers_are_added
            ]},
            %% Its own listener: compression is a listener-wide default.
            {compress_off, [], [
                listener_compress_off_leaves_pages_identity,
                explicit_compress_beats_listener_default_off
            ]},
            %% Non-buffered reply kinds are an adapter capability: a case is
            %% skipped for a kind the driver says its adapter cannot send.
            {controller_replies, [], [
                stream_reply_arrives_in_chunks,
                sendfile_reply_sends_the_range,
                loop_reply_pushes_until_it_stops,
                loop_hears_the_client_go_away,
                loop_keeps_a_linked_process_exit_meaning
            ]},
            {render, [], [
                mount_crash_is_500,
                native_page_is_json
            ]},
            %% Its own listener: a drain stops it.
            {drain, [], [
                drain_runs_handle_drain_then_closes_1001,
                drain_lets_a_request_in_flight_finish,
                notify_drain_winds_sessions_down_and_keeps_serving,
                drains_answer_not_found_for_no_listener
            ]}
        ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    ok = application:set_env(arizona, secret_key, ?SECRET),
    Priv = ?config(priv_dir, Config),
    AssetDir = filename:join(Priv, "assets"),
    ok = filelib:ensure_path(AssetDir),
    ok = file:write_file(filename:join(AssetDir, "app.txt"), ~"asset-bytes"),
    %% A file beside (not under) the asset dir: what a traversal would reach.
    ok = file:write_file(filename:join(Priv, "secret.txt"), ~"top-secret-bytes"),
    %% A link inside the asset dir pointing at it.
    ok = file:make_symlink("../secret.txt", filename:join(AssetDir, "leak.txt")),
    [{asset_dir, AssetDir} | Config].

end_per_suite(Config) when is_list(Config) ->
    ok = application:unset_env(arizona, secret_key),
    %% Started here, so stopped here: a suite run after this one may boot the
    %% application itself, with its own `server` env.
    application:stop(arizona).

%% A driver group is named after its driver module and boots the main listener;
%% a feature group sets up what its cases need.
init_per_group(Group, Config) ->
    case lists:member(Group, drivers()) of
        true ->
            {ok, Port} = arizona_conformance_driver:start(
                Group, conformance_main, main_routes(?config(asset_dir, Config)), #{}
            ),
            [{driver, Group}, {port, Port} | Config];
        false ->
            init_feature(Group, Config)
    end.

end_per_group(Group, Config) ->
    case lists:member(Group, drivers()) of
        true -> arizona_conformance_driver:stop(Group, conformance_main);
        false -> end_feature(Group, Config)
    end.

init_feature(session_store, Config) ->
    ok = application:set_env(arizona, session_store, arizona_session_store_ets),
    {ok, Pid} = arizona_session_store_ets:start_link(),
    %% Unlinked so the store outlives this init process.
    true = unlink(Pid),
    [{store, Pid} | Config];
init_feature(compress_off, Config) ->
    Routes = [
        {live, ~"/big", arizona_about, #{bindings => big_page_bindings()}},
        {live, ~"/big_compressed", arizona_about, #{
            bindings => big_page_bindings(), middlewares => [arizona_middleware:compress()]
        }},
        {ws, ~"/ws", #{}}
    ],
    start_group_listener(conformance_compress_off, Routes, #{compress => false}, Config);
init_feature(_Group, Config) ->
    Config.

end_feature(session_store, Config) ->
    ok = gen_server:stop(?config(store, Config)),
    ok = application:unset_env(arizona, session_store),
    ok;
end_feature(compress_off, Config) ->
    arizona_conformance_driver:stop(?config(driver, Config), conformance_compress_off);
end_feature(_Group, _Config) ->
    ok.

start_group_listener(Name, Routes, Opts, Config) ->
    {ok, Port} = arizona_conformance_driver:start(?config(driver, Config), Name, Routes, Opts),
    [{port, Port} | proplists:delete(port, Config)].

drivers() ->
    case os:getenv("ARIZONA_CONFORMANCE_DRIVER") of
        Unset when Unset =:= false; Unset =:= "" -> ?IN_TREE_DRIVERS;
        Name -> [list_to_atom(Name)]
    end.

%% --------------------------------------------------------------------
%% Routes
%% --------------------------------------------------------------------

main_routes(AssetDir) ->
    Echo = fun(Marker) -> #{state => #{marker => Marker}} end,
    Extract = #{middlewares => [arizona_middleware:extract([path_bindings])]},
    [
        %% Routing
        {live, ~"/", arizona_crashable, #{}},
        {post, ~"/verbs", arizona_users_controller, #{action => create}},
        {delete, ~"/verbs", arizona_state_echo_controller, Echo(~"VERBS_DELETE")},
        {post, ~"/ov/:id", arizona_users_controller, #{action => create}},
        {delete, ~"/ov/:name", arizona_state_echo_controller, Echo(~"OV_DELETE")},
        {get, ~"/echo", arizona_state_echo_controller, Echo(~"ECHO_GET")},
        {match, '*', ~"/any", arizona_state_echo_controller, Echo(~"ANY_METHOD")},
        {live, ~"/items/:item_id", arizona_crashable, Extract},
        {get, ~"/first/me", arizona_state_echo_controller, Echo(~"FIRST_LITERAL")},
        {get, ~"/first/:id", arizona_state_echo_controller, Echo(~"FIRST_PARAM")},
        {get, ~"/wc/*rest", arizona_state_echo_controller, Echo(~"WILDCARD")},
        %% Assets
        {asset, ~"/files", {dir, AssetDir}, #{cache_control => ?CACHE_CONTROL}},
        %% CSRF
        {post, ~"/csrf", arizona_users_controller, #{action => create}},
        {post, ~"/csrf_open", arizona_users_controller, #{action => create, check_origin => false}},
        %% CORS
        {post, ~"/cors_api", arizona_transform_probe_controller, #{
            check_origin => false,
            middlewares => [
                arizona_middleware:cors(#{origins => [?TRUSTED], methods => [~"POST"]})
            ]
        }},
        %% Halts
        {live, ~"/halt_redirect", arizona_crashable, #{
            middlewares => [
                fun(Req, _B) ->
                    Req1 = arizona_req:put_resp_cookie(Req, ~"gate", ~"seen", #{path => ~"/"}),
                    {halt, arizona_req:redirect(Req1, 303, ~"/login")}
                end
            ]
        }},
        {live, ~"/bare_halt", arizona_crashable, #{
            middlewares => [fun(Req, _B) -> {halt, Req} end]
        }},
        {live, ~"/halt_status", arizona_crashable, #{
            middlewares => [fun(Req, _B) -> {halt, arizona_req:put_resp_status(Req, 401)} end]
        }},
        %% Session / flash
        {post, ~"/session_write", arizona_session_write_controller, #{}},
        {post, ~"/session_clear", arizona_logout_controller, #{}},
        {live, ~"/set_flash", arizona_crashable, #{
            middlewares => [
                fun(Req, _B) ->
                    Req1 = arizona_req:put_flash(Req, error, ~"flashed"),
                    {halt, arizona_req:redirect(Req1, 303, ~"/show_flash")}
                end
            ]
        }},
        {live, ~"/show_flash", arizona_crashable, #{
            middlewares => [
                {arizona_middleware, fetch_flash},
                fun(Req, B) ->
                    Flash = maps:get(flash, B, #{}),
                    {cont, Req, B#{status => maps:get(~"error", Flash, ~"none")}}
                end
            ]
        }},
        {live, ~"/upgrade_writes", arizona_crashable, #{
            middlewares => [
                fun(Req, B) ->
                    Req1 = arizona_req:put_resp_cookie(Req, ~"probe", ~"ws", #{path => ~"/"}),
                    {cont, arizona_req:put_session(Req1, ~"seen", ~"ws"), B}
                end
            ]
        }},
        {live, ~"/cookie_twice", arizona_crashable, #{
            middlewares => [
                fun(Req, B) ->
                    Req1 = arizona_req:put_resp_cookie(Req, ~"theme", ~"dark", #{path => ~"/"}),
                    {cont, arizona_req:put_resp_cookie(Req1, ~"theme", ~"light", #{path => ~"/"}),
                        B}
                end
            ]
        }},
        {live, ~"/upgrade_session", arizona_crashable, #{
            middlewares => [
                fun(Req, B) -> {cont, arizona_req:put_session(Req, ~"seen", ~"ws"), B} end
            ]
        }},
        %% WebSocket
        {live, ~"/pv/home", arizona_patch_view, #{bindings => #{section => ~"home"}}},
        {live, ~"/pv/about", arizona_patch_view, #{bindings => #{section => ~"about"}}},
        {live, ~"/pv/other", arizona_root_counter, #{}},
        %% Transforms
        {get, ~"/etag_api", arizona_transform_probe_controller, #{
            middlewares => [arizona_middleware:etag()]
        }},
        {live, ~"/big", arizona_about, #{bindings => big_page_bindings()}},
        {live, ~"/etag_page", arizona_about, #{
            bindings => big_page_bindings(), middlewares => [arizona_middleware:etag()]
        }},
        {live, ~"/secure", arizona_crashable, #{
            middlewares => [arizona_middleware:security_headers()]
        }},
        %% Controller replies
        {get, ~"/reply/stream", arizona_stream_controller, #{
            middlewares => [
                fun(Req, B) ->
                    {cont, arizona_req:put_resp_cookie(Req, ~"probe", ~"stream", #{}), B}
                end
            ]
        }},
        {get, ~"/reply/sendfile", arizona_sendfile_controller, #{
            state => #{path => filename:join(AssetDir, "app.txt")}
        }},
        {get, ~"/reply/loop", arizona_loop_controller, #{}},
        {get, ~"/reply/loop_hold", arizona_loop_controller, #{
            action => hold, state => #{notify => ?LOOP_WATCHER}
        }},
        {get, ~"/reply/loop_linked", arizona_loop_controller, #{
            action => linked, state => #{notify => ?LOOP_WATCHER}
        }},
        %% Render
        {live, ~"/crash_on_mount", arizona_crashable, #{bindings => #{crash_on_mount => true}}},
        {live, ~"/native_page", arizona_native_counter_demo, #{}},
        {ws, ~"/ws", #{}}
    ].

%% Big enough to clear a compressor's minimum-size threshold (roadrunner's is
%% 860 bytes), so compression is observable on the wire.
big_page_bindings() ->
    #{tags => [<<"tag-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 100)]}.

%% --------------------------------------------------------------------
%% Routing
%% --------------------------------------------------------------------

live_get_is_200_html(Config) ->
    {Status, Headers, Body} = req(Config, ~"GET", ~"/", []),
    ?assertEqual(200, Status),
    ?assertEqual(~"text/html; charset=utf-8", header(~"content-type", Headers)),
    ?assertEqual(~"ok", rendered(~"Status", Body)).

live_head_has_the_get_status_and_no_body(Config) ->
    ?assertMatch(
        {200, Headers, <<>>} when is_list(Headers),
        req(Config, ~"HEAD", ~"/", [])
    ).

live_path_answers_other_methods_with_405(Config) ->
    [
        ?assertMatch(
            {405, ~"GET, HEAD"},
            begin
                {Status, Headers, _} = req(Config, Method, ~"/", []),
                {Status, header(~"allow", Headers)}
            end
        )
     || Method <- [~"POST", ~"PUT", ~"DELETE"]
    ],
    ok.

unknown_path_is_404(Config) ->
    ?assertMatch({404, _, _}, req(Config, ~"GET", ~"/no/such/route", [])).

same_path_verbs_reach_their_own_controllers(Config) ->
    ?assertMatch({200, _, ~"users#create"}, req(Config, ~"POST", ~"/verbs", [])),
    ?assertMatch({200, _, ~"VERBS_DELETE"}, req(Config, ~"DELETE", ~"/verbs", [])),
    {405, Headers, _} = req(Config, ~"GET", ~"/verbs", []),
    ?assertEqual(~"DELETE, POST", header(~"allow", Headers)).

%% `/ov/:id` (POST) and `/ov/:name` (DELETE) are different patterns matching
%% the same URL: each verb reaches its route, and the 405 lists both.
allow_is_the_union_across_overlapping_patterns(Config) ->
    ?assertMatch({200, _, ~"users#create"}, req(Config, ~"POST", ~"/ov/42", [])),
    ?assertMatch({200, _, ~"OV_DELETE"}, req(Config, ~"DELETE", ~"/ov/42", [])),
    {405, Headers, _} = req(Config, ~"GET", ~"/ov/42", []),
    ?assertEqual(~"DELETE, POST", header(~"allow", Headers)).

%% A `get` controller's allowlist is GET and HEAD: anything else is refused
%% before the action runs.
get_controller_answers_other_methods_with_405(Config) ->
    {405, Headers, _} = req(Config, ~"POST", ~"/echo", []),
    ?assertEqual(~"GET, HEAD", header(~"allow", Headers)).

get_controller_answers_head(Config) ->
    {200, GetHeaders, ~"ECHO_GET"} = req(Config, ~"GET", ~"/echo", []),
    {200, HeadHeaders, <<>>} = req(Config, ~"HEAD", ~"/echo", []),
    ?assertEqual(header(~"content-type", GetHeaders), header(~"content-type", HeadHeaders)).

match_any_answers_a_custom_method(Config) ->
    ?assertMatch({200, _, ~"ANY_METHOD"}, req(Config, ~"PROPFIND", ~"/any", [])),
    ?assertMatch({200, _, ~"ANY_METHOD"}, req(Config, ~"GET", ~"/any", [])).

path_params_are_percent_decoded(Config) ->
    {200, _, Body} = req(Config, ~"GET", ~"/items/caf%C3%A9", []),
    ?assertEqual(~"café", rendered(~"Item", Body)).

empty_segments_and_trailing_slash_are_ignored(Config) ->
    {200, _, Body} = req(Config, ~"GET", ~"//items//x/", []),
    ?assertEqual(~"x", rendered(~"Item", Body)).

%% `/first/me` is declared before `/first/:id`; both match `/first/me`.
first_declared_route_wins(Config) ->
    ?assertMatch({200, _, ~"FIRST_LITERAL"}, req(Config, ~"GET", ~"/first/me", [])),
    ?assertMatch({200, _, ~"FIRST_PARAM"}, req(Config, ~"GET", ~"/first/other", [])).

wildcard_matches_zero_and_many_segments(Config) ->
    [
        ?assertMatch({200, _, ~"WILDCARD"}, req(Config, ~"GET", Path, []))
     || Path <- [~"/wc", ~"/wc/a", ~"/wc/a/b/c"]
    ],
    ?assertMatch({404, _, _}, req(Config, ~"GET", ~"/wcx", [])).

%% Starting a listener under a running one's name fails, and the running one
%% keeps serving its own routes.
duplicate_start_keeps_the_running_listener(Config) ->
    Driver = ?config(driver, Config),
    Result =
        try arizona_conformance_driver:start(Driver, conformance_main, [], #{}) of
            Started -> Started
        catch
            Class:Reason -> {raised, Class, Reason}
        end,
    ?assertNotMatch({ok, _}, Result),
    ?assertMatch({200, _, _}, req(Config, ~"GET", ~"/", [])).

%% A route no request can reach is a configuration error, refused at start.
fully_shadowed_route_fails_to_start(Config) ->
    Driver = ?config(driver, Config),
    Routes = [
        {get, ~"/sh/:id", arizona_state_echo_controller, #{state => #{marker => ~"A"}}},
        {get, ~"/sh/x", arizona_state_echo_controller, #{state => #{marker => ~"B"}}}
    ],
    Result =
        try arizona_conformance_driver:start(Driver, conformance_shadowed, Routes, #{}) of
            Started -> Started
        catch
            Class:Reason -> {raised, Class, Reason}
        end,
    case Result of
        {ok, _Port} ->
            ok = arizona_conformance_driver:stop(Driver, conformance_shadowed),
            ct:fail({shadowed_route_accepted, Routes});
        {error, _} ->
            ok;
        {raised, _, _} ->
            ok
    end.

%% An `mcp` or `reload` route is served by an adapter that has the kind and
%% refused at start, naming it, by one that does not -- never accepted and then
%% answered with something else.
capability_routes_are_served_or_refused(Config) ->
    Driver = ?config(driver, Config),
    Cases = [
        {reload, [{reload, ~"/cap/reload", #{}}], ~"/cap/reload"},
        {mcp, [arizona_dev_mcp:route(~"/cap/mcp")], {mcp, ~"/cap/mcp"}}
    ],
    [capability_route(Driver, Kind, Routes, Probe) || {Kind, Routes, Probe} <- Cases],
    ok.

capability_route(Driver, Kind, Routes, Probe) ->
    Name = conformance_capability,
    case arizona_conformance_driver:supports_route(Driver, Kind) of
        true ->
            {ok, Port} = arizona_conformance_driver:start(Driver, Name, Routes, #{}),
            try
                ok = probe_capability(Port, Probe)
            after
                ok = arizona_conformance_driver:stop(Driver, Name)
            end;
        false ->
            ?assertError(
                {unsupported_route, Kind, _},
                arizona_conformance_driver:start(Driver, Name, Routes, #{})
            )
    end.

%% The MCP route answers an `initialize` POST with a session, and that session's
%% GET opens its server-to-client event stream (a controller `loop` reply).
probe_capability(Port, {mcp, Path}) ->
    Init = json:encode(#{
        ~"jsonrpc" => ~"2.0",
        ~"id" => 1,
        ~"method" => ~"initialize",
        ~"params" => #{
            ~"protocolVersion" => ~"2025-06-18",
            ~"capabilities" => #{},
            ~"clientInfo" => #{~"name" => ~"conformance", ~"version" => ~"1"}
        }
    }),
    {200, Headers, Body} = arizona_wire_client:request(
        Port,
        ~"POST",
        Path,
        [
            {~"content-type", ~"application/json"},
            {~"accept", ~"application/json, text/event-stream"}
        ],
        Init
    ),
    SessionId = arizona_wire_client:header(~"mcp-session-id", Headers),
    ?assert(is_binary(SessionId)),
    ?assertMatch(#{~"id" := 1, ~"result" := #{~"protocolVersion" := _}}, json:decode(Body)),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    try
        ok = gen_tcp:send(Sock, [
            ~"GET ",
            Path,
            ~" HTTP/1.1\r\nhost: localhost\r\naccept: text/event-stream\r\nmcp-session-id: ",
            SessionId,
            ~"\r\n\r\n"
        ]),
        Head = recv_until(Sock, ~"\r\n\r\n", <<>>),
        ?assertMatch(<<"HTTP/1.1 200", _/binary>>, Head),
        ?assertNotEqual(nomatch, binary:match(string:lowercase(Head), ~"text/event-stream"))
    after
        gen_tcp:close(Sock)
    end;
%% The reload route is an event stream that stays open: a reloader broadcast
%% must arrive on it as a `reload` event.
probe_capability(Port, Path) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
    try
        ok = gen_tcp:send(Sock, [
            ~"GET ", Path, ~" HTTP/1.1\r\nhost: localhost\r\n\r\n"
        ]),
        Head = recv_until(Sock, ~"\r\n\r\n", <<>>),
        ?assertMatch(<<"HTTP/1.1 200", _/binary>>, Head),
        ?assertNotEqual(nomatch, binary:match(string:lowercase(Head), ~"text/event-stream")),
        %% The stream joins the reloader topic as it opens; a broadcast before
        %% that would be missed, so keep broadcasting until one lands.
        ?assertNotEqual(nomatch, binary:match(await_reload_event(Sock, 50), ~"event: reload"))
    after
        gen_tcp:close(Sock)
    end.

await_reload_event(_Sock, 0) ->
    ct:fail(no_reload_event);
await_reload_event(Sock, Tries) ->
    ok = arizona_reloader:broadcast(),
    case gen_tcp:recv(Sock, 0, 100) of
        {ok, Data} -> Data;
        {error, timeout} -> await_reload_event(Sock, Tries - 1)
    end.

recv_until(Sock, Marker, Acc) ->
    case binary:match(Acc, Marker) of
        nomatch ->
            {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
            recv_until(Sock, Marker, <<Acc/binary, Data/binary>>);
        _Found ->
            Acc
    end.

%% --------------------------------------------------------------------
%% Assets
%% --------------------------------------------------------------------

asset_get_serves_the_file(Config) ->
    ?assertMatch({200, _, ~"asset-bytes"}, req(Config, ~"GET", ~"/files/app.txt", [])).

asset_head_has_no_body(Config) ->
    {200, Headers, <<>>} = req(Config, ~"HEAD", ~"/files/app.txt", []),
    ?assertEqual(~"11", header(~"content-length", Headers)).

asset_answers_post_with_405(Config) ->
    {405, Headers, _} = req(Config, ~"POST", ~"/files/app.txt", []),
    ?assertEqual(~"GET, HEAD", header(~"allow", Headers)).

asset_missing_file_is_404(Config) ->
    ?assertMatch({404, _, _}, req(Config, ~"GET", ~"/files/missing.txt", [])).

%% `secret.txt` sits one level above the asset dir; no spelling of `..` may
%% reach it.
asset_traversal_is_not_served(Config) ->
    [
        ?assertMatch(
            {Status, _, Body} when
                Status >= 400 andalso Status < 500 andalso Body =/= ~"top-secret-bytes",
            req(Config, ~"GET", Path, [])
        )
     || Path <- [
            ~"/files/../secret.txt",
            ~"/files/%2e%2e/secret.txt",
            ~"/files/%2E%2E%2Fsecret.txt",
            ~"/files/..%2fsecret.txt"
        ]
    ],
    ok.

%% `leak.txt` is a symlink inside the asset dir to the file above it.
asset_symlink_escape_is_not_served(Config) ->
    ?assertMatch(
        {Status, _, Body} when
            Status >= 400 andalso Status < 500 andalso Body =/= ~"top-secret-bytes",
        req(Config, ~"GET", ~"/files/leak.txt", [])
    ).

asset_cache_control_rides_a_200(Config) ->
    {200, Headers, _} = req(Config, ~"GET", ~"/files/app.txt", []),
    ?assertEqual(?CACHE_CONTROL, header(~"cache-control", Headers)).

%% --------------------------------------------------------------------
%% CSRF
%% --------------------------------------------------------------------

cross_origin_post_is_403(Config) ->
    ?assertMatch({403, _, _}, req(Config, ~"POST", ~"/csrf", [{~"origin", ?EVIL}])).

same_origin_post_is_allowed(Config) ->
    ?assertMatch(
        {200, _, ~"users#create"},
        req(Config, ~"POST", ~"/csrf", [{~"origin", same_origin(~"http", Config)}])
    ).

post_without_origin_is_allowed(Config) ->
    ?assertMatch({200, _, ~"users#create"}, req(Config, ~"POST", ~"/csrf", [])).

null_origin_is_403(Config) ->
    ?assertMatch({403, _, _}, req(Config, ~"POST", ~"/csrf", [{~"origin", ~"null"}])).

check_origin_false_accepts_cross_origin(Config) ->
    ?assertMatch(
        {200, _, ~"users#create"}, req(Config, ~"POST", ~"/csrf_open", [{~"origin", ?EVIL}])
    ).

csrf_origins_allowlist_accepts(Config) ->
    ?assertMatch({403, _, _}, req(Config, ~"POST", ~"/csrf", [{~"origin", ?TRUSTED}])),
    ok = application:set_env(arizona, csrf_origins, [?TRUSTED]),
    try
        ?assertMatch(
            {200, _, ~"users#create"}, req(Config, ~"POST", ~"/csrf", [{~"origin", ?TRUSTED}])
        ),
        ?assertMatch({403, _, _}, req(Config, ~"POST", ~"/csrf", [{~"origin", ?EVIL}]))
    after
        ok = application:unset_env(arizona, csrf_origins)
    end.

%% Behind a TLS-terminating proxy the page is https, so a plain-http page on
%% the same authority is a different origin.
forwarded_https_demands_an_https_origin(Config) ->
    Https = [{~"x-forwarded-proto", ~"https"}],
    ?assertMatch(
        {403, _, _},
        req(Config, ~"POST", ~"/csrf", [{~"origin", same_origin(~"http", Config)} | Https])
    ),
    ?assertMatch(
        {200, _, ~"users#create"},
        req(Config, ~"POST", ~"/csrf", [{~"origin", same_origin(~"https", Config)} | Https])
    ).

cross_origin_ws_upgrade_is_403(Config) ->
    ?assertMatch({403, _}, upgrade_head(Config, ~"/", [{~"origin", ?EVIL}])),
    ?assertMatch(
        {101, _}, upgrade_head(Config, ~"/", [{~"origin", same_origin(~"http", Config)}])
    ).

%% --------------------------------------------------------------------
%% CORS
%% --------------------------------------------------------------------

cors_preflight_is_204_with_allow_origin(Config) ->
    {204, Headers, <<>>} = req(Config, ~"OPTIONS", ~"/cors_api", [
        {~"origin", ?TRUSTED}, {~"access-control-request-method", ~"POST"}
    ]),
    ?assertEqual(?TRUSTED, header(~"access-control-allow-origin", Headers)),
    %% An origin the transform does not serve gets no grant.
    {204, Denied, <<>>} = req(Config, ~"OPTIONS", ~"/cors_api", [
        {~"origin", ?EVIL}, {~"access-control-request-method", ~"POST"}
    ]),
    ?assertEqual(undefined, header(~"access-control-allow-origin", Denied)).

cors_plain_options_is_204_with_allow(Config) ->
    {204, Headers, <<>>} = req(Config, ~"OPTIONS", ~"/cors_api", []),
    ?assertEqual(~"OPTIONS, POST", header(~"allow", Headers)).

cors_405_lists_options(Config) ->
    {405, Headers, _} = req(Config, ~"PUT", ~"/cors_api", []),
    ?assertEqual(~"OPTIONS, POST", header(~"allow", Headers)).

%% --------------------------------------------------------------------
%% Halts
%% --------------------------------------------------------------------

halt_redirect_carries_location_and_cookie(Config) ->
    {303, Headers, _} = req(Config, ~"GET", ~"/halt_redirect", []),
    ?assertEqual(~"/login", header(~"location", Headers)),
    ?assertMatch([<<"gate=seen", _/binary>>], cookies(~"gate", Headers)).

bare_halt_is_403(Config) ->
    ?assertMatch({403, _, _}, req(Config, ~"GET", ~"/bare_halt", [])).

halt_with_status_uses_that_status(Config) ->
    ?assertMatch({401, _, _}, req(Config, ~"GET", ~"/halt_status", [])).

ws_upgrade_halted_by_redirect_gets_the_redirect(Config) ->
    {303, Headers} = upgrade_head(Config, ~"/halt_redirect", []),
    ?assertEqual(~"/login", header(~"location", Headers)),
    ?assertMatch([<<"gate=seen", _/binary>>], cookies(~"gate", Headers)).

%% --------------------------------------------------------------------
%% Session / flash
%% --------------------------------------------------------------------

controller_session_write_sets_a_decodable_cookie(Config) ->
    {200, Headers, _} = req(Config, ~"POST", ~"/session_write", []),
    ?assertEqual(#{~"user_id" => ~"u42"}, arizona_session:decode(session_value(Headers))).

controller_session_clear_sends_a_clearing_cookie(Config) ->
    Session = arizona_session:encode(#{~"user_id" => ~"u1"}),
    {200, Headers, _} = req(Config, ~"POST", ~"/session_clear", [
        {~"cookie", <<"az_session=", Session/binary>>}
    ]),
    ?assertMatch([<<"az_session=;", _/binary>>], cookies(~"az_session", Headers)).

flash_round_trips_through_a_redirect(Config) ->
    {303, Headers, _} = req(Config, ~"GET", ~"/set_flash", []),
    ?assertEqual(~"/show_flash", header(~"location", Headers)),
    [SetCookie] = cookies(~"az_flash", Headers),
    [Pair | _] = binary:split(SetCookie, ~";"),
    ?assertNotEqual(~"az_flash=", Pair),
    {200, _, Body} = req(Config, ~"GET", ~"/show_flash", [{~"cookie", Pair}]),
    ?assertEqual(~"flashed", rendered(~"Status", Body)),
    {200, _, Plain} = req(Config, ~"GET", ~"/show_flash", []),
    ?assertEqual(~"none", rendered(~"Status", Plain)).

ws_upgrade_carries_middleware_cookie_and_session(Config) ->
    {101, Headers} = upgrade_head(Config, ~"/upgrade_writes", []),
    ?assertMatch([<<"probe=ws", _/binary>>], cookies(~"probe", Headers)),
    ?assertEqual(#{~"seen" => ~"ws"}, arizona_session:decode(session_value(Headers))).

%% A browser keeps the last `Set-Cookie` for a name, so a cookie written twice
%% must go out in the order it was written.
cookie_written_twice_sends_its_latest_value_last(Config) ->
    {200, Headers, _} = req(Config, ~"GET", ~"/cookie_twice", []),
    ?assertMatch(
        [<<"theme=dark", _/binary>>, <<"theme=light", _/binary>>], cookies(~"theme", Headers)
    ).

store_session_write_lands_in_the_store(Config) ->
    Id = seed_stored_session(),
    {200, Headers, _} = req(Config, ~"POST", ~"/session_write", [stored_cookie(Id)]),
    ?assertEqual({ok, Id}, arizona_session:decode_id(session_value(Headers))),
    ?assertMatch({ok, #{~"user_id" := ~"u42"}}, arizona_session_store_ets:get(Id)),
    %% The same through a WS upgrade: committed by the 101.
    Id2 = seed_stored_session(),
    {101, _} = upgrade_head(Config, ~"/upgrade_session", [stored_cookie(Id2)]),
    ?assertMatch(
        {ok, #{~"user_id" := ~"u1", ~"seen" := ~"ws"}}, arizona_session_store_ets:get(Id2)
    ).

store_session_clear_deletes_the_entry(Config) ->
    Id = seed_stored_session(),
    {200, Headers, _} = req(Config, ~"POST", ~"/session_clear", [stored_cookie(Id)]),
    ?assertMatch([<<"az_session=;", _/binary>>], cookies(~"az_session", Headers)),
    ?assertEqual(no_session, arizona_session_store_ets:get(Id)).

seed_stored_session() ->
    Id = arizona_session:new_id(),
    ok = arizona_session_store_ets:put(Id, #{~"user_id" => ~"u1"}, arizona_session:max_age()),
    Id.

stored_cookie(Id) ->
    {Name, Value, _Opts} = arizona_session:set_cookie_id(Id),
    {~"cookie", <<Name/binary, "=", Value/binary>>}.

%% --------------------------------------------------------------------
%% WebSocket
%% --------------------------------------------------------------------

ws_upgrade_to_live_path_opens_with_names(Config) ->
    {ok, Sock} = ws_connect(Config, ~"/"),
    {text, Frame} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(#{~"a" := Names} when is_list(Names), json:decode(Frame)),
    ok = arizona_wire_client:ws_close(Sock).

ws_upgrade_to_unknown_path_is_404(Config) ->
    ?assertMatch({404, _}, upgrade_head(Config, ~"/no/such/route", [])),
    %% A real path that is not a live route is no target either.
    ?assertMatch({404, _}, upgrade_head(Config, ~"/echo", [])).

ws_navigate_to_param_route_replaces(Config) ->
    Sock = live_socket(Config, ~"/"),
    ok = navigate(Sock, ~"navigate", ~"/items/navtest"),
    {text, Resp} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(#{~"o" := [[?OP_REPLACE, _, _]]}, json:decode(Resp)),
    ?assertNotEqual(nomatch, binary:match(Resp, ~"navtest")),
    ok = arizona_wire_client:ws_close(Sock).

ws_navigate_to_non_live_path_is_a_full_navigate(Config) ->
    Sock = live_socket(Config, ~"/"),
    ok = navigate(Sock, ~"navigate", ~"/echo"),
    {text, Resp} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(
        #{~"e" := [[?EFFECT_NAVIGATE, ~"/echo", #{~"full" := true}]]}, json:decode(Resp)
    ),
    ok = arizona_wire_client:ws_close(Sock).

ws_patch_same_handler_diffs_in_place(Config) ->
    Sock = live_socket(Config, ~"/pv/home"),
    ok = navigate(Sock, ~"patch", ~"/pv/about"),
    {text, Resp} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(#{~"o" := [[?OP_TEXT, _, ~"about"]]}, json:decode(Resp)),
    ok = arizona_wire_client:ws_close(Sock).

ws_patch_other_handler_replaces(Config) ->
    Sock = live_socket(Config, ~"/pv/home"),
    ok = navigate(Sock, ~"patch", ~"/pv/other"),
    {text, Resp} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(#{~"o" := [[?OP_REPLACE, _, _]]}, json:decode(Resp)),
    ok = arizona_wire_client:ws_close(Sock).

%% The client already pushed the gated URL; a redirect must replace that
%% history entry, not stack on it.
ws_navigate_into_redirecting_gate_replaces_history(Config) ->
    Sock = live_socket(Config, ~"/"),
    ok = navigate(Sock, ~"navigate", ~"/halt_redirect"),
    {text, Resp} = arizona_wire_client:ws_recv(Sock),
    Decoded = json:decode(Resp),
    ?assertMatch(#{~"e" := [[?EFFECT_NAVIGATE, ~"/login", #{~"replace" := true}]]}, Decoded),
    ?assertNot(is_map_key(~"o", Decoded)),
    ok = arizona_wire_client:ws_close(Sock).

%% --------------------------------------------------------------------
%% Transforms
%% --------------------------------------------------------------------

etag_tags_and_revalidates(Config) ->
    {200, Headers, ~"stable-transform-probe-body"} = req(Config, ~"GET", ~"/etag_api", []),
    Tag = header(~"etag", Headers),
    ?assertMatch(<<"W/\"", _/binary>>, Tag),
    ?assertMatch({304, _, <<>>}, req(Config, ~"GET", ~"/etag_api", [{~"if-none-match", Tag}])),
    ?assertMatch(
        {200, _, ~"stable-transform-probe-body"},
        req(Config, ~"GET", ~"/etag_api", [{~"if-none-match", ~"W/\"stale\""}])
    ).

default_compress_gzips_a_big_live_page(Config) ->
    {200, PlainHeaders, Plain} = req(Config, ~"GET", ~"/big", []),
    ?assertEqual(undefined, header(~"content-encoding", PlainHeaders)),
    {200, GzipHeaders, Gzip} = req(Config, ~"GET", ~"/big", [{~"accept-encoding", ~"gzip"}]),
    ?assertEqual(~"gzip", header(~"content-encoding", GzipHeaders)),
    ?assertEqual(Plain, zlib:gunzip(Gzip)).

%% The validator names the representation's content, so a gzip-accepting
%% client revalidates with the tag it was handed.
etag_revalidates_through_compression(Config) ->
    Gzip = {~"accept-encoding", ~"gzip"},
    {200, Headers, Body} = req(Config, ~"GET", ~"/etag_page", [Gzip]),
    ?assertEqual(~"gzip", header(~"content-encoding", Headers)),
    ?assertNotEqual(nomatch, binary:match(zlib:gunzip(Body), ~"tag-100")),
    Tag = header(~"etag", Headers),
    ?assertMatch(<<_, _/binary>>, Tag),
    ?assertMatch(
        {304, _, <<>>}, req(Config, ~"GET", ~"/etag_page", [Gzip, {~"if-none-match", Tag}])
    ).

security_headers_are_added(Config) ->
    {200, Headers, _} = req(Config, ~"GET", ~"/secure", []),
    ?assertEqual(~"nosniff", header(~"x-content-type-options", Headers)),
    ?assertEqual(~"SAMEORIGIN", header(~"x-frame-options", Headers)),
    ?assertEqual(~"strict-origin-when-cross-origin", header(~"referrer-policy", Headers)),
    {200, Plain, _} = req(Config, ~"GET", ~"/", []),
    ?assertEqual(undefined, header(~"x-content-type-options", Plain)).

listener_compress_off_leaves_pages_identity(Config) ->
    {200, Headers, Body} = req(Config, ~"GET", ~"/big", [{~"accept-encoding", ~"gzip"}]),
    ?assertEqual(undefined, header(~"content-encoding", Headers)),
    ?assertNotEqual(nomatch, binary:match(Body, ~"tag-100")).

explicit_compress_beats_listener_default_off(Config) ->
    {200, Headers, Body} = req(Config, ~"GET", ~"/big_compressed", [{~"accept-encoding", ~"gzip"}]),
    ?assertEqual(~"gzip", header(~"content-encoding", Headers)),
    ?assertNotEqual(nomatch, binary:match(zlib:gunzip(Body), ~"tag-100")).

%% --------------------------------------------------------------------
%% Controller replies
%% --------------------------------------------------------------------

%% A middleware's cookie reaches a non-buffered reply too.
stream_reply_arrives_in_chunks(Config) ->
    when_supported(stream, Config, fun() ->
        {200, Headers, Body} = req(Config, ~"GET", ~"/reply/stream", []),
        ?assertEqual(~"STREAM_OK", Body),
        ?assertMatch([<<"probe=stream", _/binary>>], cookies(~"probe", Headers))
    end).

sendfile_reply_sends_the_range(Config) ->
    when_supported(sendfile, Config, fun() ->
        ?assertMatch({200, _, ~"sset-bytes"}, req(Config, ~"GET", ~"/reply/sendfile", []))
    end).

loop_reply_pushes_until_it_stops(Config) ->
    when_supported(loop, Config, fun() ->
        {200, Headers, Body} = req(Config, ~"GET", ~"/reply/loop", []),
        ?assertEqual(~"text/event-stream", header(~"content-type", Headers)),
        ?assertEqual(~"data: 1\n\ndata: 2\n\n", Body)
    end).

%% Runs `Case` when the adapter can send this reply kind, else skips it.
when_supported(Kind, Config, Case) ->
    case arizona_conformance_driver:supports_reply(?config(driver, Config), Kind) of
        true -> Case();
        false -> {skip, {unsupported_reply, Kind}}
    end.

%% The client closing a held loop reaches the controller as a final
%% `{arizona_disconnect, Reason}`, so it can clean up (unsubscribe, ...).
loop_hears_the_client_go_away(Config) ->
    when_supported(loop, Config, fun() ->
        true = register(?LOOP_WATCHER, self()),
        try
            Head = arizona_wire_client:request_head(
                ?config(port, Config), ~"GET", ~"/reply/loop_hold", []
            ),
            ?assertMatch({200, _}, Head),
            receive
                {arizona_loop_controller, {arizona_disconnect, _Reason}} -> ok
            after 3000 -> ct:fail(no_disconnect)
            end
        after
            true = unregister(?LOOP_WATCHER)
        end
    end).

%% A loop's links mean what they mean to any process, however the adapter hears
%% its client go: a linked process exiting `normal` is no event (the controller
%% is not handed it), and one exiting with any other reason takes the loop down
%% with that reason (a server catching its request process's crash may add a
%% stacktrace).
loop_keeps_a_linked_process_exit_meaning(Config) ->
    when_supported(loop, Config, fun() ->
        true = register(?LOOP_WATCHER, self()),
        try
            {ok, Sock} = gen_tcp:connect("localhost", ?config(port, Config), [
                binary, {active, false}
            ]),
            ok = gen_tcp:send(Sock, ~"GET /reply/loop_linked HTTP/1.1\r\nhost: localhost\r\n\r\n"),
            [Normal, Crash, Loop] =
                receive
                    {arizona_loop_controller, {linked, LoopPid, [A, B]}} -> [A, B, LoopPid]
                after 3000 -> ct:fail(no_loop)
                end,
            Mon = erlang:monitor(process, Loop),
            Normal ! {exit, normal},
            receive
                {'DOWN', Mon, process, Loop, Early} -> ct:fail({loop_died, Early});
                {arizona_loop_controller, {'EXIT', _, _} = Exit} -> ct:fail({handed, Exit})
            after 300 -> ok
            end,
            Crash ! {exit, linked_crash},
            receive
                {'DOWN', Mon, process, Loop, Reason} -> ok = linked_crash(Reason)
            after 3000 -> ct:fail(loop_alive)
            end,
            gen_tcp:close(Sock)
        after
            true = unregister(?LOOP_WATCHER)
        end
    end).

linked_crash(linked_crash) -> ok;
linked_crash({linked_crash, Stacktrace}) when is_list(Stacktrace) -> ok.

%% --------------------------------------------------------------------
%% Render
%% --------------------------------------------------------------------

mount_crash_is_500(Config) ->
    ?assertMatch({500, _, _}, req(Config, ~"GET", ~"/crash_on_mount", [])).

native_page_is_json(Config) ->
    {200, Headers, Body} = req(Config, ~"GET", ~"/native_page", []),
    ?assertEqual(~"application/json", header(~"content-type", Headers)),
    ?assertMatch(#{~"type" := _}, json:decode(Body)).

%% --------------------------------------------------------------------
%% Drain
%% --------------------------------------------------------------------

%% `arizona_drainable` (drain_mode stop) answers the drain with an effect and
%% stops: the effect frame arrives first, then a going-away close.
drain_runs_handle_drain_then_closes_1001(Config0) ->
    Config = drain_listener(Config0),
    Sock = live_socket(Config, ~"/drain_stop"),
    Self = self(),
    Driver = ?config(driver, Config),
    Drainer = spawn_link(fun() ->
        Self ! {drained, arizona_conformance_driver:drain(Driver, conformance_drain, 5000)}
    end),
    {text, Effect} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(#{~"e" := [_ | _]}, json:decode(Effect)),
    ?assertMatch({close, 1001, _}, arizona_wire_client:ws_recv(Sock)),
    receive
        {drained, Result} -> ?assertEqual(ok, Result)
    after 10000 ->
        ct:fail({drain_did_not_return, Drainer})
    end,
    ok = arizona_wire_client:ws_close(Sock).

%% A drain waits for the requests in flight: one that started before it
%% finishes with its response instead of being cut off.
drain_lets_a_request_in_flight_finish(Config0) ->
    Config = drain_listener(Config0),
    Port = ?config(port, Config),
    Self = self(),
    _Requester = spawn_link(fun() ->
        Self ! {slow, arizona_wire_client:request(Port, ~"GET", ~"/slow", [], <<>>)}
    end),
    receive
    after 150 -> ok
    end,
    ?assertEqual(
        ok, arizona_conformance_driver:drain(?config(driver, Config), conformance_drain, 5000)
    ),
    receive
        {slow, Response} -> ?assertMatch({200, _, ~"SLOW_OK"}, Response)
    after 5000 -> ct:fail(slow_request_lost)
    end.

%% The rolling-deploy drain: a live view answers it as it answers `drain/2`,
%% and the listener keeps serving, so the client's reconnect lands on it.
notify_drain_winds_sessions_down_and_keeps_serving(Config0) ->
    Config = drain_listener(Config0),
    Driver = ?config(driver, Config),
    Sock = live_socket(Config, ~"/drain_stop"),
    ?assertEqual(ok, arizona_conformance_driver:notify_drain(Driver, conformance_drain, 5000)),
    {text, Effect} = arizona_wire_client:ws_recv(Sock),
    ?assertMatch(#{~"e" := [_ | _]}, json:decode(Effect)),
    ?assertMatch({close, 1001, _}, arizona_wire_client:ws_recv(Sock)),
    ok = arizona_wire_client:ws_close(Sock),
    ?assertMatch({200, _, _}, req(Config, ~"GET", ~"/drain_stop", [])),
    Again = live_socket(Config, ~"/drain_stop"),
    ok = arizona_wire_client:ws_close(Again),
    ok = arizona_conformance_driver:stop(Driver, conformance_drain).

drains_answer_not_found_for_no_listener(Config) ->
    Driver = ?config(driver, Config),
    ?assertEqual({error, not_found}, arizona_conformance_driver:drain(Driver, no_listener, 0)),
    ?assertEqual(
        {error, not_found}, arizona_conformance_driver:notify_drain(Driver, no_listener, 0)
    ).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

%% A listener of its own for one drain case: draining stops it. A case that
%% fails before its drain leaves it running, so a leftover is stopped first.
drain_listener(Config) ->
    Driver = ?config(driver, Config),
    case arizona_conformance_driver:stop(Driver, conformance_drain) of
        ok -> ok;
        {error, not_found} -> ok
    end,
    Routes = [
        {live, ~"/drain_stop", arizona_drainable, #{bindings => #{drain_mode => stop}}},
        {get, ~"/slow", arizona_slow_controller, #{state => #{delay_ms => 500}}},
        {ws, ~"/ws", #{}}
    ],
    start_group_listener(conformance_drain, Routes, #{}, Config).

req(Config, Method, Path, Headers) ->
    arizona_wire_client:request(?config(port, Config), Method, Path, Headers, <<>>).

header(Name, Headers) ->
    arizona_wire_client:header(Name, Headers).

%% The text `arizona_crashable` renders after `Label: `, past its slot marker.
rendered(Label, Body) ->
    Pattern = <<Label/binary, ": (?:<!--[^>]*-->)?([^<]*)">>,
    {match, [Text]} = re:run(Body, Pattern, [unicode, {capture, [1], binary}]),
    Text.

%% The `set-cookie` lines setting cookie `Name`.
cookies(Name, Headers) ->
    Prefix = <<Name/binary, "=">>,
    [C || C <- arizona_wire_client:headers(~"set-cookie", Headers), is_prefix(Prefix, C)].

is_prefix(Prefix, Bin) ->
    case Bin of
        <<Prefix:(byte_size(Prefix))/binary, _/binary>> -> true;
        _ -> false
    end.

%% The value of the one `az_session` cookie a response sets.
session_value(Headers) ->
    [<<"az_session=", Rest/binary>>] = cookies(~"az_session", Headers),
    [Value | _] = binary:split(Rest, ~";"),
    Value.

same_origin(Scheme, Config) ->
    <<Scheme/binary, "://localhost:", (integer_to_binary(?config(port, Config)))/binary>>.

upgrade_head(Config, Path, Headers) ->
    arizona_wire_client:ws_upgrade_head(?config(port, Config), Path, #{headers => Headers}).

ws_connect(Config, Path) ->
    arizona_wire_client:ws_connect(?config(port, Config), Path, #{}).

%% An open socket on `Path` with its connect frame consumed.
live_socket(Config, Path) ->
    {ok, Sock} = ws_connect(Config, Path),
    {text, Connect} = arizona_wire_client:ws_recv(Sock),
    #{~"a" := _} = json:decode(Connect),
    Sock.

navigate(Sock, Kind, Path) ->
    arizona_wire_client:ws_send_json(Sock, [Kind, #{~"path" => Path, ~"qs" => ~""}]).
