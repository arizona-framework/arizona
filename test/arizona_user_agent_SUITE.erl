-module(arizona_user_agent_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([browser_true_for_browser_uas/1]).
-export([browser_false_for_non_browser/1]).
-export([os_detects_each_platform/1]).
-export([os_prefers_specific_over_overlapping/1]).
-export([os_other_when_unknown/1]).
-export([mobile_true_for_mobile_uas/1]).
-export([mobile_false_for_desktop/1]).

all() ->
    [{group, browser}, {group, os}, {group, mobile}].

groups() ->
    [
        {browser, [parallel], [
            browser_true_for_browser_uas,
            browser_false_for_non_browser
        ]},
        {os, [parallel], [
            os_detects_each_platform,
            os_prefers_specific_over_overlapping,
            os_other_when_unknown
        ]},
        {mobile, [parallel], [
            mobile_true_for_mobile_uas,
            mobile_false_for_desktop
        ]}
    ].

%% --------------------------------------------------------------------
%% browser/1
%% --------------------------------------------------------------------

browser_true_for_browser_uas(Config) when is_list(Config) ->
    Chrome = ~"Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0 Safari/537.36",
    SafariIos = ~"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) Mobile/15E148",
    ?assert(arizona_user_agent:browser(Chrome)),
    ?assert(arizona_user_agent:browser(SafariIos)).

browser_false_for_non_browser(Config) when is_list(Config) ->
    %% A native app's HTTP stack carries no "Mozilla" -- the signal to render
    %% native instead of HTML.
    ?assertNot(arizona_user_agent:browser(~"MyArizonaApp/1.2.0 (iOS 17.0)")),
    ?assertNot(arizona_user_agent:browser(<<>>)).

%% --------------------------------------------------------------------
%% os/1
%% --------------------------------------------------------------------

os_detects_each_platform(Config) when is_list(Config) ->
    ?assertEqual(ios, arizona_user_agent:os(~"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0)")),
    ?assertEqual(ios, arizona_user_agent:os(~"Mozilla/5.0 (iPad; CPU OS 17_0)")),
    ?assertEqual(windows, arizona_user_agent:os(~"Mozilla/5.0 (Windows NT 10.0; Win64; x64)")),
    ?assertEqual(macos, arizona_user_agent:os(~"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")),
    ?assertEqual(linux, arizona_user_agent:os(~"Mozilla/5.0 (X11; Linux x86_64)")).

os_prefers_specific_over_overlapping(Config) when is_list(Config) ->
    %% Android UAs also carry "Linux"; iOS UAs also carry "Mac OS X". The more
    %% specific substring must win.
    Android = ~"Mozilla/5.0 (Linux; Android 14; Pixel 7) Chrome/120.0 Mobile Safari",
    Ios = ~"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) Mobile/15E148",
    ?assertEqual(android, arizona_user_agent:os(Android)),
    ?assertEqual(ios, arizona_user_agent:os(Ios)).

os_other_when_unknown(Config) when is_list(Config) ->
    ?assertEqual(other, arizona_user_agent:os(<<>>)),
    ?assertEqual(other, arizona_user_agent:os(~"MyArizonaApp/1.2.0")).

%% --------------------------------------------------------------------
%% mobile/1
%% --------------------------------------------------------------------

mobile_true_for_mobile_uas(Config) when is_list(Config) ->
    %% iPhone Safari carries "Mobile" (matches "Mobi"); Android carries "Android".
    Iphone = ~"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) Mobile/15E148",
    Android = ~"Mozilla/5.0 (Linux; Android 14; Pixel 7) Chrome/120.0 Mobile Safari",
    ?assert(arizona_user_agent:mobile(Iphone)),
    ?assert(arizona_user_agent:mobile(Android)).

mobile_false_for_desktop(Config) when is_list(Config) ->
    Desktop = ~"Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/120.0 Safari/537.36",
    ?assertNot(arizona_user_agent:mobile(Desktop)),
    ?assertNot(arizona_user_agent:mobile(<<>>)).
