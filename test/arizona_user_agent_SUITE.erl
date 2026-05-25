-module(arizona_user_agent_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([browser_true_for_browser_uas/1]).
-export([browser_false_for_non_browser/1]).
-export([os_classifies_real_user_agents/1]).
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
            os_classifies_real_user_agents,
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

os_classifies_real_user_agents(Config) when is_list(Config) ->
    %% A corpus of real UA strings, one per platform. The OS-identifying prefix of
    %% each is verbatim from a real UA (sourced from DeviceAtlas / user-agents.net);
    %% the browser-version tail is trimmed to the 100-col limit -- os/1 only inspects
    %% the OS tokens, which are kept. The rows that also carry an overlapping token
    %% (noted inline) lock in the rule ordering: a regression that moved
    %% `Linux`/`Mac OS X` ahead of the specific tokens would fail here. (watchOS has
    %% no real browser; its row is a representative client-set UA.)
    Corpus = [
        {~"Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) Mobile/15E148", ios},
        {~"Mozilla/5.0 (iPad; CPU OS 17_0 like Mac OS X) Mobile/15E148", ios},
        {~"Mozilla/5.0 (Linux; Android 14; Pixel 7) Mobile Safari/537.36", android},
        {~"Mozilla/5.0 (Windows NT 10.0; Win64; x64) Safari/537.36", windows},
        {~"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) Safari/605.1.15", macos},
        {~"Mozilla/5.0 (X11; Linux x86_64) Chrome/120.0 Safari/537.36", linux},
        %% Apple TV: carries "Mac OS X" and is not even a Mozilla UA.
        {~"AppleCoreMedia/1.0.0.20L563 (Apple TV; CPU OS 16_5 like Mac OS X)", tvos},
        %% LG webOS: token is "Web0S" (with a zero) and the UA also carries "Linux".
        {~"Mozilla/5.0 (Web0S; Linux/SmartTV) Chrome/79.0 Safari/537.36", webos},
        %% Samsung Tizen: also carries "Linux".
        {~"Mozilla/5.0 (SMART-TV; Linux; Tizen 6.0) SamsungBrowser/4.0", tizen},
        %% watchOS has no browser; this is a representative client-set UA.
        {~"MyArizonaApp/1.0 (watchOS 10.0; Apple Watch)", watchos}
    ],
    lists:foreach(
        fun({UA, Expected}) ->
            %% Pair the UA into the assertion so a failure names the offending UA.
            ?assertEqual({UA, Expected}, {UA, arizona_user_agent:os(UA)})
        end,
        Corpus
    ).

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
