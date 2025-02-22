-module(arizona_html).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([scripts/0]).

%

-ignore_xref([scripts/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec scripts() -> binary().
scripts() ->
    ~"""
    <script src="assets/js/arizona/morphdom.min.js"></script>
    <script src="assets/js/arizona/main.js"></script>
    """.
