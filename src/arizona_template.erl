-module(arizona_template).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([static/1]).
-export([dynamic/1]).
-export([dynamic_sequence/1]).
-export([dynamic_anno/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([template/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(template, {
    static :: [StaticContent :: binary()],
    dynamic :: tuple(),
    dynamic_sequence :: [pos_integer()],
    dynamic_anno :: tuple()
}).

-opaque template() :: #template{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec static(template()) -> [binary()].
static(#template{static = Static}) ->
    Static.

-spec dynamic(template()) -> tuple().
dynamic(#template{dynamic = Dynamic}) ->
    Dynamic.

-spec dynamic_sequence(template()) -> [pos_integer()].
dynamic_sequence(#template{dynamic_sequence = Sequence}) ->
    Sequence.

-spec dynamic_anno(template()) -> tuple().
dynamic_anno(#template{dynamic_anno = Anno}) ->
    Anno.
