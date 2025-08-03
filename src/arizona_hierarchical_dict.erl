-module(arizona_hierarchical_dict).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([get_structure/0]).
-export([put_stateful_data/2]).
-export([clear/0]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([hierarchical_structure/0]).
-export_type([stateful_data/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-opaque hierarchical_structure() :: #{arizona_stateful:id() => stateful_data()}.

-nominal stateful_data() :: #{
    static := arizona_template:static(),
    dynamic := arizona_renderer:dynamic()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec new() -> ok.
new() ->
    _ = put(?MODULE, #{}),
    ok.

-spec get_structure() -> Structure | undefined when
    Structure :: hierarchical_structure().
get_structure() ->
    get(?MODULE).

-spec put_stateful_data(StatefulId, StatefulData) -> ok when
    StatefulId :: arizona_stateful:id(),
    StatefulData :: stateful_data().
put_stateful_data(StatefulId, StatefulData) ->
    case get_structure() of
        undefined ->
            ok;
        Structure ->
            _ = put(?MODULE, Structure#{StatefulId => StatefulData}),
            ok
    end.

-spec clear() -> ok.
clear() ->
    _ = erase(?MODULE),
    ok.
