-module(arizona_hierarchical_dict).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([set_structure/1]).
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

-nominal hierarchical_structure() :: #{arizona_stateful:id() => stateful_data()}.

-nominal stateful_data() :: #{
    static := arizona_template:static(),
    dynamic := arizona_renderer:dynamic()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec set_structure(Structure) -> OldStructure | undefined when
    Structure :: hierarchical_structure(),
    OldStructure :: hierarchical_structure().
set_structure(Structure) ->
    put(?MODULE, Structure).

-spec put_stateful_data(StatefulId, StatefulData) -> UpdatedStructure | undefined when
    StatefulId :: arizona_stateful:id(),
    StatefulData :: stateful_data(),
    UpdatedStructure :: hierarchical_structure().
put_stateful_data(StatefulId, StatefulData) ->
    case get_structure() of
        undefined ->
            undefined;
        Structure ->
            UpdatedStructure = Structure#{StatefulId => StatefulData},
            Structure = set_structure(UpdatedStructure),
            UpdatedStructure
    end.

-spec clear() -> Structure | undefined when
    Structure :: hierarchical_structure().
clear() ->
    erase(?MODULE).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec get_structure() -> Structure | undefined when
    Structure :: hierarchical_structure().
get_structure() ->
    get(?MODULE).
