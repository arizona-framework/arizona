-module(arizona_hierarchical_dict).
-moduledoc ~"""
Process dictionary storage for hierarchical component structures.

Provides global storage for hierarchical structure data during rendering,
allowing `arizona_hierarchical` to build component hierarchies without
explicit parameter passing. Maps stateful component IDs to their
rendering data (static templates and dynamic content).

## Usage Pattern

1. Store empty structure with `set_structure/1`
2. Add component data during traversal with `put_stateful_data/2`
3. Retrieve final structure for WebSocket updates
4. Clear with `clear/0` when done

## Example

```erlang
1> arizona_hierarchical_dict:set_structure(#{}).
undefined
2> Data = #{static => StaticTemplate, dynamic => DynamicContent}.
3> arizona_hierarchical_dict:put_stateful_data(~"comp1", Data).
#{~"comp1" => #{static => ..., dynamic => ...}}
4> arizona_hierarchical_dict:clear().
#{~"comp1" => #{static => ..., dynamic => ...}}
```
""".

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

-doc ~"""
Stores a hierarchical structure in the process dictionary.

Replaces any existing structure and returns the previous one.
Used to initialize structure storage before component traversal.
""".
-spec set_structure(Structure) -> OldStructure | undefined when
    Structure :: hierarchical_structure(),
    OldStructure :: hierarchical_structure().
set_structure(Structure) ->
    put(?MODULE, Structure).

-doc ~"""
Adds stateful component data to the stored structure.

Updates the hierarchical structure with rendering data for a specific
stateful component. Returns the updated structure or `undefined` if
no structure is stored.
""".
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

-doc ~"""
Clears the hierarchical structure from process dictionary.

Removes stored structure and returns it. Used to clean up
after hierarchical rendering is complete.
""".
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
