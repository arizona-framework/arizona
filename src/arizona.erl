-module(arizona).

-opaque socket() :: map().
-export_type([socket/0]).

-opaque macros() :: map().
-export_type([macros/0]).

-opaque tree() :: list().
-export_type([tree/0]).

-type event_name() :: binary().
-export_type([event_name/0]).

-opaque payload() :: map().
-export_type([payload/0]).
