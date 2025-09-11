-module(arizona_action).
-moduledoc ~"""
Action system for Arizona callback responses.

Defines the action types that can be returned from callbacks like
`handle_event/3` and `handle_info/2`. Actions allow multiple responses
and built-in functionality like redirects and reloads.

## Usage

Callbacks return a tuple containing a list of actions and the updated state:

```erlang
handle_event(Event, Params, State) ->
    {[Action1, Action2, ...], State}
```

No action:

```erlang
{[], State}
```
""".

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([action/0]).
-export_type([actions/0]).
-export_type([reply_data/0]).
-export_type([redirect_url/0]).
-export_type([redirect_options/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal action() ::
    {reply, reply_data()}
    | {redirect, redirect_url(), redirect_options()}
    | reload.

-nominal actions() :: [action()].

-nominal reply_data() :: dynamic().
-nominal redirect_url() :: binary().
-nominal redirect_options() :: #{
    target => binary(),
    window_features => binary()
}.
