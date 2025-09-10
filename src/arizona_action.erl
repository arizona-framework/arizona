-module(arizona_action).
-moduledoc ~"""
Action system for Arizona callback responses.

Defines the action types that can be returned from callbacks like
`handle_event/3` and `handle_info/2`. Actions allow multiple responses
and built-in functionality like redirects and reloads.

## Usage

Instead of the old pattern:
```erlang
handle_event(Event, Params, State) ->
    {reply, Reply, State}  % or {noreply, State}
```

Use the new action pattern:
```erlang
handle_event(Event, Params, State) ->
    {[Action1, Action2, ...], State}
```

## Examples

### Single reply:
```erlang
{[{reply, #{success => true}}], State}
```

### Multiple replies:
```erlang
{[{reply, #{step => 1}}, {reply, #{step => 2}}], State}
```

### Reply with reload:
```erlang
{[{reply, #{saved => true}}, reload], State}
```

### Redirect:
```erlang
{[{redirect, ~"/dashboard", ~"_self"}], State}
```

### No action (equivalent to old noreply):
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
-export_type([redirect_target/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal action() ::
    {reply, reply_data()}
    | {redirect, redirect_url(), redirect_target()}
    | reload.

-nominal actions() :: [action()].

-nominal reply_data() :: dynamic().
-nominal redirect_url() :: binary().
% e.g., ~"_self", ~"_blank"
-nominal redirect_target() :: binary().
