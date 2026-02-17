# Actions

- [Overview](#overview)
- [Action Types](#action-types)
  - [dispatch](#dispatch)
  - [redirect](#redirect)
  - [reply](#reply)
  - [reload](#reload)
- [Multiple Actions](#multiple-actions)

## Overview

Actions are instructions returned by event handlers that tell the client to perform operations
beyond DOM updates. When `handle_event/3` or `handle_info/2` returns `{Actions, View}`, the actions
list is processed by the WebSocket transport and sent to the JavaScript client. While state changes
automatically produce DOM diffs, actions handle side effects like navigation, custom JS events, and
request-reply responses.

## Action Types

Arizona supports four action types.

### dispatch

Sends a custom event to the JavaScript client. The tuple takes the form `{dispatch, EventName,
Data}`, where `EventName` is a binary and `Data` is the payload delivered to the client. The client
can listen for dispatched events using `arizona.on(event, callback)`.

```erlang
handle_event(~"save", _Params, View) ->
    %% ... save logic ...
    {[{dispatch, ~"save_complete", #{status => ~"ok"}}], View}.
```

See [Client Events](../events/client-events.md) for listening to dispatched events.

### redirect

Instructs the client to navigate to a different URL. The tuple takes the form `{redirect, URL,
Options}`, where `URL` is a binary and `Options` is a map.

```erlang
handle_event(~"go_home", _Params, View) ->
    {[{redirect, ~"/dashboard", #{}}], View}.

%% Open in new tab:
handle_event(~"open_docs", _Params, View) ->
    {[{redirect, ~"https://docs.example.com", #{target => ~"_blank"}}], View}.
```

Options:

- `target` -- The browsing context (e.g., `~"_blank"` to open in a new tab).
- `window_features` -- A binary of features passed to `window.open` (e.g.,
  `~"width=600,height=400"`).

### reply

Responds to a `callEvent` or `callEventFrom` request from the client. The tuple takes the form
`{reply, Ref, Data}`, where `Ref` is the reference received from the event payload and `Data` is the
response sent back to the caller.

```erlang
handle_event(~"validate", {Ref, #{~"email" := Email}}, View) ->
    Valid = is_valid_email(Email),
    {[{reply, Ref, #{valid => Valid}}], View}.
```

See [Call Events](../events/call-events.md) for the full request-reply pattern.

### reload

Forces a full page reload on the client. Unlike the other action types, `reload` is a bare atom
rather than a tuple.

```erlang
handle_event(~"reset", _Params, View) ->
    {[reload], View}.
```

## Multiple Actions

Multiple actions can be returned in a single list. They are processed in order, so you can combine
side effects in one handler invocation.

```erlang
handle_event(~"submit", {Ref, Params}, View) ->
    %% Process form, reply to caller, dispatch notification, then redirect
    {[
        {reply, Ref, #{ok => true}},
        {dispatch, ~"form_submitted", #{}},
        {redirect, ~"/thank-you", #{}}
    ], View}.
```

See also: [WebSocket Events](../events/websocket-events.md), [Call
Events](../events/call-events.md), [Client Events](../events/client-events.md)
