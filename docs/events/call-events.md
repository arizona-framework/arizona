# Call Events

- [Request-Reply](#request-reply)
- [Client API](#client-api)
  - [callEvent/callEventFrom](#calleventcalleventfrom)
- [Server Callbacks](#server-callbacks)
  - [{reply, Ref, Data}](#reply-ref-data)
- [Timeouts](#timeouts)

## Request-Reply

Call events extend WebSocket events with a request-reply pattern. Instead of
a fire-and-forget message, the client sends an event and receives a **Promise**
that resolves when the server explicitly replies. This is useful for operations
where the client needs a server-computed result before proceeding, such as:

- Form validation that depends on server state (e.g., checking if a username
  is taken).
- Data lookups where the result must be used in JavaScript (e.g., populating
  an autocomplete dropdown).
- Any workflow where the client needs confirmation that a server operation
  completed successfully.

The flow is:

1. Client calls `callEvent` or `callEventFrom`, receiving a Promise.
2. The server's `handle_event/3` receives the event with a reference (`Ref`).
3. The server includes `{reply, Ref, Data}` in its actions list.
4. The client's Promise resolves with `Data`.

## Client API

Two methods provide the request-reply pattern, mirroring `pushEvent` and
`pushEventTo`.

### callEvent/callEventFrom

`callEvent(event, params?, options?)` sends a call event to the **view** and
returns a Promise that resolves with the server's reply data.

`callEventFrom(statefulId, event, params?, options?)` sends a call event to
a **specific stateful component** and returns a Promise.

```javascript
// Call the view and await the reply
const result = await arizona.callEvent("validate", { email: "test@example.com" });
console.log(result); // { valid: true }

// Call a stateful component and await the reply
const data = await arizona.callEventFrom("search-1", "query", { term: "arizona" });
console.log(data); // { results: [...] }
```

Both methods accept an optional `options` object. Currently the supported
option is `timeout` (see [Timeouts](#timeouts)).

## Server Callbacks

Call events arrive in `handle_event/3` just like regular WebSocket events, but
with an important difference: the `Params` argument is a **tuple** containing
a reference and the actual parameters.

### {reply, Ref, Data}

When a call event arrives, the second argument to `handle_event/3` is
`{Ref, ActualParams}` instead of just `Params`. The `Ref` is an opaque
reference of type `arizona_stateful:event_ref()` that ties the reply back to
the client's Promise.

Include `{reply, Ref, Data}` in the returned actions list to resolve the
Promise on the client side:

```erlang
handle_event(~"validate", {Ref, #{~"email" := Email}}, View) ->
    Valid = is_valid_email(Email),
    {[{reply, Ref, #{valid => Valid}}], View}.
```

```erlang
%% Stateful component example
handle_event(~"query", {Ref, #{~"term" := Term}}, State) ->
    Results = search(Term),
    {[{reply, Ref, #{results => Results}}], State}.
```

The `Data` value can be any term that is JSON-serializable (maps, lists,
binaries, numbers, booleans, `null`).

**Important:** If the returned actions list does **not** include a
`{reply, Ref, _}` action, the client's Promise will never resolve. It will
either hang indefinitely or reject when the client-side timeout expires (if
one was set). Always ensure that every call event handler includes a reply
action.

You can include other actions alongside the reply. All actions are processed
in order:

```erlang
handle_event(~"save", {Ref, #{~"data" := Data}}, View) ->
    ok = persist(Data),
    {[
        {reply, Ref, #{saved => true}},
        {dispatch, ~"save_complete", #{}}
    ], View}.
```

## Timeouts

Pass a `timeout` option (in milliseconds) to set a client-side deadline for
the reply. If the server does not reply within the specified time, the Promise
rejects with a timeout error.

```javascript
try {
    const result = await arizona.callEvent("slow_operation", {}, { timeout: 5000 });
    console.log("Got result:", result);
} catch (error) {
    console.error("Operation timed out");
}
```

```javascript
// Timeout on a stateful component call
try {
    const data = await arizona.callEventFrom(
        "report-1",
        "generate",
        { format: "pdf" },
        { timeout: 30000 }
    );
    downloadFile(data.url);
} catch (error) {
    showError("Report generation timed out. Please try again.");
}
```

When no `timeout` is specified, the Promise will wait indefinitely for a reply.
For user-facing operations, it is recommended to always set a reasonable timeout
to prevent the UI from appearing frozen.

See also:

- [WebSocket Events](websocket-events.md) -- fire-and-forget events
- [Actions](../actions/actions.md) -- all available action types
