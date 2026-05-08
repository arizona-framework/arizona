-module(arizona_handler).
-moduledoc """
Shared behaviour for callbacks common to `arizona_stateful` and
`arizona_view` handlers.

Declares the lifecycle callbacks that both a stateful component and
a route-level view implement identically: `render/1` (required) plus
the optional `handle_event/3`, `handle_info/2`, and `unmount/1`.
Mount-specific callbacks live in the specialized behaviour modules
because their signatures differ (`mount/1` for stateful,
`mount/2` for view). Parent-update interception
(`handle_update/2`) lives in `arizona_stateful` because views don't
receive it.

User modules declare both behaviours via the headers:

```erlang
%% A stateful component:
-include_lib("arizona/include/arizona_stateful.hrl").
%% expands to -behaviour(arizona_handler) + -behaviour(arizona_stateful)

%% A route-level view:
-include_lib("arizona/include/arizona_view.hrl").
%% expands to -behaviour(arizona_handler) + -behaviour(arizona_view)
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([call_render/2]).
-export([call_handle_event/4]).
-export([call_handle_info/3]).
-export([call_handle_update/3]).
-export([call_unmount/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Behaviour callbacks
%% --------------------------------------------------------------------

-doc """
Returns the template for the current bindings. Called on the first
render and after every state change.

Must use `?html(...)` so the parse transform can compile it into a
template map.
""".
-callback render(Bindings :: arizona_stateful:bindings()) ->
    arizona_template:template().

-doc """
Handles a client UI event. Optional.

`Event` is the event name dispatched from `az-click`, `az-submit`,
etc. `Payload` carries any auto-collected form data plus explicit
extra fields. Returns updated bindings, resets, and a list of
client-side effects to run after the diff applies.
""".
-callback handle_event(
    arizona_stateful:event_name(),
    arizona_stateful:event_payload(),
    arizona_stateful:bindings()
) -> arizona_stateful:handle_event_ret().

-doc """
Handles a mailbox message. Optional.
""".
-callback handle_info(Info :: term(), arizona_stateful:bindings()) ->
    arizona_stateful:handle_info_ret().

-doc """
Intercepts a parent prop update before it reaches the bindings. Optional.

When a parent re-renders and a child receives new props, this callback
gets the chance to merge / transform them. If not exported, the
framework merges `Props` into `Bindings` directly.

Only fires on `arizona_stateful` instances today -- views are
route-mounted and never receive parent prop updates. Kept in the
shared behaviour so the contract is unified if a future
view-embedding or patch-style URL primitive needs the same
reconciliation hook.
""".
-callback handle_update(Props :: arizona_stateful:bindings(), arizona_stateful:bindings()) ->
    arizona_stateful:handle_update_ret().

-doc """
Cleanup hook called when the instance is removed. Optional.
""".
-callback unmount(arizona_stateful:bindings()) -> term().

-optional_callbacks([handle_event/3, handle_info/2, handle_update/2, unmount/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Invokes the required `render/1` callback on a handler module.

Re-tags the failure when the immediate stack frame is the user's
`render/1` itself: a missing export becomes `{missing_callback, H,
render, 1}` and a head-pattern mismatch becomes
`{render_no_clause, H, Bindings}`. Errors raised from inside the
callback body propagate untagged.
""".
-spec call_render(Handler, Bindings) -> arizona_template:template() when
    Handler :: module(),
    Bindings :: arizona_stateful:bindings().
call_render(H, Bindings) ->
    try
        H:render(Bindings)
    catch
        error:undef:ST ->
            raise_or_propagate(
                undef, ST, H, render, {missing_callback, H, render, 1}, [H, Bindings]
            );
        error:function_clause:ST ->
            raise_or_propagate(
                function_clause, ST, H, render, {render_no_clause, H, Bindings}, [H, Bindings]
            )
    end.

-doc """
Invokes the optional `handle_event/3` callback.

Re-tags the failure when the immediate stack frame is the user's
`handle_event/3` itself: a missing export becomes
`{missing_callback, H, handle_event, 3}` and a no-matching-clause
crash becomes `{unhandled_event, H, Event, Bindings}`. Errors
raised from inside the callback body propagate untagged.
""".
-spec call_handle_event(Handler, Event, Payload, Bindings) ->
    {arizona_stateful:bindings(), arizona_stateful:resets(), arizona_stateful:effects()}
when
    Handler :: module(),
    Event :: binary(),
    Payload :: map(),
    Bindings :: arizona_stateful:bindings().
call_handle_event(H, Event, Payload, Bindings) ->
    try
        H:handle_event(Event, Payload, Bindings)
    catch
        error:undef:ST ->
            raise_or_propagate(
                undef,
                ST,
                H,
                handle_event,
                {missing_callback, H, handle_event, 3},
                [H, Event, Payload, Bindings]
            );
        error:function_clause:ST ->
            raise_or_propagate(
                function_clause,
                ST,
                H,
                handle_event,
                {unhandled_event, H, Event, Bindings},
                [H, Event, Payload, Bindings]
            )
    end.

-doc """
Invokes the optional `handle_info/2` callback.

Returns `ok` if the handler does not export `handle_info/2`, letting
the caller skip the subsequent diff + push cycle. Re-tags a
no-matching-clause crash at the callback's head as
`{unhandled_info, H, Info, Bindings}`; errors raised from inside
the callback body propagate untagged.
""".
-spec call_handle_info(Handler, Info, Bindings) ->
    {arizona_stateful:bindings(), arizona_stateful:resets(), arizona_stateful:effects()} | ok
when
    Handler :: module(),
    Info :: term(),
    Bindings :: arizona_stateful:bindings().
call_handle_info(H, Info, Bindings) ->
    case erlang:function_exported(H, handle_info, 2) of
        true ->
            try
                H:handle_info(Info, Bindings)
            catch
                error:function_clause:ST ->
                    raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        handle_info,
                        {unhandled_info, H, Info, Bindings},
                        [H, Info, Bindings]
                    )
            end;
        false ->
            ok
    end.

-doc """
Invokes the optional `handle_update/2` callback.

Falls back to merging `Props` into `Bindings` if the callback is not
exported. Re-tags a no-matching-clause crash at the callback's
head as `{unhandled_update, H, Props, Bindings}`; errors raised
from inside the callback body propagate untagged.
""".
-spec call_handle_update(Handler, Props, Bindings) ->
    arizona_stateful:handle_update_ret()
when
    Handler :: module(),
    Props :: map(),
    Bindings :: arizona_stateful:bindings().
call_handle_update(H, Props, Bindings) ->
    case erlang:function_exported(H, handle_update, 2) of
        true ->
            try
                H:handle_update(Props, Bindings)
            catch
                error:function_clause:ST ->
                    raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        handle_update,
                        {unhandled_update, H, Props, Bindings},
                        [H, Props, Bindings]
                    )
            end;
        false ->
            {maps:merge(Bindings, Props), #{}}
    end.

-doc """
Invokes the optional `unmount/1` callback on a handler module.

No-op if the callback is not exported. Re-tags a no-matching-clause
crash at the callback's head as `{unhandled_unmount, H, Bindings}`;
errors raised from inside the callback body propagate untagged.
""".
-spec call_unmount(Handler, Bindings) -> ok when
    Handler :: module(),
    Bindings :: arizona_stateful:bindings().
call_unmount(H, Bindings) ->
    case erlang:function_exported(H, unmount, 1) of
        true ->
            try
                H:unmount(Bindings)
            catch
                error:function_clause:ST ->
                    raise_or_propagate(
                        function_clause,
                        ST,
                        H,
                        unmount,
                        {unhandled_unmount, H, Bindings},
                        [H, Bindings]
                    )
            end;
        false ->
            ok
    end.

-doc """
Formats dispatcher-tagged errors raised by the `call_*` wrappers above.

Picked up by `erl_error:format_exception/3` via the `error_info`
annotation, so the dev error page (and crash log) reads a sentence
that names the failing module/callback and (when known) the view id.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({missing_callback, Mod, Name, Arity}, [{_M, _F, Args, _Info} | _]) ->
    %% Bindings is always the LAST arg to every dispatcher (see call_*/N
    %% above). Pull it from the failing stack frame so the message can
    %% name the view that was being served.
    Bindings = lists:last(Args),
    #{
        general => io_lib:format(
            "while serving view ~0tp: module ~s does not export ~s/~b. "
            "Add the callback to make this code path work.",
            [view_id(Bindings), Mod, Name, Arity]
        )
    };
format_error({unhandled_event, Mod, Event, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while handling event ~0tp on view ~0tp: no clause in "
            "~s:handle_event/3 matches that event name. Add a clause "
            "or fix the az-click attribute that dispatched it.",
            [Event, view_id(Bindings), Mod]
        )
    };
format_error({unhandled_info, Mod, Info, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while delivering message ~0tp on view ~0tp: no clause "
            "in ~s:handle_info/2 matches that message.",
            [Info, view_id(Bindings), Mod]
        )
    };
format_error({unhandled_update, Mod, Props, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while applying parent props ~0tp on view ~0tp: no clause "
            "in ~s:handle_update/2 matches.",
            [Props, view_id(Bindings), Mod]
        )
    };
format_error({unhandled_unmount, Mod, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while unmounting view ~0tp: no clause in ~s:unmount/1 "
            "matches.",
            [view_id(Bindings), Mod]
        )
    };
format_error({render_no_clause, Mod, Bindings}, _ST) ->
    #{
        general => io_lib:format(
            "while rendering view ~0tp: no clause in ~s:render/1 "
            "matches the current bindings.",
            [view_id(Bindings), Mod]
        )
    }.

view_id(Bindings) ->
    case Bindings of
        #{id := ViewId} -> ViewId;
        _ -> undefined
    end.

%% Used by every dispatcher catch clause. When the top stack frame is the
%% user's exact callback, raises Reason with an error_info annotation
%% pointing at this module. Otherwise propagates the original
%% `error:OrigReason` untouched -- failures from inside the callback body
%% are not the dispatcher's concern.
raise_or_propagate(_OrigReason, [{H, Fn, _, _} | _], H, Fn, Reason, ErrorArgs) ->
    erlang:error(Reason, ErrorArgs, [{error_info, #{module => ?MODULE}}]);
raise_or_propagate(OrigReason, ST, _H, _Fn, _Reason, _ErrorArgs) ->
    erlang:raise(error, OrigReason, ST).
