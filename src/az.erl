-module(az).
-moduledoc """
Short alias module for `arizona_template` and `arizona_stateful`.

Provides the same runtime API surface as `arizona_template` under a
shorter prefix so handcrafted templates that don't use the macros can
be written as `az:get(...)`, `az:html(...)`, etc. It also re-exports
the common types so users writing handler specs can reference them
through a single module: `az:bindings()`, `az:mount_ret()`, etc.

The parse transform recognizes both `arizona_template:html/1` and
`az:html/1` (same for `each/2`), so either alias compiles to the same
optimized template map.

## Parse transform stubs

`html/1` and `each/2` are stubs -- they exist so callers can write
`az:html({...})` and the parse transform can detect and replace the
call. They never actually execute at runtime; calling them directly
errors with `parse_transform_not_applied`.

## Layout helper

`inner_content/1` is the only function that does real runtime work
not delegated to `arizona_template`. It extracts the `inner_content`
binding, used by stateless layouts to render the wrapped page.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get/2]).
-export([get/3]).
-export([get_lazy/3]).
-export([inner_content/1]).
-export([track/1]).
-export([html/1]).
-export([stateful/2]).
-export([stateless/2]).
-export([stateless/3]).
-export([each/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([
    get/2,
    get/3,
    get_lazy/3,
    inner_content/1,
    track/1,
    html/1,
    stateful/2,
    stateless/2,
    stateless/3,
    each/2
]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([bindings/0]).
-export_type([template/0]).
-export_type([render_fun/0]).
-export_type([request/0]).
-export_type([resets/0]).
-export_type([effect/0]).
-export_type([effects/0]).
-export_type([event_name/0]).
-export_type([event_payload/0]).
-export_type([mount_ret/0]).
-export_type([handle_event_ret/0]).
-export_type([handle_info_ret/0]).
-export_type([handle_update_ret/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-type bindings() :: arizona_template:bindings().
-type template() :: arizona_template:template().
-type render_fun() :: arizona_template:render_fun().
-type request() :: arizona_req:request().
-type resets() :: arizona_stateful:resets().
-type effect() :: arizona_stateful:effect().
-type effects() :: arizona_stateful:effects().
-type event_name() :: arizona_stateful:event_name().
-type event_payload() :: arizona_stateful:event_payload().
-type mount_ret() :: arizona_stateful:mount_ret().
-type handle_event_ret() :: arizona_stateful:handle_event_ret().
-type handle_info_ret() :: arizona_stateful:handle_info_ret().
-type handle_update_ret() :: arizona_stateful:handle_update_ret().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Alias for `arizona_template:get/2`.
""".
-spec get(Key, Bindings) -> term() when
    Key :: term(),
    Bindings :: map().
get(Key, Bindings) ->
    arizona_template:get(Key, Bindings).

-doc """
Alias for `arizona_template:get/3`.
""".
-spec get(Key, Bindings, Default) -> term() when
    Key :: term(),
    Bindings :: map(),
    Default :: term().
get(Key, Bindings, Default) ->
    arizona_template:get(Key, Bindings, Default).

-doc """
Alias for `arizona_template:get_lazy/3`.
""".
-spec get_lazy(Key, Bindings, DefaultFun) -> term() when
    Key :: term(),
    Bindings :: map(),
    DefaultFun :: fun(() -> term()).
get_lazy(Key, Bindings, DefaultFun) ->
    arizona_template:get_lazy(Key, Bindings, DefaultFun).

-doc """
Returns the `inner_content` binding from a layout's bindings map.

Used by stateless layout modules to render the wrapped page content.
Errors with `{badkey, inner_content}` if not present.
""".
-spec inner_content(Bindings) -> term() when
    Bindings :: map().
inner_content(Bindings) ->
    maps:get(inner_content, Bindings).

-doc """
Alias for `arizona_template:track/1`.
""".
-spec track(Key) -> ok when
    Key :: term().
track(Key) ->
    arizona_template:track(Key).

-doc """
Parse transform stub. Calling this at runtime errors -- the parse
transform replaces `az:html(...)` (or `arizona_template:html(...)`)
calls with compiled template maps before code generation.
""".
-spec html(term()) -> no_return().
html(Elems) ->
    arizona_template:html(Elems).

-doc """
Alias for `arizona_template:stateful/2`.
""".
-spec stateful(Handler, Props) -> arizona_template:stateful_descriptor() when
    Handler :: module(),
    Props :: map().
stateful(Handler, Props) ->
    arizona_template:stateful(Handler, Props).

-doc """
Alias for `arizona_template:stateless/2`.
""".
-spec stateless(Callback, Props) -> arizona_template:stateless_descriptor() when
    Callback :: fun((map()) -> arizona_template:template()),
    Props :: map().
stateless(Callback, Props) ->
    arizona_template:stateless(Callback, Props).

-doc """
Alias for `arizona_template:stateless/3`.
""".
-spec stateless(Handler, Fun, Props) -> arizona_template:stateless_descriptor() when
    Handler :: module(),
    Fun :: atom(),
    Props :: map().
stateless(Handler, Fun, Props) ->
    arizona_template:stateless(Handler, Fun, Props).

-doc """
Parse transform stub. The parse transform replaces `az:each(Fun, Source)`
with compiled iteration code; this runtime body is only reached if the
parse transform did not run, in which case the call will fail.
""".
-spec each(Fun, Source) -> no_return() when
    Fun :: fun(),
    Source :: term().
each(Fun, Source) ->
    arizona_template:each(Fun, Source).
