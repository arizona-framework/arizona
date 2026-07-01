-module(arizona_config).
-moduledoc """
Reads the `arizona` application environment, resolving environment-variable
references declared in config.

Any config value may be written as an env-var reference instead of a literal:

- `{env, "VAR"}` -- **required**: reads `$VAR` and returns it as a binary;
  raises `env_not_set` if the variable is unset. Suits a secret with no
  sensible default (e.g. `{secret_key, {env, "SECRET_KEY"}}`).
- `{env, "VAR", Default}` -- **optional**: reads `$VAR`, coercing it to the
  type of `Default`; falls back to `Default` when the variable is unset.

References resolve **anywhere** in the config surface, including nested inside
the `server` map:

```erlang
{arizona, [
    {secret_key, {env, "SECRET_KEY"}},
    {session_secure, {env, "SESSION_SECURE", false}},
    {server, #{
        scheme => {env, "SCHEME", http},
        transport_opts => [{port, {env, "PORT", 8080}}],
        routes => [ ... ]
    }}
]}.
```

## Type coercion

Environment variables are always strings; the resolver coerces the raw string
to match the `Default`'s type (the required form has no default, so it yields a
binary):

| `Default` type | Coercion |
|----------------|----------|
| integer        | `list_to_integer/1` |
| float          | `list_to_float/1` |
| boolean        | `"true"`/`"false"` (case-insensitive) |
| binary         | `list_to_binary/1` |
| atom (non-boolean) | `list_to_existing_atom/1` |
| list (`[]` or of binaries) | comma-split, each element trimmed `list_to_binary/1` |
| string (char list, e.g. a cert path) | the raw string, unchanged |
| (required, no default) | `list_to_binary/1` |

A set-but-**empty** variable (`PORT=`) is treated as **unset**: the optional form
falls back to `Default`, the required form raises `env_not_set`. A non-empty value
that fails to coerce (`PORT=abc` for an integer default) raises `env_invalid_value`,
rendered by `format_error/2` into an actionable boot message.

## Resolution scope

`resolve/1` recurses into maps, lists, and 2-tuples (proplist pairs like
`{port, ...}` / `{certfile, ...}`), so nested references resolve. It does **not**
descend into tuples of three or more elements: route tuples (`{live, Path,
Handler, Opts}` and friends) pass through untouched, so an operator-supplied
`{env, _, _}` term sitting inside a route's `bindings`/`state` is never rewritten.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get_env/1]).
-export([get_env/2]).
-export([resolve/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Error formatting exports
%% --------------------------------------------------------------------

-export([format_error/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Like `application:get_env(arizona, Key)`, but resolves any `{env, ...}`
references in the stored value. Returns `undefined` when `Key` is unset.
""".
-spec get_env(Key) -> {ok, Value} | undefined when
    Key :: atom(),
    Value :: term().
get_env(Key) ->
    case application:get_env(arizona, Key) of
        {ok, Value} -> {ok, resolve(Value)};
        undefined -> undefined
    end.

-doc """
Like `application:get_env(arizona, Key, Default)`, but resolves any `{env, ...}`
references in the stored value. `Default` is returned (unresolved) when `Key` is
unset.
""".
-spec get_env(Key, Default) -> Value when
    Key :: atom(),
    Default :: term(),
    Value :: term().
get_env(Key, Default) ->
    resolve(application:get_env(arizona, Key, Default)).

-doc """
Resolves `{env, "VAR"}` / `{env, "VAR", Default}` references in `Term`, recursing
through maps, lists, and 2-tuples. Non-reference values are returned unchanged.
""".
-spec resolve(Term) -> term() when
    Term :: term().
resolve({env, Var}) when is_list(Var) ->
    resolve_env(Var, required);
resolve({env, Var, Default}) when is_list(Var) ->
    resolve_env(Var, {default, Default});
resolve(Map) when is_map(Map) ->
    #{K => resolve(V) || K := V <- Map};
resolve(List) when is_list(List) ->
    [resolve(E) || E <- List];
resolve({A, B}) ->
    {resolve(A), resolve(B)};
resolve(Other) ->
    Other.

%% --------------------------------------------------------------------
%% Error formatting
%% --------------------------------------------------------------------

-doc """
Formats `arizona_config` runtime errors into a human-readable message. Picked up
by `erl_error:format_exception/3` via the `error_info` annotation at the raise site.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({env_not_set, Var}, _ST) ->
    #{
        general =>
            io_lib:format(
                "required environment variable ~ts is not set; either export it or "
                "declare a default with `{env, ~tp, Default}`",
                [Var, Var]
            )
    };
format_error({env_invalid_value, Var, Value, Type}, _ST) ->
    #{
        general =>
            io_lib:format(
                "environment variable ~ts=~tp is not a valid ~ts",
                [Var, Value, type_label(Type)]
            )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% `os:getenv` returns `false` (unset) or a string. An empty string is treated as
%% unset too, so a bare `PORT=` in an env file falls back to the default and a
%% required var with no value raises `env_not_set` (rather than a silent empty
%% binary that only fails later).
resolve_env(Var, Spec) ->
    case os:getenv(Var) of
        false -> unset(Var, Spec);
        "" -> unset(Var, Spec);
        Value -> coerce(Var, Value, Spec)
    end.

unset(Var, required) ->
    erlang:error({env_not_set, Var}, [Var], [{error_info, #{module => ?MODULE}}]);
unset(_Var, {default, Default}) ->
    Default.

%% The required form carries no default, so the raw string is the value (binary).
coerce(_Var, Value, required) ->
    list_to_binary(Value);
%% Coerce to the type of the default. A malformed value (e.g. `PORT=abc` for an
%% integer default) raises a `format_error`-rendered `env_invalid_value` at boot
%% rather than a cryptic `badarg` from deep in the conversion.
coerce(Var, Value, {default, Default}) ->
    Type = default_type(Default),
    try
        convert(Type, Value)
    catch
        error:_ -> invalid(Var, Value, Type)
    end.

%% The coercion target inferred from the default's type. `is_boolean` is checked
%% before `is_atom` because booleans are atoms. A char-list (string) default (e.g. a
%% tls cert path) keeps the raw string; an empty list or a list of binaries (e.g.
%% `csrf_origins`, `secret_key_previous`) comma-splits into trimmed binaries. `[]` is
%% ambiguous (an empty list and an empty string are the same term) and resolves to
%% the list case, the documented empty default.
default_type(Default) when is_integer(Default) -> integer;
default_type(Default) when is_float(Default) -> float;
default_type(Default) when is_boolean(Default) -> boolean;
default_type(Default) when is_binary(Default) -> binary;
default_type(Default) when is_atom(Default) -> atom;
default_type([]) -> binary_list;
default_type([H | _]) when is_binary(H) -> binary_list;
default_type([H | _]) when is_integer(H) -> string.

%% Only integer/float/boolean/atom can fail to convert (and are re-raised as
%% `env_invalid_value`); binary/binary_list/string always succeed.
convert(integer, Value) -> list_to_integer(Value);
convert(float, Value) -> list_to_float(Value);
convert(boolean, Value) -> to_boolean(Value);
convert(binary, Value) -> list_to_binary(Value);
convert(atom, Value) -> list_to_existing_atom(Value);
convert(binary_list, Value) -> comma_split(Value);
convert(string, Value) -> Value.

invalid(Var, Value, Type) ->
    erlang:error(
        {env_invalid_value, Var, Value, Type}, [Var], [{error_info, #{module => ?MODULE}}]
    ).

type_label(integer) -> "integer";
type_label(float) -> "float";
type_label(boolean) -> "boolean (expected \"true\" or \"false\")";
type_label(atom) -> "atom (it must already be a known atom)";
type_label(binary) -> "binary string";
type_label(binary_list) -> "comma-separated list";
type_label(string) -> "string".

comma_split(Value) ->
    Parts = [string:trim(S) || S <- string:split(Value, ",", all)],
    [list_to_binary(P) || P <- Parts, P =/= ""].

to_boolean(Value) ->
    case string:lowercase(Value) of
        "true" -> true;
        "false" -> false
    end.
