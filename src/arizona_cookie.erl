-module(arizona_cookie).
-moduledoc """
`Set-Cookie` serialization (RFC 6265 §4.1), independent of any HTTP server.

A response cookie is stashed on the `arizona_req:request()` by
`arizona_req:put_resp_cookie/4` and by the session and flash writers, and
`arizona_req:flush/1` serializes it here, so every server adapter emits the
same bytes. `arizona_session` budgets the `session_max_bytes` cap against this
exact format.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([serialize/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Builds a `Set-Cookie` header value.

Attributes are appended in this fixed order: `Domain`, `Path`, `Max-Age`,
`Expires`, `Secure`, `HttpOnly`, `SameSite`. The boolean flags appear only when
`true`.

Every caller-supplied binary is checked against the RFC 6265 §4.1.1 grammar
first, and a violation raises rather than reaching the wire, where a `;` or a
line break in a value echoed from user input would smuggle attributes or split
the header:

- `{invalid_cookie_name, Bin}` -- `Name` is empty or has a byte outside an
  RFC 9110 `token`
- `{invalid_cookie_value, Bin}` -- `Value` has a byte outside `cookie-octet`
  (CTL, SP, DQUOTE, `,`, `;`, `\\`)
- `{invalid_cookie_attr, Attr, Bin}` -- `Domain`, `Path`, or `Expires` holds a
  byte that would end the attribute or the line
""".
-spec serialize(Name, Value, Opts) -> binary() when
    Name :: binary(),
    Value :: binary(),
    Opts :: arizona_req:resp_cookie_opts().
serialize(Name, Value, Opts) when is_binary(Name), is_binary(Value), is_map(Opts) ->
    ok = valid_name(Name),
    ok = valid_value(Value, Value),
    iolist_to_binary([
        Name,
        $=,
        Value,
        attr_domain(Opts),
        attr_path(Opts),
        attr_max_age(Opts),
        attr_expires(Opts),
        attr_secure(Opts),
        attr_http_only(Opts),
        attr_same_site(Opts)
    ]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

attr_domain(#{domain := D}) when is_binary(D) ->
    ok = valid_attr(domain, D, D),
    [~"; Domain=", D];
attr_domain(_) ->
    [].

attr_path(#{path := P}) when is_binary(P) ->
    ok = valid_attr(path, P, P),
    [~"; Path=", P];
attr_path(_) ->
    [].

attr_max_age(#{max_age := N}) when is_integer(N), N >= 0 ->
    [~"; Max-Age=", integer_to_binary(N)];
attr_max_age(_) ->
    [].

attr_expires(#{expires := E}) when is_binary(E) ->
    ok = valid_attr(expires, E, E),
    [~"; Expires=", E];
attr_expires(_) ->
    [].

attr_secure(#{secure := true}) -> ~"; Secure";
attr_secure(_) -> [].

attr_http_only(#{http_only := true}) -> ~"; HttpOnly";
attr_http_only(_) -> [].

attr_same_site(#{same_site := strict}) -> ~"; SameSite=Strict";
attr_same_site(#{same_site := lax}) -> ~"; SameSite=Lax";
attr_same_site(#{same_site := none}) -> ~"; SameSite=None";
attr_same_site(_) -> [].

valid_name(<<>>) ->
    error({invalid_cookie_name, <<>>});
valid_name(Name) ->
    case lists:all(fun is_token_char/1, binary_to_list(Name)) of
        true -> ok;
        false -> error({invalid_cookie_name, Name})
    end.

%% RFC 9110 §5.6.2 `tchar`.
is_token_char(C) when C >= $a, C =< $z -> true;
is_token_char(C) when C >= $A, C =< $Z -> true;
is_token_char(C) when C >= $0, C =< $9 -> true;
is_token_char(C) -> lists:member(C, "!#$%&'*+-.^_`|~").

%% RFC 6265 §4.1.1 `cookie-octet`: printable ASCII minus SP, DQUOTE, `,`, `;`,
%% and `\`. An empty value is allowed.
valid_value(<<>>, _Orig) ->
    ok;
valid_value(<<C, R/binary>>, Orig) when
    C > 32, C < 127, C =/= $", C =/= $,, C =/= $;, C =/= $\\
->
    valid_value(R, Orig);
valid_value(<<_, _/binary>>, Orig) ->
    error({invalid_cookie_value, Orig}).

%% Header-injection safety only, not each attribute's full grammar: `Domain`
%% rejects CTLs, SP, and `;`; `Path` rejects CTLs and `;`; `Expires` rejects
%% CR, LF, NUL, and `;` (its value is a date with spaces in it).
valid_attr(_Attr, <<>>, _Orig) ->
    ok;
valid_attr(Attr, <<C, R/binary>>, Orig) ->
    case attr_char_ok(Attr, C) of
        true -> valid_attr(Attr, R, Orig);
        false -> error({invalid_cookie_attr, Attr, Orig})
    end.

attr_char_ok(domain, C) -> C > 32 andalso C < 127 andalso C =/= $;;
attr_char_ok(path, C) -> C > 31 andalso C =/= 127 andalso C =/= $;;
attr_char_ok(expires, C) -> C =/= $\r andalso C =/= $\n andalso C =/= 0 andalso C =/= $;.
