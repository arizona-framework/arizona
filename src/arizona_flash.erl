-module(arizona_flash).
-moduledoc """
Signed, one-time flash cookie codec.

A flash is a small map of short-lived display messages carried across a single
redirect (the Post/Redirect/Get pattern). It rides a dedicated cookie that is
signed (HMAC-SHA256) with the `arizona` `secret_key` application env, so a client
cannot forge one, and is consumed on the next request that reads it.

This module owns the wire format: encode/decode the payload and build the
`Set-Cookie` tuples. `arizona_req` integrates it into the request/response stash
(`put_flash/3`, `flash/1`, `read_flash/1`), and the `arizona_middleware:fetch_flash/2`
step reads it into the `flash` binding.

The cookie value is `payload "." signature`, both URL-safe base64 without padding;
`payload` is the JSON of the flash map and `signature` is its HMAC.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([cookie_name/0]).
-export([encode/1]).
-export([decode/1]).
-export([set_cookie/1]).
-export([clear_cookie/0]).
-export([resp_cookie/2]).
-export([key/1]).
-export([secret/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Public codec, exercised by arizona_req; no other internal callers by design.
-ignore_xref([cookie_name/0]).
-ignore_xref([encode/1]).
-ignore_xref([decode/1]).
-ignore_xref([set_cookie/1]).
-ignore_xref([clear_cookie/0]).
-ignore_xref([resp_cookie/2]).
-ignore_xref([key/1]).
-ignore_xref([secret/0]).

%% Flash cookie name and lifetime. The flash is consumed on the very next
%% request, so the max-age is only a safety net if that request never arrives.
-define(COOKIE, ~"az_flash").
-define(MAX_AGE, 60).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "The flash cookie name.".
-spec cookie_name() -> binary().
cookie_name() ->
    ?COOKIE.

-doc """
Encodes a flash map into a signed cookie value. Errors if `secret_key` is unset.
""".
-spec encode(Flash) -> binary() when
    Flash :: arizona_req:flash().
encode(Flash) ->
    Payload = iolist_to_binary(json:encode(Flash)),
    Sig = crypto:mac(hmac, sha256, secret(), Payload),
    <<(b64(Payload))/binary, ".", (b64(Sig))/binary>>.

-doc """
Decodes and verifies a cookie value into a flash map, returning `#{}` when the
signature does not match or the value is malformed.
""".
-spec decode(Value) -> arizona_req:flash() when
    Value :: binary().
decode(Value) ->
    try
        [B64Payload, B64Sig] = binary:split(Value, ~"."),
        Payload = unb64(B64Payload),
        Expected = crypto:mac(hmac, sha256, secret(), Payload),
        true = crypto:hash_equals(unb64(B64Sig), Expected),
        case json:decode(Payload) of
            Flash when is_map(Flash) -> Flash;
            _ -> #{}
        end
    catch
        _:_ -> #{}
    end.

-doc "The `Set-Cookie` tuple that carries `Flash` to the next request.".
-spec set_cookie(Flash) -> {binary(), binary(), arizona_req:resp_cookie_opts()} when
    Flash :: arizona_req:flash().
set_cookie(Flash) ->
    {?COOKIE, encode(Flash), cookie_opts(?MAX_AGE)}.

-doc "The `Set-Cookie` tuple that clears a consumed flash.".
-spec clear_cookie() -> {binary(), binary(), arizona_req:resp_cookie_opts()}.
clear_cookie() ->
    {?COOKIE, <<>>, cookie_opts(0)}.

-doc """
The flash `Set-Cookie` a response should carry, given the outgoing flash and
whether an incoming one was consumed: a freshly set flash (signed), a clearing
cookie when one was consumed and none was set, or `none`.
""".
-spec resp_cookie(FlashOut, Consumed) ->
    {binary(), binary(), arizona_req:resp_cookie_opts()} | none
when
    FlashOut :: arizona_req:flash(),
    Consumed :: boolean().
resp_cookie(FlashOut, _Consumed) when map_size(FlashOut) > 0 ->
    set_cookie(FlashOut);
resp_cookie(_FlashOut, true) ->
    clear_cookie();
resp_cookie(_FlashOut, false) ->
    none.

-doc "Normalizes a flash key (an atom is converted to a binary).".
-spec key(atom() | binary()) -> binary().
key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
key(Key) when is_binary(Key) -> Key.

-doc """
Returns the configured flash signing secret from the `arizona` `secret_key`
application env, erroring if it is unset or empty.
""".
-spec secret() -> binary().
secret() ->
    case application:get_env(arizona, secret_key) of
        {ok, Secret} when is_binary(Secret), Secret =/= <<>> ->
            Secret;
        _ ->
            error(
                {arizona_flash, secret_key_not_configured},
                none,
                [
                    {error_info, #{
                        cause =>
                            <<
                                "flash requires a signing key: set the arizona "
                                "`secret_key` application env to a random binary"
                            >>
                    }}
                ]
            )
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

cookie_opts(MaxAge) ->
    #{
        http_only => true,
        same_site => lax,
        path => ~"/",
        max_age => MaxAge,
        secure => application:get_env(arizona, flash_secure, false)
    }.

b64(Bin) ->
    base64:encode(Bin, #{mode => urlsafe, padding => false}).

unb64(Bin) ->
    base64:decode(Bin, #{mode => urlsafe, padding => false}).
