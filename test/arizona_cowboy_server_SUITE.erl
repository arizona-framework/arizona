-module(arizona_cowboy_server_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([tls_is_refused_rather_than_served_in_plaintext/1]).

all() ->
    [tls_is_refused_rather_than_served_in_plaintext].

%% TLS is not supported yet: configuring it must fail the start, not quietly
%% serve the configured certs' traffic in cleartext.
tls_is_refused_rather_than_served_in_plaintext(Config) when is_list(Config) ->
    Routes = [{live, ~"/", arizona_crashable, #{}}],
    ?assertError(
        {unsupported_server_opt, scheme, https},
        arizona_cowboy_server:start(cowboy_tls_test, #{routes => Routes, scheme => https})
    ),
    ?assertError(
        {unsupported_server_opt, tls, [{certfile, "cert.pem"}]},
        arizona_cowboy_server:start(cowboy_tls_test, #{
            routes => Routes, tls => [{certfile, "cert.pem"}]
        })
    ).
