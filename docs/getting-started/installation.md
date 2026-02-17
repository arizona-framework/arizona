# Installation

- [Adding the Dependency](#adding-the-dependency)
- [Erlang/OTP Requirements](#erlangotp-requirements)
- [JavaScript Client Setup](#javascript-client-setup)

## Adding the Dependency

Arizona is published as a Hex package and is managed with
[rebar3](https://rebar3.org/), the standard Erlang build tool. To add Arizona
to your project, include it in the `deps` list of your `rebar.config` file:

```erlang
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}}
]}.
```

Then fetch the dependency by running:

```shell
rebar3 get-deps
```

Alternatively, if you want to compile your project (which also fetches
dependencies automatically), run:

```shell
rebar3 compile
```

If you are starting a brand new project, create the rebar3 project first:

```shell
rebar3 new app my_app
```

Then add the `arizona` dependency to the generated `rebar.config` as shown
above.

## Erlang/OTP Requirements

Arizona requires **Erlang/OTP 28 or later**. The framework relies on features
introduced in OTP 28, including nominal types and other language-level
improvements that Arizona uses internally for type safety and performance
optimizations.

You can verify your installed Erlang/OTP version by running:

```shell
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell
```

If you need to install or upgrade Erlang/OTP, popular options include:

- [kerl](https://github.com/kerl/kerl) -- a tool for building and managing
  multiple Erlang/OTP installations.
- [asdf](https://asdf-vm.com/) with the
  [asdf-erlang](https://github.com/asdf-vm/asdf-erlang) plugin.
- Your operating system's package manager (e.g., `apt`, `brew`, `pacman`),
  provided it offers OTP 28 or newer.

## JavaScript Client Setup

Arizona ships with a JavaScript client that manages the WebSocket connection,
DOM patching, and event delegation on the browser side. Install it from npm:

```shell
npm install @arizona-framework/client
```

The client package is required for live, interactive features. It handles the
WebSocket lifecycle, differential DOM updates, and event forwarding to the
server.

For full details on configuring the JavaScript client, including the Web Worker
architecture and build tool integration, see
[JavaScript Client Setup](../javascript-client/setup.md).
