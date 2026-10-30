-module(arizona_error_page).
-moduledoc """
Stateless template that renders the dev-mode error page.

When a request crashes, `arizona_http` builds an `error_info` map
and calls this template's `render/1` to produce a self-contained
HTML page with the exception class, reason, and stack frames styled
for monospace display.

If a `reload_url` is present in the bindings, the page also embeds a
small `EventSource` script so the browser auto-reloads as soon as
the dev watcher reports a successful recompile.

## Bindings

- `error_info` -- map with `class`, `reason`, `stacktrace`, and
  optional `reload_url`

## Reason formatting

The title, the reason summary and each stack frame are formatted by
`arizona_error` (`format_title/3`, `format_reason/3`, `format_fun_name/2`,
`format_loc/1`), the same helpers the plain-text dev 500 uses.

This page is opt-in: a listener serves it when its config sets
`error_page => {arizona_error_page, render}`.
""".
-include("arizona_stateless.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([render/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Renders the error page from `error_info` bindings.
""".
-spec render(Bindings) -> arizona_template:template() when
    Bindings :: #{error_info := az:error_info(), _ => _}.
render(Bindings) ->
    ErrorInfo = maps:get(error_info, Bindings),
    Class = maps:get(class, ErrorInfo),
    Reason = maps:get(reason, ErrorInfo),
    Stacktrace = maps:get(stacktrace, ErrorInfo),
    ReloadUrl = maps:get(reload_url, ErrorInfo, undefined),
    Title = arizona_error:format_title(Class, Reason, Stacktrace),
    ReasonStr = arizona_error:format_reason(Class, Reason, Stacktrace),
    ?html(
        {html, [], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}]},
                {title, [], [~"500 -- Server Error"]},
                {style, [],
                    ~"""
                    *{margin:0;padding:0;box-sizing:border-box}
                    body{font-family:monospace;background:#1e1e2e;color:#cdd6f4;padding:2rem}
                    h1{color:#f38ba8;font-size:1.4rem;margin-bottom:1rem}
                    .reason{background:#313244;padding:1rem;border-radius:6px;
                    margin-bottom:1.5rem;overflow-x:auto;white-space:pre-wrap;word-break:break-all}
                    .frame{padding:.5rem 1rem;border-left:2px solid #45475a}
                    .frame+.frame{margin-top:.25rem}
                    .mod{color:#89b4fa}.fun{color:#a6e3a1}.loc{color:#9399b2}
                    """}
            ]},
            {body, [], [
                {h1, [], [Title]},
                {pre, [{class, ~"reason"}], [ReasonStr]},
                {'div', [{id, ~"stacktrace"}], [
                    ?each(
                        fun({M, F, A, Info}) ->
                            Loc = arizona_error:format_loc(Info),
                            {'div', [{class, ~"frame"}], [
                                {span, [{class, ~"mod"}], [atom_to_binary(M)]},
                                ~":",
                                {span, [{class, ~"fun"}], [arizona_error:format_fun_name(F, A)]},
                                {span, [{class, ~"loc"}], [~" ", Loc]}
                            ]}
                        end,
                        Stacktrace
                    )
                ]},
                ?raw(reload_script(ReloadUrl))
            ]}
        ]}
    ).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Trusted, framework-built markup -- the caller wraps it in `?raw` so the
%% content slot splices it verbatim and the browser sees a real <script> tag
%% rather than escaped text (`?raw` must be the literal call at the template
%% site for the parse transform to recognize the escape opt-out).
reload_script(undefined) ->
    <<>>;
reload_script(Url) ->
    <<"<script>new EventSource('", Url/binary,
        "').addEventListener('reload',function(){location.reload()})</script>">>.

-ifdef(TEST).

render_with_reload_url_test() ->
    ErrorInfo = #{
        class => error,
        reason => badarg,
        stacktrace => [],
        reload_url => <<"/reload">>
    },
    Tmpl = render(#{error_info => ErrorInfo}),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    %% Must be a real <script> tag, not escaped text -- assert the literal
    %% opening tag is present and that no escaped form (&lt;script&gt;) leaked.
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<script>new EventSource('/reload')">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"&lt;script&gt;">>)).

-endif.
