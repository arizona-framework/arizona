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

For known reason shapes, the renderer produces a richer summary:

- `{compile_error, Errors}` -- formatted as a per-file error list
- `{arizona_loc, {Mod, Line}, Reason}` -- prefixes the title with the
  source location captured by `arizona_render` during dynamic eval

For everything else it falls back to `erl_error:format_exception/3`,
extracts the shell-style summary, and pretty-prints the raw reason.
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
    Title = escape(format_title(Class, Reason, Stacktrace)),
    ReasonStr = format_reason(Reason),
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
                            Loc = format_loc(Info),
                            {'div', [{class, ~"frame"}], [
                                {span, [{class, ~"mod"}], [escape(atom_to_binary(M))]},
                                ~":",
                                {span, [{class, ~"fun"}], [
                                    <<
                                        (escape(atom_to_binary(F)))/binary,
                                        "/",
                                        (integer_to_binary(arity(A)))/binary
                                    >>
                                ]},
                                {span, [{class, ~"loc"}], [~" ", Loc]}
                            ]}
                        end,
                        Stacktrace
                    )
                ]},
                reload_script(ReloadUrl)
            ]}
        ]}
    ).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

format_title(_Class, {compile_error, _Errors}, _Stacktrace) ->
    ~"Compilation Error";
format_title(Class, {arizona_loc, {Mod, Line}, Reason}, Stacktrace) ->
    Prefix = unicode:characters_to_binary(io_lib:format("~s:~b: ", [Mod, Line])),
    unicode:characters_to_binary([Prefix, error_summary(Class, Reason, Stacktrace)]);
format_title(Class, Reason, Stacktrace) ->
    unicode:characters_to_binary(
        io_lib:format("~s: ~ts", [Class, error_summary(Class, Reason, Stacktrace)])
    ).

%% Use erl_error:format_exception to get the rich, shell-style summary.
%% Output looks like:
%%   "exception error: no function clause matching \n
%%                     arizona_template:to_bin({}) (src/...erl:68)\n
%%     in function  arizona_render:zip/2 ..."
%% We extract everything after "exception <class>: " up to "\n  in ",
%% collapsing continuation newlines into spaces.
error_summary(Class, Reason, Stacktrace) ->
    try
        Flat = lists:flatten(erl_error:format_exception(Class, Reason, Stacktrace)),
        extract_summary(Flat)
    catch
        _:_ ->
            io_lib:format("~w:~0tp", [Class, Reason])
    end.

extract_summary("exception " ++ Rest) ->
    skip_class(Rest);
extract_summary(Other) ->
    Other.

skip_class([$:, $\s | Rest]) ->
    take_summary(Rest, []);
skip_class([_ | Rest]) ->
    skip_class(Rest);
skip_class([]) ->
    [].

%% "\n  in " marks start of stack frames -- stop here.
take_summary([$\n | Rest], Acc) ->
    Trimmed = skip_spaces(Rest),
    case Trimmed of
        [$i, $n, $\s | _] -> lists:reverse(Acc);
        _ -> take_summary(Trimmed, [$\s | Acc])
    end;
take_summary([C | Rest], Acc) ->
    take_summary(Rest, [C | Acc]);
take_summary([], Acc) ->
    lists:reverse(Acc).

skip_spaces([$\s | Rest]) -> skip_spaces(Rest);
skip_spaces(Other) -> Other.

format_reason({compile_error, Errors}) ->
    escape(
        unicode:characters_to_binary(
            lists:join("\n\n", [format_file_errors(F, Es) || {F, Es} <:- Errors])
        )
    );
format_reason({arizona_loc, _Loc, Reason}) ->
    format_reason(Reason);
format_reason(Reason) ->
    escape(unicode:characters_to_binary(io_lib:format("~0tp", [Reason]))).

format_file_errors(File, Errors) ->
    [File, ":\n", lists:join("\n", [format_one_error(E) || E <- Errors])].

format_one_error({Location, Mod, Reason}) ->
    Line = erl_anno:line(Location),
    Msg = Mod:format_error(Reason),
    io_lib:format("  line ~b: ~ts", [Line, Msg]).

format_loc(Info) ->
    File = proplists:get_value(file, Info, ""),
    Line = proplists:get_value(line, Info, 0),
    case File of
        "" -> <<>>;
        _ -> unicode:characters_to_binary([File, ":", integer_to_list(Line)])
    end.

escape(Bin) when is_binary(Bin) ->
    escape(Bin, <<>>).

escape(<<>>, Acc) ->
    Acc;
escape(<<"&", Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, "&amp;">>);
escape(<<"<", Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, "&lt;">>);
escape(<<">", Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, "&gt;">>);
escape(<<"\"", Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, "&quot;">>);
escape(<<C, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, C>>).

reload_script(undefined) ->
    <<>>;
reload_script(Url) ->
    <<"<script>new EventSource('", Url/binary,
        "').addEventListener('reload',function(){location.reload()})</script>">>.

arity(A) when is_list(A) -> length(A);
arity(N) when is_integer(N) -> N.

-ifdef(TEST).

format_title_compile_error_test() ->
    ?assertEqual(~"Compilation Error", format_title(error, {compile_error, []}, [])).

format_title_arizona_loc_test() ->
    Title = format_title(error, {arizona_loc, {my_mod, 42}, badarg}, []),
    ?assertNotEqual(nomatch, binary:match(Title, <<"my_mod:42: ">>)).

format_reason_compile_error_test() ->
    Errors = [{"src/foo.erl", [{{10, 1}, erl_lint, {unused_var, 'X'}}]}],
    Bin = format_reason({compile_error, Errors}),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"src/foo.erl">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"line 10">>)).

format_reason_arizona_loc_test() ->
    %% arizona_loc wraps another reason -- the wrapper is stripped for body.
    Bin = format_reason({arizona_loc, {m, 1}, badarg}),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"badarg">>)).

render_with_reload_url_test() ->
    ErrorInfo = #{
        class => error,
        reason => badarg,
        stacktrace => [],
        reload_url => <<"/reload">>
    },
    Tmpl = render(#{error_info => ErrorInfo}),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"EventSource">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"/reload">>)).

-endif.
