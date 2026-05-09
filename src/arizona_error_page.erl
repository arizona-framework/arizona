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
    ReasonStr = format_reason(Class, Reason, Stacktrace),
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
                                {span, [{class, ~"fun"}], [escape(format_fun_name(F, A))]},
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

%% Title is just a short location -- the body shows the full reason.
%%
%% Strategy:
%%
%% 1. `compile_error` -- always "Compilation Error".
%% 2. Otherwise, walk the stacktrace for the deepest user frame --
%%    skipping `arizona_*` (framework) and any OTP/stdlib module.
%%    That's the user's nearest call site, the line they actually want
%%    to look at.
%% 3. If the whole stack is framework/OTP, fall back to the top frame.
%% 4. If the stack is empty, fall back to `arizona_loc`'s captured
%%    location (when the reason is wrapped) or the bare class.
%%
%% Note: `arizona_loc` captures the *lexical* location of a template
%% dynamic, which for layouts is the outer dynamic's call site, not
%% the inner crash site. The stack tells the truth, so prefer it.
format_title(_Class, {compile_error, _Errors}, _Stacktrace) ->
    ~"Compilation Error";
format_title(Class, Reason, Stacktrace) ->
    case first_user_frame(Stacktrace) of
        none ->
            fallback_title(Class, Reason);
        {Mod, _Fn, _A, Info} ->
            case proplists:get_value(line, Info) of
                undefined -> fallback_title(Class, Reason);
                Line -> fmt_mod_line(Mod, Line)
            end
    end.

fallback_title(_Class, {arizona_loc, {Mod, Line}, _}) ->
    fmt_mod_line(Mod, Line);
fallback_title(Class, _Reason) ->
    unicode:characters_to_binary(io_lib:format("~s", [Class])).

fmt_mod_line(Mod, Line) ->
    unicode:characters_to_binary(io_lib:format("~s:~b", [Mod, Line])).

%% Returns the first stack frame whose module is not framework or OTP,
%% the topmost frame when the whole stack is framework/OTP, or `none`
%% when the stack is empty.
first_user_frame([]) ->
    none;
first_user_frame([Top | _] = Stack) ->
    case lists:dropwhile(fun is_framework_frame/1, Stack) of
        [] -> Top;
        [User | _] -> User
    end.

is_framework_frame({Mod, _Fn, _A, _Info}) ->
    is_arizona_module(Mod) orelse is_otp_module(Mod).

is_arizona_module(Mod) ->
    case atom_to_list(Mod) of
        "arizona_" ++ _ -> true;
        _ -> false
    end.

%% True for OTP/stdlib modules. Detected via `code:which/1`:
%% built-ins are `preloaded`, stdlib lives under `code:root_dir()`.
%% User modules and dependencies (e.g. arizona itself when checked out)
%% live elsewhere and return false here.
is_otp_module(Mod) ->
    case code:which(Mod) of
        preloaded ->
            true;
        Path when is_list(Path) ->
            string:prefix(Path, code:root_dir() ++ "/") =/= nomatch;
        _ ->
            false
    end.

%% Rewrites OTP-mangled fun names for readability.
%%
%%   '-render/1-fun-1-'/N         -> "render/1 (anonymous fun)"
%%   '-render_ssr_val/1-lc$^0/1-0-'/N -> "render_ssr_val/1 (list comprehension)"
%%   '-render/1-mc$^0/1-0-'/N         -> "render/1 (map comprehension)"
%%   <plain Fn>/N                  -> "Fn/N"
format_fun_name(Fn, A) ->
    Arity = arity(A),
    Name = atom_to_list(Fn),
    case prettify_mangled(Name) of
        {Outer, Suffix} ->
            unicode:characters_to_binary([Outer, " (", Suffix, ")"]);
        plain ->
            unicode:characters_to_binary([Name, "/", integer_to_list(Arity)])
    end.

prettify_mangled([$- | Rest]) ->
    prettify_mangled_body(Rest, [
        {"-fun-", "anonymous fun"},
        {"-lc$", "list comprehension"},
        {"-mc$", "map comprehension"}
    ]);
prettify_mangled(_) ->
    plain.

prettify_mangled_body(_Rest, []) ->
    plain;
prettify_mangled_body(Rest, [{Sep, Suffix} | More]) ->
    case string:split(Rest, Sep) of
        [Outer, _Tail] when Outer =/= Rest -> {Outer, Suffix};
        _ -> prettify_mangled_body(Rest, More)
    end.

%% Use erl_error:format_exception to get the rich, shell-style summary.
%% Output for an error_info-annotated raise looks like:
%%   "exception error: missing_binding\n
%%      in function  arizona_template:get/2\n
%%         called as arizona_template:get(missing_key, #{a => 1})\n
%%         *** binding missing_key not found. Available bindings: ...\n
%%      in call from ..."
%% The format_error/2 sentence lands on the `*** ` line, nested under
%% the failing function's frame -- prefer it when present. For errors
%% without error_info (plain badmatch, etc.) the top "exception <class>: "
%% line already carries the useful message, so we fall back to that.
error_summary(Class, Reason, Stacktrace) ->
    try
        Flat = lists:flatten(erl_error:format_exception(Class, Reason, Stacktrace)),
        extract_summary(Flat)
    catch
        _:_ ->
            io_lib:format("~w:~0tp", [Class, Reason])
    end.

%% Prefer the format_error/2 annotation line when erl_error attached one.
%% Otherwise take the top "exception <class>: ..." line up to the first
%% stack frame.
extract_summary(Flat) ->
    case extract_format_error_line(Flat) of
        nomatch -> extract_top_line(Flat);
        Line -> Line
    end.

%% erl_error indents the format_error/2 annotation with 5 spaces under
%% "called as ...". Find that marker, skip it, take the rest of the line.
extract_format_error_line(Flat) ->
    Marker = "\n     *** ",
    case string:find(Flat, Marker) of
        nomatch ->
            nomatch;
        Found ->
            AfterMarker = string:slice(Found, length(Marker)),
            take_until_newline(AfterMarker, [])
    end.

take_until_newline([$\n | _], Acc) -> lists:reverse(Acc);
take_until_newline([C | Rest], Acc) -> take_until_newline(Rest, [C | Acc]);
take_until_newline([], Acc) -> lists:reverse(Acc).

extract_top_line("exception " ++ Rest) ->
    skip_class(Rest);
extract_top_line(Other) ->
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

format_reason(_Class, {compile_error, Errors}, _Stacktrace) ->
    escape(
        unicode:characters_to_binary(
            lists:join("\n\n", [format_file_errors(F, Es) || {F, Es} <:- Errors])
        )
    );
format_reason(Class, {arizona_loc, _Loc, Reason}, Stacktrace) ->
    format_reason(Class, Reason, Stacktrace);
format_reason(Class, Reason, Stacktrace) ->
    %% Body shows the full summary -- title is just the location, so
    %% there's no repetition. Falls back to `~0tp` for plain reasons
    %% without any erl_error-format output.
    case error_summary(Class, Reason, Stacktrace) of
        [] -> escape(unicode:characters_to_binary(io_lib:format("~0tp", [Reason])));
        Summary -> escape(unicode:characters_to_binary(Summary))
    end.

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
        _ -> unicode:characters_to_binary([relative_path(File), ":", integer_to_list(Line)])
    end.

%% Trims the cwd prefix from absolute paths so frames render as e.g.
%% `src/arizona_template.erl:153` instead of
%% `/home/.../arizona_template.erl:153`. Falls through unchanged when
%% cwd lookup fails or the path is already relative / outside cwd.
relative_path(File) ->
    case file:get_cwd() of
        {ok, Cwd} ->
            case string:prefix(File, Cwd ++ "/") of
                nomatch -> File;
                Rel -> Rel
            end;
        _ ->
            File
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
    %% Empty stack -> fallback uses arizona_loc's captured location.
    Title = format_title(error, {arizona_loc, {my_mod, 42}, badarg}, []),
    ?assertEqual(<<"my_mod:42">>, Title).

format_reason_compile_error_test() ->
    Errors = [{"src/foo.erl", [{{10, 1}, erl_lint, {unused_var, 'X'}}]}],
    Bin = format_reason(error, {compile_error, Errors}, []),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"src/foo.erl">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"line 10">>)).

format_reason_arizona_loc_test() ->
    %% arizona_loc wraps another reason -- the wrapper is stripped for body
    %% and erl_error renders the inner reason via its standard pipeline.
    Bin = format_reason(error, {arizona_loc, {m, 1}, badarg}, []),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"bad argument">>)).

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
