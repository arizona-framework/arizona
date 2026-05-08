-module(arizona_handler_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0]).
-export([
    missing_callback_render_tagged/1,
    unhandled_event_tagged/1,
    unhandled_info_tagged/1,
    unhandled_unmount_tagged/1,
    function_clause_inside_callback_propagates_untagged/1,
    format_error_messages_include_view_id/1
]).

all() ->
    [{group, dispatcher_tagging}].

groups() ->
    [
        {dispatcher_tagging, [parallel], [
            missing_callback_render_tagged,
            unhandled_event_tagged,
            unhandled_info_tagged,
            unhandled_unmount_tagged,
            function_clause_inside_callback_propagates_untagged,
            format_error_messages_include_view_id
        ]}
    ].

%% --- Stub modules built per-case via dynamic compile ---------------------

compile_module(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Splits = split_forms(Tokens, [], []),
    Forms = [parse_form(S) || S <- Splits],
    Result = compile:forms(Forms, [binary]),
    {Mod, Bin} =
        case Result of
            {ok, M, B} -> {M, B};
            {ok, M, B, _Warnings} -> {M, B}
        end,
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    Mod.

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([], Cur, Acc) ->
    lists:reverse([lists:reverse(Cur) | Acc]);
split_forms([{dot, _} = Dot | Rest], Cur, Acc) ->
    split_forms(Rest, [], [lists:reverse([Dot | Cur]) | Acc]);
split_forms([T | Rest], Cur, Acc) ->
    split_forms(Rest, [T | Cur], Acc).

parse_form(Tokens) ->
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.

%% --- Tests ---------------------------------------------------------------

missing_callback_render_tagged(Config) when is_list(Config) ->
    %% Module without a render/1 export -- call_render must re-tag the
    %% resulting `undef` into `{missing_callback, _, render, 1}` with
    %% an error_info annotation.
    Mod = compile_module("-module(stub_no_render). -export([]).\n"),
    Stack =
        try arizona_handler:call_render(Mod, #{id => <<"v">>}) of
            _ -> ct:fail(expected_missing_callback)
        catch
            error:{missing_callback, Mod, render, 1}:ST -> ST
        end,
    [{arizona_handler, _, _, Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_handler},
        proplists:get_value(error_info, Info)
    ).

unhandled_event_tagged(Config) when is_list(Config) ->
    %% Module exports handle_event/3 but no clause matches the dispatched
    %% event -- call_handle_event must re-tag the function_clause into
    %% `{unhandled_event, _, _, _}`.
    Mod = compile_module(
        "-module(stub_event). "
        "-export([handle_event/3]). "
        "handle_event(<<\"known\">>, _P, B) -> {B, #{}, []}.\n"
    ),
    Stack =
        try
            arizona_handler:call_handle_event(
                Mod, <<"unknown">>, #{}, #{id => <<"v">>}
            )
        of
            _ -> ct:fail(expected_unhandled_event)
        catch
            error:{unhandled_event, Mod, <<"unknown">>, _}:ST -> ST
        end,
    [{arizona_handler, _, _, Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_handler},
        proplists:get_value(error_info, Info)
    ).

unhandled_info_tagged(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(stub_info). "
        "-export([handle_info/2]). "
        "handle_info(known, B) -> {B, #{}, []}.\n"
    ),
    try arizona_handler:call_handle_info(Mod, unknown, #{id => <<"v">>}) of
        _ -> ct:fail(expected_unhandled_info)
    catch
        error:{unhandled_info, Mod, unknown, _} -> ok
    end.

unhandled_unmount_tagged(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(stub_unmount). "
        "-export([unmount/1]). "
        "unmount(#{tag := keep}) -> ok.\n"
    ),
    try arizona_handler:call_unmount(Mod, #{id => <<"v">>}) of
        _ -> ct:fail(expected_unhandled_unmount)
    catch
        error:{unhandled_unmount, Mod, _} -> ok
    end.

function_clause_inside_callback_propagates_untagged(Config) when is_list(Config) ->
    %% function_clause raised from INSIDE the callback's body (a case with
    %% no matching pattern) must propagate as raw function_clause -- only
    %% failures at the callback's own head get re-tagged.
    Mod = compile_module(
        "-module(stub_inner_fc). "
        "-export([handle_event/3]). "
        "handle_event(_E, _P, B) -> "
        "    case B of #{required := V} -> V end.\n"
    ),
    try
        arizona_handler:call_handle_event(
            Mod, <<"any">>, #{}, #{id => <<"v">>}
        )
    of
        _ -> ct:fail(expected_function_clause)
    catch
        %% Raw function_clause -- not re-tagged, because the inner case
        %% (not the handle_event head) is what failed.
        error:{case_clause, _} -> ok;
        error:function_clause -> ok
    end.

format_error_messages_include_view_id(Config) when is_list(Config) ->
    %% format_error/2 must include the view id pulled from Bindings.
    Reason = {unhandled_event, my_handler, <<"foo">>, #{id => <<"page">>}},
    #{general := Msg} = arizona_handler:format_error(Reason, []),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"page">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"my_handler">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"foo">>)).
