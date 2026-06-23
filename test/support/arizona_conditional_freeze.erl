-module(arizona_conditional_freeze).
-moduledoc """
Stateless templates exercising the content-slot conditional dependency fix.

A `case`/`if`/`maybe` written directly in a content slot compiles each branch
element into a nested template whose `?get` reads are isolated from the
conditional dynamic's own dependency bracket. The parse transform unions those
branch-tail reads into the slot's deps (via injected `arizona_template:track/1`),
so a change to a binding read only in a branch re-renders the slot instead of
freezing it. Each function is a distinct scenario driven by `arizona_diff_SUITE`.
""".
-include("arizona_stateless.hrl").
-export([case_branch/1]).
-export([if_branch/1]).
-export([maybe_branch/1]).
-export([nested/1]).
-export([over_track/1]).
-export([optional_key/1]).
-export([case_var/1]).
-export([if_var/1]).
-export([maybe_var/1]).
-export([nested_var/1]).
-export([over_track_var/1]).
-export([optional_key_var/1]).
-export([deep/1]).
-export([attr_in_branch/1]).
-export([try_branch/1]).
-export([block_branch/1]).
-export([receive_branch/1]).
-export([two_slot/1]).
-export([nested_element/1]).
-export([top_text/1]).
-export([raw_text/1]).

%% Constant scrutinee (`flag`), branch reads `val`: a change to `val` must
%% re-render the `<p>` even though `flag` is unchanged.
-spec case_branch(az:bindings()) -> az:template().
case_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [?get(val)]};
                false -> <<>>
            end
        ]}
    ).

%% `if` analogue: the guard reads `flag` (auto-tracked), the branch reads `val`.
-spec if_branch(az:bindings()) -> az:template().
if_branch(Bindings) ->
    Flag = ?get(flag),
    ?html(
        {'div', [{id, ~"x"}], [
            if
                Flag -> {p, [], [?get(val)]};
                true -> <<>>
            end
        ]}
    ).

%% `maybe` analogue: the match reads `flag`, the success tail reads `val`.
-spec maybe_branch(az:bindings()) -> az:template().
maybe_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            maybe
                true ?= ?get(flag),
                {p, [], [?get(val)]}
            else
                _ -> <<>>
            end
        ]}
    ).

%% Nested conditional: the inner branch reads `val`, both scrutinees constant.
-spec nested(az:bindings()) -> az:template().
nested(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(outer) of
                true ->
                    case ?get(inner) of
                        true -> {p, [], [?get(val)]};
                        false -> <<>>
                    end;
                false ->
                    <<>>
            end
        ]}
    ).

%% Two branches read different keys. Changing the key read only in the
%% non-taken branch re-evaluates the slot but emits no op (snapshot equality).
-spec over_track(az:bindings()) -> az:template().
over_track(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [?get(a)]};
                false -> {p, [], [?get(b)]}
            end
        ]}
    ).

%% A branch reads a binding that is absent when the other branch is taken.
%% Rendering the taken (`_`) branch must not raise `missing_binding` -- proves
%% the injected touch uses `track/1` (records the key, no read), not `get/2`.
-spec optional_key(az:bindings()) -> az:template().
optional_key(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(mode) of
                admin -> {p, [], [?get(secret)]};
                _ -> <<>>
            end
        ]}
    ).

%% Hoisted-variable counterparts. The conditional is bound to `Content` and then
%% interpolated; binding-read inlining (collect_inline/inline_vars) rewrites
%% `Content` back into the slot, so these reach the same content-slot path as the
%% inline forms above and must produce identical diffs.
-spec case_var(az:bindings()) -> az:template().
case_var(Bindings) ->
    Content =
        case ?get(flag) of
            true -> {p, [], [?get(val)]};
            false -> <<>>
        end,
    ?html({'div', [{id, ~"x"}], [Content]}).

-spec if_var(az:bindings()) -> az:template().
if_var(Bindings) ->
    Flag = ?get(flag),
    Content =
        if
            Flag -> {p, [], [?get(val)]};
            true -> <<>>
        end,
    ?html({'div', [{id, ~"x"}], [Content]}).

-spec maybe_var(az:bindings()) -> az:template().
maybe_var(Bindings) ->
    Content =
        maybe
            true ?= ?get(flag),
            {p, [], [?get(val)]}
        else
            _ -> <<>>
        end,
    ?html({'div', [{id, ~"x"}], [Content]}).

-spec nested_var(az:bindings()) -> az:template().
nested_var(Bindings) ->
    Content =
        case ?get(outer) of
            true ->
                case ?get(inner) of
                    true -> {p, [], [?get(val)]};
                    false -> <<>>
                end;
            false ->
                <<>>
        end,
    ?html({'div', [{id, ~"x"}], [Content]}).

-spec over_track_var(az:bindings()) -> az:template().
over_track_var(Bindings) ->
    Content =
        case ?get(flag) of
            true -> {p, [], [?get(a)]};
            false -> {p, [], [?get(b)]}
        end,
    ?html({'div', [{id, ~"x"}], [Content]}).

-spec optional_key_var(az:bindings()) -> az:template().
optional_key_var(Bindings) ->
    Content =
        case ?get(mode) of
            admin -> {p, [], [?get(secret)]};
            _ -> <<>>
        end,
    ?html({'div', [{id, ~"x"}], [Content]}).

%% Edge cases. Each reads `val` (or `cls`) in a branch position that is only reached
%% by walking deep into the tail/element structure; if the read were not tracked the
%% diff would be `[]`, so each asserts an op.

%% Three levels of nesting: the read sits under three constant scrutinees, exercising
%% collect_branch_keys/2's recursion through nested control flow.
-spec deep(az:bindings()) -> az:template().
deep(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(a) of
                true ->
                    case ?get(b) of
                        true ->
                            case ?get(c) of
                                true -> {p, [], [?get(val)]};
                                false -> <<>>
                            end;
                        false ->
                            <<>>
                    end;
                false ->
                    <<>>
            end
        ]}
    ).

%% The branch element reads in an *attribute*, not content: collect_read_keys/2 must
%% walk the whole element subtree (attrs included), not just its children.
-spec attr_in_branch(az:bindings()) -> az:template().
attr_in_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [{class, ?get(cls)}], [~"hi"]};
                false -> <<>>
            end
        ]}
    ).

%% A `try` body is a walked tail position (map_tail_exprs/3); the success body reads
%% `val` in an element.
-spec try_branch(az:bindings()) -> az:template().
try_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            try ?get(flag) of
                true -> {p, [], [?get(val)]};
                false -> <<>>
            catch
                _:_ -> <<>>
            end
        ]}
    ).

%% A `begin ... end` block is a walked tail position (map_tail_exprs/3): the block's
%% last expression is the tail element, whose `val` read must be tracked.
-spec block_branch(az:bindings()) -> az:template().
block_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            begin
                Class = ?get(cls),
                {p, [{class, Class}], [?get(val)]}
            end
        ]}
    ).

%% A `receive ... after` body is a walked tail position: the `after 0` element branch's
%% `val` read must be tracked. `after 0` fires immediately on the empty mailbox, so the
%% render never blocks.
-spec receive_branch(az:bindings()) -> az:template().
receive_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            receive
            after 0 -> {p, [], [?get(val)]}
            end
        ]}
    ).

%% The branch element holds two text slots. Changing one must patch only that slot
%% (fine-grained inner diff), leaving the sibling slot untouched.
-spec two_slot(az:bindings()) -> az:template().
two_slot(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [?get(a), ~" ", ?get(b)]};
                false -> <<>>
            end
        ]}
    ).

%% A genuinely nested-nested template: the outer branch is a `<section>` element that
%% itself contains a conditional whose branch is a `<p>` element. Fine-graining must
%% recurse (make_ops -> diff_dynamics -> make_ops) to patch only the deepest slot.
-spec nested_element(az:bindings()) -> az:template().
nested_element(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true ->
                    {section, [], [
                        case ?get(inner) of
                            true -> {p, [], [?get(val)]};
                            false -> <<>>
                        end
                    ]};
                false ->
                    <<>>
            end
        ]}
    ).

%% Top-level (non-nested) text dynamic: a `?get` value is sent RAW on the diff wire
%% (the client text-nodes it -- safe, matches SSR).
-spec top_text(az:bindings()) -> az:template().
top_text(Bindings) ->
    ?html({'div', [{id, ~"x"}], [?get(name)]}).

%% A `?raw` trusted-HTML value: tagged on the diff wire so the client innerHTMLs it.
-spec raw_text(az:bindings()) -> az:template().
raw_text(Bindings) ->
    ?html({'div', [{id, ~"x"}], [?raw(?get(html))]}).
