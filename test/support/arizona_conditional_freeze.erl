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
-export([get_lazy_branch/1]).
-export([with_branch/1]).
-export([with_macro_branch/1]).
-export([computed_key/1]).
-export([two_slot/1]).
-export([nested_element/1]).
-export([top_text/1]).
-export([raw_text/1]).
-export([helper_raw/1]).
-export([runtime_local_content/1]).
-export([helper_raw_template/1]).
-export([helper_raw_template_nodiff/1]).
-export([helper_raw_template_json/1]).
-export([literal_raw_template/1]).
-export([raw_wrapped/1]).
-export([title_raw_value/1]).
-export([textarea_raw_value/1]).
-export([title_raw_helper_raw/1]).
-export([title_helper_raw_value/1]).
-export([title_text_template_prop/1]).
-export([textarea_text_template_prop/1]).
-export([title_static_text_template/1]).
-export([script_raw_bad_list/1]).
-export([script_literal_raw_template/1]).
-export([script_helper_raw_template/1]).
-export([style_literal_raw_template/1]).
-export([style_helper_raw_template/1]).
-export([textarea_literal_raw_template/1]).
-export([textarea_helper_raw_template/1]).
-export([title_literal_raw_template/1]).
-export([title_helper_raw_template/1]).
-export([each_bare/1]).
-export([each_raw/1]).
-export([stream_bare/1]).
-export([stream_raw/1]).
-export([nodiff_pick/1]).
-export([literal_raw_attr/1]).
-export([helper_raw_attr/1]).
-export([interp_raw_attr/1]).
-export([helper_raw_attr_json/1]).
-export([literal_raw_attr_json/1]).
-export([raw_raw_effect/1]).
-export([raw_raw_effect_attr/1]).
-export([raw_in_list_item/1]).
-export([raw_in_stream_item/1]).
-export([raw_in_branch/1]).
-export([raw_attr/1]).
-export([raw_interp_attr/1]).
-export([attr_name_swap/1]).
-export([event_attr_name_swap/1]).
-export([json_attr_name_swap/1]).

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

%% A branch element reading via `?get_lazy` instead of `?get`: collect_call_keys must
%% extract the key from a `get_lazy` call too, so a change to `val` re-renders the slot.
-spec get_lazy_branch(az:bindings()) -> az:template().
get_lazy_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [?get_lazy(val, fun() -> ~"def" end)]};
                false -> <<>>
            end
        ]}
    ).

%% A branch element handing a bindings subset to a sub-context via `az:with`:
%% collect_call_keys must extract each key from the `with` list, so a change to `val`
%% re-renders the slot. Without the injected track the projection's read is isolated in
%% the branch's nested template and the slot would freeze.
-spec with_branch(az:bindings()) -> az:template().
with_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [maps:get(val, az:with([val], Bindings))]};
                false -> <<>>
            end
        ]}
    ).

%% The same projection through the `?with` macro, which expands to
%% `arizona_template:with/2` rather than the `az` alias: the macro must land in the
%% shape collect_call_keys matches, so a change to `val` re-renders the slot.
-spec with_macro_branch(az:bindings()) -> az:template().
with_macro_branch(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [maps:get(val, ?with([val]))]};
                false -> <<>>
            end
        ]}
    ).

%% A branch read whose key is a *computed* variable (`?get(Key)`, Key bound at runtime) is
%% NOT auto-tracked: add_literal_key only injects a track for a literal atom/binary key. So
%% a change to the binding the computed key names freezes the branch -- the documented
%% limitation. (`which`, a literal read, still tracks; only the computed read is missed.)
-spec computed_key(az:bindings()) -> az:template().
computed_key(Bindings) ->
    Key = ?get(which),
    ?html(
        {'div', [{id, ~"x"}], [
            case ?get(flag) of
                true -> {p, [], [?get(Key)]};
                false -> <<>>
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

%% A raw tuple returned by a HELPER (`wrap_raw/1`), not a literal `?raw` at the
%% template site. The documented rule honors the `raw/1` opt-out only when literal
%% at the slot, so a helper-returned raw in a content slot must be ESCAPED (SSR and
%% across the diff), never spliced/innerHTMLd -- unlike `raw_text/1` above.
-spec helper_raw(az:bindings()) -> az:template().
helper_raw(Bindings) ->
    ?html({'div', [{id, ~"x"}], [wrap_raw(?get(html))]}).

wrap_raw(Value) ->
    arizona_template:raw(Value).

%% A content slot whose value becomes a `?local` arriving at runtime (`kind` switches
%% it from text to a local, to a local with a `raw/1` init, or a template init).
-spec runtime_local_content(az:bindings()) -> az:template().
runtime_local_content(Bindings) ->
    ?html({'div', [{id, ~"x"}], [local_or_text(?get(kind), ?get(v))]}).

local_or_text(text, V) -> V;
local_or_text(local, V) -> arizona_template:local(k, V);
local_or_text(raw_local, V) -> arizona_template:local(k, arizona_template:raw(V));
local_or_text(template_local, V) -> arizona_template:local(k, ?html({b, [], [V]})).

%% A helper's `raw/1` around a template, once `wrap` is true: redundant in content (a
%% template is markup already), refused in an attribute value (which must be text).
-spec helper_raw_template(az:bindings()) -> az:template().
helper_raw_template(Bindings) ->
    ?html({'div', [{id, ~"x"}], [maybe_raw_template(?get(wrap), ?get(name))]}).

-spec helper_raw_template_nodiff(az:bindings()) -> az:template().
helper_raw_template_nodiff(Bindings) ->
    ?html({'div', [{id, ~"x"}, 'az-nodiff'], [maybe_raw_template(?get(wrap), ?get(name))]}).

-spec helper_raw_template_json(az:bindings()) -> az:template().
helper_raw_template_json(Bindings) ->
    ?json({'Column', [], [maybe_raw_json(?get(wrap), ?get(name))]}).

maybe_raw_template(true, Name) -> arizona_template:raw(?html({b, [], [Name]}));
maybe_raw_template(false, Name) -> Name.

maybe_raw_json(true, Name) -> arizona_template:raw(?json({'Text', [], [Name]}));
maybe_raw_json(false, Name) -> Name.

%% A component, template or `?local` under `raws` levels of a helper's `raw/1`: the
%% raw is redundant, so every level renders and diffs as the bare value (`raws => 0`).
-spec raw_wrapped(az:bindings()) -> az:template().
raw_wrapped(Bindings) ->
    ?html({'div', [], [wrapped_value(?get(raws), ?get(kind), ?get(v))]}).

wrapped_value(0, stateful, V) ->
    arizona_template:stateful(arizona_counter, #{id => ~"c1", count => V});
wrapped_value(0, stateless, V) ->
    arizona_template:stateless(fun italic/1, #{t => V});
wrapped_value(0, template, V) ->
    ?html({b, [], [V]});
wrapped_value(0, local, V) ->
    arizona_template:local(~"k", V);
wrapped_value(N, Kind, V) ->
    arizona_template:raw(wrapped_value(N - 1, Kind, V)).

italic(Bindings) ->
    ?html({i, [], [?get(t)]}).

%% A template has no rendering inside a raw-text element: in a literal `?raw`, and a
%% helper's raw (inside the literal `?raw` a script/style slot requires).
-spec script_literal_raw_template(az:bindings()) -> az:template().
script_literal_raw_template(Bindings) ->
    ?html({script, [], [?raw(text_or_template(?get(wrap), ?get(name)))]}).

-spec script_helper_raw_template(az:bindings()) -> az:template().
script_helper_raw_template(Bindings) ->
    ?html({script, [], [?raw(maybe_raw_template(?get(wrap), ?get(name)))]}).

-spec style_literal_raw_template(az:bindings()) -> az:template().
style_literal_raw_template(Bindings) ->
    ?html({style, [], [?raw(text_or_template(?get(wrap), ?get(name)))]}).

-spec style_helper_raw_template(az:bindings()) -> az:template().
style_helper_raw_template(Bindings) ->
    ?html({style, [], [?raw(maybe_raw_template(?get(wrap), ?get(name)))]}).

-spec textarea_literal_raw_template(az:bindings()) -> az:template().
textarea_literal_raw_template(Bindings) ->
    ?html({textarea, [], [?raw(text_or_template(?get(wrap), ?get(name)))]}).

-spec textarea_helper_raw_template(az:bindings()) -> az:template().
textarea_helper_raw_template(Bindings) ->
    ?html({textarea, [], [maybe_raw_template(?get(wrap), ?get(name))]}).

-spec title_literal_raw_template(az:bindings()) -> az:template().
title_literal_raw_template(Bindings) ->
    ?html({title, [], [?raw(text_or_template(?get(wrap), ?get(name)))]}).

-spec title_helper_raw_template(az:bindings()) -> az:template().
title_helper_raw_template(Bindings) ->
    ?html({title, [], [maybe_raw_template(?get(wrap), ?get(name))]}).

%% A `?raw` value in title/textarea content: the element's own end tag in it must
%% not close the element.
-spec title_raw_value(az:bindings()) -> az:template().
title_raw_value(Bindings) ->
    ?html({title, [], [?raw(?get(v))]}).

-spec textarea_raw_value(az:bindings()) -> az:template().
textarea_raw_value(Bindings) ->
    ?html({textarea, [], [?raw(?get(v))]}).

-spec title_raw_helper_raw(az:bindings()) -> az:template().
title_raw_helper_raw(Bindings) ->
    ?html({title, [], [?raw(wrap_raw(?get(v)))]}).

-spec title_helper_raw_value(az:bindings()) -> az:template().
title_helper_raw_value(Bindings) ->
    ?html({title, [], [wrap_raw(?get(v))]}).

%% A text-only template a runtime value carries into a title/textarea renders its
%% text (`doc_title/2` is multi-clause, so it is a call, not inlined markup).
-spec title_text_template_prop(az:bindings()) -> az:template().
title_text_template_prop(Bindings) ->
    ?html({head, [], [{title, [], [doc_title(?get(kind, docs), ?get(page))]}]}).

-spec textarea_text_template_prop(az:bindings()) -> az:template().
textarea_text_template_prop(Bindings) ->
    ?html({'div', [], [{textarea, [], [doc_title(?get(kind, docs), ?get(page))]}]}).

doc_title(docs, Page) -> ?html([~"Docs - ", Page]);
doc_title(plain, Page) -> Page.

text_titled(Bindings) ->
    ?html({title, [], [?get(t)]}).

-spec title_static_text_template(az:bindings()) -> az:template().
title_static_text_template(Bindings) ->
    ?html({head, [], [?stateless(fun text_titled/1, #{t => ?html(~"a < b"), x => ?get(x)})]}).

%% A `?raw` list in a script holding a non-text term.
-spec script_raw_bad_list(az:bindings()) -> az:template().
script_raw_bad_list(Bindings) ->
    ?html({script, [], [?raw(bad_list(?get(v)))]}).

bad_list(V) -> [~"a", {bad, V}].

%% A list and a stream `?each`, bare and under a literal `?raw`.
-spec each_bare(az:bindings()) -> az:template().
each_bare(Bindings) ->
    ?html({ul, [], [?each(fun(I) -> {li, [], [I]} end, ?get(items))]}).

-spec each_raw(az:bindings()) -> az:template().
each_raw(Bindings) ->
    ?html({ul, [], [?raw(?each(fun(I) -> {li, [], [I]} end, ?get(items)))]}).

-spec stream_bare(az:bindings()) -> az:template().
stream_bare(Bindings) ->
    ?html({ul, [], [?each(fun(I, K) -> {li, [{az_key, K}], [maps:get(t, I)]} end, ?get(s))]}).

-spec stream_raw(az:bindings()) -> az:template().
stream_raw(Bindings) ->
    ?html({ul, [], [?raw(?each(fun(I, K) -> {li, [{az_key, K}], [maps:get(t, I)]} end, ?get(s)))]}).

%% An az-nodiff element chosen by `x`, under `raws` helper raws; its content reports
%% each evaluation to the calling process.
-spec nodiff_pick(az:bindings()) -> az:template().
nodiff_pick(Bindings) ->
    ?html({'div', [], [nodiff_value(?get(raws), ?get(x), ?get(y))]}).

nodiff_value(0, _X, Y) -> ?html({section, ['az-nodiff'], [{span, [], [evaluated(Y)]}]});
nodiff_value(N, X, Y) -> arizona_template:raw(nodiff_value(N - 1, X, Y)).

evaluated(Y) ->
    self() ! {nodiff_evaluated, Y},
    Y.

%% The same for a `?raw` literal at the template site, around a template
%% once `wrap` is true.
-spec literal_raw_template(az:bindings()) -> az:template().
literal_raw_template(Bindings) ->
    ?html({'div', [{id, ~"x"}], [?raw(text_or_template(?get(wrap), ?get(name)))]}).

text_or_template(true, Name) -> ?html({b, [], [Name]});
text_or_template(false, Name) -> Name.

%% The same in an attribute value, written literally and returned by a helper.
-spec literal_raw_attr(az:bindings()) -> az:template().
literal_raw_attr(Bindings) ->
    ?html({'div', [{id, ~"x"}, {title, ?raw(text_or_template(?get(wrap), ?get(name)))}], []}).

-spec helper_raw_attr(az:bindings()) -> az:template().
helper_raw_attr(Bindings) ->
    ?html({'div', [{id, ~"x"}, {title, maybe_raw_template(?get(wrap), ?get(name))}], []}).

%% A literal `?raw` part of an interpolated attribute value.
-spec interp_raw_attr(az:bindings()) -> az:template().
interp_raw_attr(Bindings) ->
    ?html(
        {'div', [{id, ~"x"}, {title, [~"t-", ?raw(text_or_template(?get(wrap), ?get(name)))]}], []}
    ).

%% A `?json` prop, from a helper and from a literal `?raw`.
-spec helper_raw_attr_json(az:bindings()) -> az:template().
helper_raw_attr_json(Bindings) ->
    ?json({'Text', [{label, maybe_raw_json(?get(wrap), ?get(name))}], []}).

-spec literal_raw_attr_json(az:bindings()) -> az:template().
literal_raw_attr_json(Bindings) ->
    ?json({'Text', [{label, ?raw(text_or_json(?get(wrap), ?get(name)))}], []}).

text_or_json(true, Name) -> ?json({'Text', [], [Name]});
text_or_json(false, Name) -> Name.

%% A literal `?raw` around a helper's raw holding a command, once `wrap` is true.
-spec raw_raw_effect(az:bindings()) -> az:template().
raw_raw_effect(Bindings) ->
    ?html({'div', [{id, ~"x"}], [?raw(raw_command(?get(wrap)))]}).

-spec raw_raw_effect_attr(az:bindings()) -> az:template().
raw_raw_effect_attr(Bindings) ->
    ?html({'div', [{id, ~"x"}, {title, ?raw(raw_command(?get(wrap)))}], []}).

raw_command(true) -> arizona_template:raw(arizona_js:push_event(~"x"));
raw_command(false) -> ~"a".

%% A `?get` and a `?raw` side by side in one plain-list `?each` item.
-spec raw_in_list_item(az:bindings()) -> az:template().
raw_in_list_item(Bindings) ->
    ?html(
        {ul, [], [
            ?each(fun(I) -> {li, [], [maps:get(t, I), ?raw(maps:get(h, I))]} end, ?get(items))
        ]}
    ).

%% The same pair in a stream `?each` item.
-spec raw_in_stream_item(az:bindings()) -> az:template().
raw_in_stream_item(Bindings) ->
    ?html(
        {ul, [], [
            ?each(
                fun(I, K) -> {li, [{az_key, K}], [maps:get(t, I), ?raw(maps:get(h, I))]} end,
                ?get(items)
            )
        ]}
    ).

%% The same pair in a conditional branch element (a nested template).
-spec raw_in_branch(az:bindings()) -> az:template().
raw_in_branch(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(flag) of
                true -> {p, [], [?get(v), ?raw(?get(h))]};
                false -> <<>>
            end
        ]}
    ).

%% A plain and a `?raw` attribute value.
-spec raw_attr(az:bindings()) -> az:template().
raw_attr(Bindings) ->
    ?html({span, [{title, ?get(v)}, {'data-r', ?raw(?get(h))}], []}).

%% An interpolated attribute value mixing static text, a `?raw` part and a plain one.
-spec raw_interp_attr(az:bindings()) -> az:template().
raw_interp_attr(Bindings) ->
    ?html({a, [{href, [~"/a?q=\"<&", ?raw(?get(h)), ~"&z=", ?get(v)]}], []}).

%% Two branches whose only difference is the NAME of a dynamic attribute: switching
%% `mode` must replace the branch, not patch the shared value under the new name and
%% leave the old attribute on the element.
-spec attr_name_swap(az:bindings()) -> az:template().
attr_name_swap(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(mode) of
                a -> {span, [{class, ?get(v)}], [~"x"]};
                b -> {span, [{title, ?get(v)}], [~"x"]}
            end
        ]}
    ).

%% The same swap on an event attribute holding a runtime command (not folded into
%% a static), where a stale name leaves the old listener bound.
-spec event_attr_name_swap(az:bindings()) -> az:template().
event_attr_name_swap(Bindings) ->
    ?html(
        {'div', [], [
            case ?get(mode) of
                a -> {form, [{az_click, ?get(cmd)}], [~"x"]};
                b -> {form, [{az_submit, ?get(cmd)}], [~"x"]}
            end
        ]}
    ).

%% The `?json` counterpart of attr_name_swap/1.
-spec json_attr_name_swap(az:bindings()) -> az:template().
json_attr_name_swap(Bindings) ->
    ?json(
        {'Column', [], [
            case ?get(mode) of
                a -> {'Text', [{color, ?get(v)}], [~"x"]};
                b -> {'Text', [{weight, ?get(v)}], [~"x"]}
            end
        ]}
    ).
