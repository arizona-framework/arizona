-module(arizona_text_backend).
-moduledoc false.
-behaviour(arizona_renderer).

%% A render target the framework does not ship, named by backend in a template
%% (`arizona_template:target(arizona_text_backend, Elems)`) and compiled as the
%% target `text`. It writes elements as `[tag attrs]...[/tag]`,
%% so its output is told apart from HTML at a glance, and it answers the capability
%% callbacks the way a frame-repainting target does: no live root, no keyed item
%% ops, no list patch, no `?local`, repeatable attributes.

-export([name/1]).
-export([element_open/1]).
-export([az_attr/1]).
-export([element_open_end/0]).
-export([element_void_close/0]).
-export([element_close/1]).
-export([attr/2]).
-export([attr_bool/2]).
-export([attr_command/2]).
-export([attr_dyn_name/1]).
-export([children_sep/0]).
-export([each_items/1]).
-export([text_child/1]).
-export([text_az/2]).
-export([text_slot_open/1]).
-export([text_slot_close/0]).
-export([is_void/1]).
-export([raw_text_kind/2]).
-export([content_context/2]).
-export([raw_text/4]).
-export([scope_static/3]).
-export([supports_list_patch/0]).
-export([target/0]).
-export([live_root/0]).
-export([content_type/0]).
-export([supports_local/0]).
-export([single_root_document/0]).
-export([supports_nodiff_keep/0]).
-export([keyed_item_ops/0]).
-export([implied_attrs/2]).
-export([boolean_attrs/0]).
-export([attr_names/0]).
-export([escape/1]).
-export([render_attr/2]).
-export([wire_escape/1]).
-export([wire_attr/2]).
-export([static_document/2]).

name(Bin) when is_binary(Bin) -> Bin;
name(Atom) -> atom_to_binary(Atom).

element_open(TagName) -> <<"[", TagName/binary>>.

az_attr(_Az) -> <<>>.

element_open_end() -> ~"]".

element_void_close() -> ~"/]".

element_close(TagName) -> <<"[/", TagName/binary, "]">>.

attr(Name, Value) -> <<" ", Name/binary, "=", Value/binary>>.

attr_bool(Name, true) -> <<" ", Name/binary>>;
attr_bool(_Name, false) -> <<>>.

-spec attr_command(binary(), term()) -> no_return().
attr_command(Name, _Cmd) -> reject(Name).

-spec attr_dyn_name(binary()) -> no_return().
attr_dyn_name(Name) -> reject(Name).

children_sep() -> <<>>.

each_items(Items) -> Items.

text_child(Text) -> Text.

text_az(ElemAz, Slot) -> <<ElemAz/binary, "t", (integer_to_binary(Slot))/binary>>.

text_slot_open(_Az) -> <<>>.

text_slot_close() -> <<>>.

is_void(br) -> true;
is_void(~"br") -> true;
is_void(_Tag) -> false.

raw_text_kind(_Tag, _Context) -> none.

content_context(_Tag, Parent) -> Parent.

raw_text(_Tag, _Before, _After, Value) -> Value.

scope_static(_Fp, _Prefix, Static) -> Static.

supports_list_patch() -> false.

target() -> text.

live_root() -> false.

content_type() -> ~"text/plain; charset=utf-8".

supports_local() -> false.

single_root_document() -> false.

supports_nodiff_keep() -> false.

keyed_item_ops() -> false.

implied_attrs(_Tag, _Names) -> [].

boolean_attrs() -> presence.

attr_names() -> repeatable.

escape(Bin) -> binary:replace(Bin, ~"[", ~"\\[", [global]).

-spec render_attr(binary(), term()) -> no_return().
render_attr(Name, _Value) -> reject(Name).

wire_escape(Bin) -> escape(Bin).

-spec wire_attr(binary(), term()) -> no_return().
wire_attr(Name, _Value) -> reject(Name).

static_document(Doc, _EventNames) -> Doc.

-spec reject(binary()) -> no_return().
reject(Name) ->
    erlang:error({arizona_render_reject, <<"no dynamic attributes here: ", Name/binary>>}).
