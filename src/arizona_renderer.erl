-module(arizona_renderer).
-moduledoc """
Behaviour for render-target backends.

The parse transform walks the element-tuple form generically -- assigning `az`
indices, building fingerprints, and constructing dynamic closures. The
target-specific *byte emission* (how an element, attribute, or dynamic text
slot is serialized into the `s` statics) is delegated to a backend module
implementing this behaviour. `arizona_html` emits HTML; other backends emit
other formats while reusing the same walker, diff engine, and transport.
""".

-doc "Tag or attribute atom -> wire name.".
-callback name(atom()) -> binary().

-doc "Start of an element's open tag for the given wire tag name.".
-callback element_open(TagName :: binary()) -> binary().

-doc "The framework-injected `az` diff-target marker for an element.".
-callback az_attr(Az :: binary()) -> binary().

-doc "End of an element's open tag (after attributes), before children.".
-callback element_open_end() -> binary().

-doc "Self-close for a void (childless) element.".
-callback element_void_close() -> binary().

-doc "Close tag for the given wire tag name.".
-callback element_close(TagName :: binary()) -> binary().

-doc "A name/value attribute.".
-callback attr(Name :: binary(), Value :: binary()) -> binary().

-doc "A valueless (boolean) attribute.".
-callback attr_boolean(Name :: binary()) -> binary().

-doc """
An attribute whose value is a compile-time-folded effect command (e.g.
`{az_click, arizona_js:push_event(...)}` on the web, `{on_tap,
arizona_android:push_event(...)}` on native). HTML escapes it into the attribute
(`name="[0,&quot;inc&quot;]"`); native embeds it as a raw JSON value
(`,"name":[0,"inc"]`). `Cmd` is an `t:arizona_effect:cmd/0` term --
`{arizona_effect, list()}` or a list of them.
""".
-callback attr_command(Name :: binary(), Cmd :: term()) -> binary().

-doc """
Static prefix emitted before a *dynamic* attribute's value. HTML keeps the name
in the dynamic (so this is empty); native bakes the prop name into the static
(`,"name":`) so the dynamic carries only the value to stringify.
""".
-callback attr_dyn_name(Name :: binary()) -> binary().

-doc """
Separator emitted before each child after the first (HTML needs none; JSON
needs a comma between array elements).
""".
-callback children_sep() -> binary().

-doc "A static text child (raw text for HTML; a JSON string for native).".
-callback text_child(Text :: binary()) -> binary().

-doc """
The `az` for a dynamic text slot, given the element's `az` and the child slot
index. HTML reuses the element `az` for slot 0 (the comment marker and the
element attribute coexist); native needs a distinct `az` because every node
shares one flat registry.
""".
-callback text_az(ElemAz :: binary(), Slot :: non_neg_integer()) -> binary().

-doc "Open marker for a dynamic text slot with the given `az`.".
-callback text_slot_open(Az :: binary()) -> binary().

-doc "Close marker for a dynamic text slot.".
-callback text_slot_close() -> binary().

-doc "Whether the tag is a void element (no children / self-closing).".
-callback is_void(Tag :: atom()) -> boolean().

-doc """
Raw-text classification of a tag, governing how a dynamic content slot inside it
is emitted.

`none` for ordinary elements: a content slot gets the usual comment-marker diff
target (`<!--az:X-->...<!--/az-->`) and is fully diffable. `raw` for raw-text
elements (`script`/`style`): the browser never decodes character references or
HTML comments there, so the slot is emitted verbatim, markerless and render-once
(comment markers would become literal bytes and corrupt the script/CSS).
`escapable` for escapable-raw-text elements (`textarea`/`title`): character
references ARE decoded, so a scalar slot is HTML-escaped, but it is still
markerless and render-once. Non-HTML backends return `none` -- their wire format
does not use HTML comment markers.
""".
-callback raw_text_kind(Tag :: atom()) -> none | raw | escapable.

-doc """
Prefix a static's embedded `az` references with the fingerprint, so a child
template inlined into a parent does not collide on `az` targets.
""".
-callback scope_static(Fingerprint :: binary(), Static :: binary()) -> binary().

-doc """
Whether this backend's client supports `?OP_LIST_PATCH` -- positional, per-item
diffing of a single-root plain-list `?each` (vs. a wholesale re-render). When
`true`, the parse transform stamps `single_root => true` on such item templates
so the diff emits per-item patches; when `false`, the each keeps the wholesale
path the client already handles. A backend capability, declared by each backend.
""".
-callback supports_list_patch() -> boolean().

-doc """
Escape a dynamic value's rendered bytes for this backend's output. Called at the
render boundary on interpolated scalar values (`arizona_template:escape_value/2`).
HTML entity-escapes (`<`, `&`, ...); the terminal strips control bytes that would
inject escape sequences; a plain-text/JSON backend is the identity. A `?raw`
opt-out is classified out before this callback runs, so it never sees trusted
fragments.
""".
-callback escape(Value :: binary()) -> binary().

-doc """
Render a dynamic attribute's evaluated value to this backend's output bytes.

Called at the render boundary for a dynamic attribute (`{attr, Name, Value}`).
HTML emits ` Name="Escaped"` (or a bare/absent name for a boolean value); the
native backend bakes the prop name into the static, so it emits just the
stringified value. Escaping is governed by the backend (via `escape/1`, through
`arizona_template:escape_value/2`, so a `?raw` opt-out or effect command is
classified out first). A backend that rejects dynamic attributes at compile
time (`attr_dyn_name/1`) never has this callback reached.
""".
-callback render_attr(Name :: binary(), Value :: term()) -> binary().
