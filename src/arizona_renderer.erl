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
Prefix a static's embedded `az` references with the fingerprint, so a child
template inlined into a parent does not collide on `az` targets.
""".
-callback scope_static(Fingerprint :: binary(), Static :: binary()) -> binary().
