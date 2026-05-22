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

%% Tag or attribute atom -> wire name.
-callback name(atom()) -> binary().
%% Start of an element's open tag for the given wire tag name.
-callback element_open(TagName :: binary()) -> binary().
%% The framework-injected `az` diff-target marker for an element.
-callback az_attr(Az :: binary()) -> binary().
%% End of an element's open tag (after attributes), before children.
-callback element_open_end() -> binary().
%% Self-close for a void (childless) element.
-callback element_void_close() -> binary().
%% Close tag for the given wire tag name.
-callback element_close(TagName :: binary()) -> binary().
%% A name/value attribute.
-callback attr(Name :: binary(), Value :: binary()) -> binary().
%% A valueless (boolean) attribute.
-callback attr_boolean(Name :: binary()) -> binary().
%% Open marker for a dynamic text slot with the given `az`.
-callback text_slot_open(Az :: binary()) -> binary().
%% Close marker for a dynamic text slot.
-callback text_slot_close() -> binary().
%% Whether the tag is a void element (no children / self-closing).
-callback is_void(Tag :: atom()) -> boolean().
%% Prefix a static's embedded `az` references with the fingerprint, so a child
%% template inlined into a parent does not collide on `az` targets.
-callback scope_static(Fingerprint :: binary(), Static :: binary()) -> binary().
