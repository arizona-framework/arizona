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
-callback name(tag()) -> binary().

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
-callback is_void(Tag :: tag()) -> boolean().

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
-callback raw_text_kind(Tag :: tag(), Context :: content_context()) -> raw_text_kind().

-doc """
The content context an element's children are parsed in.

`foreign` is the HTML parser's foreign-content mode (inside `<svg>`), where an
element is ordinary parsed content -- comments are comments, so a slot keeps its
markers and stays diffable. The distinction only matters for a tag classified
differently in each: an HTML `<title>` makes comment markers literal text, an SVG
`<title>` does not. Backends with no such mode return `Parent` unchanged.
""".
-callback content_context(Tag :: tag(), Parent :: content_context()) -> content_context().

-doc """
The `arizona_template` function that names an `?each` compiled for `Context`.

The transform renames every `?each` call to this, so a backend decides how its
own eaches are marked instead of the transform hardcoding one name per target.
A backend with a single content context returns the same marker for every
`Context`, which makes the renaming inert for it by its own answer rather than
by a target check in the transform.
""".
-callback each_marker(Context :: content_context()) -> atom().

-type content_context() :: html | foreign.
-export_type([content_context/0]).

-doc """
An element tag or attribute name as written in a template.

The **atom** form goes through `name/1`, which a backend may translate -- `?html`
replaces `_` with `-`, so `az_click` needs no quoting. The **binary** form is taken
verbatim, which is the only way to write a name that genuinely contains an
underscore: `'my-widget_v2'` reaches the output as `my-widget-v2`, a different
element, silently. Every tag-keyed callback accepts both and normalises before
classifying, so which form is written never changes how a tag is classified.
""".
-type tag() :: atom() | binary().
-export_type([tag/0]).

-doc """
How a dynamic content slot inside an element must be rendered.

`none` is an ordinary parsed-markup element: the slot is comment-anchored and
diffable. `raw` (`script`/`style` in HTML) is emitted verbatim, since the browser
decodes no character references there, so escaping would corrupt it. `escapable`
(`textarea`/`title`) escapes a scalar because references ARE decoded, but comments
are still literal, so the slot carries no markers either way. Non-HTML backends
answer `none` for every tag -- their wire format has no comment markers to corrupt.
""".
-type raw_text_kind() :: none | raw | escapable.
-export_type([raw_text_kind/0]).

-doc """
Prefix a static's embedded `az` references with `Prefix`, so a child template
inlined into a parent does not collide on `az` targets.

`Fingerprint` is the fingerprint of the template the static belongs to, and it
is the **anchor**: the parse transform builds every marker from the id it
allocated, so a framework `az` in a compiled static is always `<Fingerprint>-…`.
Match on `<marker-open><Fingerprint>`, never on the marker opener alone -- a
static text child is spliced verbatim (the raw-HTML seam), so ` az="` and
`<!--az:` also occur as ordinary user content in a page that shows markup.
""".
-callback scope_static(Fingerprint :: binary(), Prefix :: binary(), Static :: binary()) ->
    binary().

-doc """
Whether this backend's client supports `?OP_LIST_PATCH` -- positional, per-item
diffing of a single-root plain-list `?each` (vs. a wholesale re-render). When
`true`, the parse transform stamps `single_root => true` on such item templates
so the diff emits per-item patches; when `false`, the each keeps the wholesale
path the client already handles. A backend capability, declared by each backend.
""".
-callback supports_list_patch() -> boolean().

-doc """
This backend's render-target name (`html` | `native` | `terminal`) -- the atom the
`?html`/`?native`/`?terminal` macros expand to. Lets the parse transform recover a
target name from a backend module without hardcoding the module -> name mapping.
""".
-callback target() -> atom().

-doc """
Whether this backend supports the client-owned `?local` slot (a browser/HTML-only
feature). A backend capability, declared by each backend; the parse transform
rejects `?local` at compile time when `false`.
""".
-callback supports_local() -> boolean().

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
Neutralize a dynamic value spliced into a **raw-text** element (`raw_text_kind/2
=:= raw` -- HTML `script`/`style`). Such content is emitted verbatim (the browser
decodes nothing there), so HTML entity-escaping does not apply, yet a value
carrying a close-tag sequence (`</script>`) would still break out of the element
into HTML parsing. The parse transform wraps every raw-text dynamic in this
callback, so the backend that owns raw-text elements neutralizes the breakout;
backends without raw-text elements return the value unchanged.

`Tag` is the enclosing element, because the neutralization a value needs is a
property of *that element's* tokenizer state, not of raw text in general: HTML
reads `<script>` content in the script-data states (which `<!--` and `<script`
move) but `<style>` content as plain RAWTEXT (which only its own close tag ends).
Rewriting the script-data sequences inside a `<style>` would defend nothing and
corrupt valid CSS, so the backend needs the tag to pick the right policy.
""".
-callback raw_text(Tag :: atom(), Value :: term()) -> term().

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
