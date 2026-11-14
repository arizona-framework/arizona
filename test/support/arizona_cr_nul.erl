-module(arizona_cr_nul).
-moduledoc """
Fixture for the characters the HTML parser rewrites before anything reads the page:
a CR (alone or before a LF) becomes a LF and a NUL becomes U+FFFD (or is dropped, in
body text). A value holding one renders in every place an escaped value lands -- an
attribute, element content, server-owned control defaults, a `?local` initial beside
a static affix -- so the SSR page can be compared with the value a live update
writes. The inputs hold a `?local` beside affixes their value sanitization rewrites
(newlines stripped, a url or email value trimmed).
""".
-include("arizona_stateless.hrl").
-export([render/1]).
-export([svg/1]).

render(Bindings) ->
    ?html(
        {'div', [], [
            {p, [{title, ?get(t)}], [?get(t)]},
            {input, [{value, ?get(t)}]},
            {textarea, [], [?get(t)]},
            {span, [{title, [?local(~"k", ~"x\r"), ~"\ny"]}], []},
            {input, [{value, [?local(~"q", ~"x"), ~"\r\ny"]}]},
            {input, [{type, ~"url"}, {value, [~" ", ?local(~"u", ~"x"), ~"/ "]}]},
            {input, [
                {type, ~"email"},
                multiple,
                {value, [~" a@b.c ,\n ", ?local(~"m", ~"d@e.f"), ~" , g@h.i "]}
            ]}
        ]}
    ).

%% An SVG `textArea`: foreign content, whose start tag the parser drops no newline after.
svg(Bindings) ->
    ?html({svg, [], [{~"textArea", [], [?get(t)]}]}).
