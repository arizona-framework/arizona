-module(arizona_text_page).
-moduledoc false.

%% Compiles and loads a page of a render target without the optional
%% `embed_attrs/2` (`arizona_text_backend`, as `arizona_terminal`). The
%% backend is a test module, so the page cannot be a test module beside it: the
%% parse transform calls the backend while the page compiles.

-export([load/0]).

-spec load() -> module().
load() ->
    {module, arizona_text_backend} = code:ensure_loaded(arizona_text_backend),
    Forms = [
        form("-module(arizona_text_view)."),
        form("-export([mount/1, render/1])."),
        form("mount(_Bindings) -> {#{id => <<\"text_view\">>}, #{}}."),
        form(
            "render(_Bindings) -> "
            "arizona_template:target(arizona_text_backend, {line, [], [<<\"plain\">>]})."
        )
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [{parse_transform, arizona_parse_transform}]),
    true = code:soft_purge(Mod),
    {module, Mod} = code:load_binary(Mod, "", Bin),
    Mod.

form(Src) ->
    {ok, Tokens, _} = erl_scan:string(Src),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.
