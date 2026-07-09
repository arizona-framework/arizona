-module(arizona_accented_docs).
-moduledoc """
Relatório diário de operações.

A module whose documentation is not ASCII: `get_docs` and `describe_component`
render it with `~ts`, so it is the widest blast radius of an `fmt/2` that emits
latin1 instead of UTF-8 -- the response would fail to encode.
""".

-export([resumo/0]).

-doc "Devolve o rótulo padrão: Diário.".
resumo() ->
    ~"Diário".
