%% The `?terminal` (ANSI) render target. Include it beside arizona_stateful.hrl or
%% arizona_stateless.hrl: the attribute declares the target to the parse transform,
%% which compiles `?terminal(...)` with the arizona_terminal backend.
-arizona_target({terminal, arizona_terminal}).

-define(terminal(Elems), arizona_template:terminal(Elems)).
