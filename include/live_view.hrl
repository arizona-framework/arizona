%% Macros.
%% TODO: Use ct_expand to compile to tree:
%%       https://github.com/uwiger/parse_trans/blob/master/src/ct_expand.erl
-define(LV(Str), (begin
    {ok, Tree} = arizona_live_view:parse_str(Str, Macros),
    Tree
end)).

