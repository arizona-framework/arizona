-define(LV(Str), (begin
    {ok, Tree} = arizona_live_view:parse_str(Str, Macros),
    Tree
end)).

