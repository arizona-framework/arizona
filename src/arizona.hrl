%% Op codes
-define(OP_TEXT, 0).
-define(OP_SET_ATTR, 1).
-define(OP_REM_ATTR, 2).
-define(OP_UPDATE, 3).
-define(OP_REMOVE_NODE, 4).
-define(OP_INSERT, 5).
-define(OP_REMOVE, 6).
-define(OP_ITEM_PATCH, 7).
-define(OP_REPLACE, 8).
-define(OP_MOVE, 9).

%% Type constants
-define(EACH, 0).

%% Stream record -- internal, handlers use API functions only
-record(stream, {
    key      :: fun((term()) -> term()),
    items    :: #{term() => term()},      %% Key => Item (O(log n) lookup)
    order    :: [term()],                 %% Keys in display order
    pending  :: queue:queue(),            %% Ops in insertion order (O(1) amortized append)
    limit    :: pos_integer() | infinity, %% Max visible items
    on_limit :: halt | drop,              %% Limit mode
    size     :: non_neg_integer()         %% Cached length(order)
}).
