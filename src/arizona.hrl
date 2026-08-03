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
-define(OP_LIST_PATCH, 10).

%% Type constants
-define(EACH, 0).

%% Stream record -- internal, handlers use API functions only.
%% `order` uses a 2-list buffer to keep `insert/2` (the bulk-append hot
%% path) at O(1) instead of O(N): Front holds the materialized
%% display order, Back holds recently-appended keys NEWEST-first
%% (cons'd to). Logical order = Front ++ lists:reverse(Back). All
%% operations except `insert/2` flush the buffer (Back -> []) before
%% they need the full ordered list. `arizona_template:visible_keys/2`
%% is the only external-facing reader and handles flushing internally.
%%
%% `ref`/`qbase`/`qnext` position the pending queue in a per-stream op sequence
%% so a drain can tell how much of the queue it already consumed (see
%% `arizona_stream:drain_mark/1` and `undrained_pending/2`). `ref` identifies
%% the stream lineage, and the queue holds the ops numbered [qbase, qnext):
%% appends bump `qnext`, while the two operations that REBUILD the queue --
%% `reset/1,2` and `clear_stream_pending/2` -- move `qbase` up to `qnext`, so a
%% watermark recorded against the old queue can never address the new one.
-record(stream, {
    key      :: fun((term()) -> term()),
    items    :: #{term() => term()},      %% Key => Item (O(log n) lookup)
    order    :: {[term()], [term()]},     %% {Front, BackRev} -- see header note
    pending  :: queue:queue(),            %% Ops in insertion order (O(1) amortized append)
    limit    :: pos_integer() | infinity, %% Max visible items
    on_limit :: halt | drop,              %% Limit mode
    size     :: non_neg_integer(),        %% Cached length(order)
    ref      :: reference(),              %% Stream lineage identity
    qbase    :: non_neg_integer(),        %% Op sequence number of pending's head
    qnext    :: non_neg_integer()         %% Op sequence number the next append takes
}).
