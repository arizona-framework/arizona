%% Diff op codes -- the server-to-client patch protocol, shared by the diff
%% engine, the socket and every client runtime. `arizona_wire_SUITE` checks
%% each client's copy against this header.
%%
%% 3 is unassigned: it was `OP_UPDATE` (innerHTML on the resolved element),
%% removed once every container full render moved to the marker-aware
%% `?OP_TEXT` -- a content slot's `az` is not reliably carried by an element,
%% so innerHTML on whatever the client resolves destroys the slot's static
%% siblings (the whole view when that element is the view root). It is free to
%% reuse for a genuinely new op: codes 0..9 emit as a single byte in
%% `arizona_socket:op_code_iodata/1` and 10+ cost an extra one per op, so the
%% cheap range is worth reclaiming.
-define(OP_TEXT, 0).
-define(OP_SET_ATTR, 1).
-define(OP_REM_ATTR, 2).
-define(OP_REMOVE_NODE, 4).
-define(OP_INSERT, 5).
-define(OP_REMOVE, 6).
-define(OP_ITEM_PATCH, 7).
-define(OP_REPLACE, 8).
-define(OP_MOVE, 9).
-define(OP_LIST_PATCH, 10).
