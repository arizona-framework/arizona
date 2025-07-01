-module(arizona_list).

-export([call_item_function/2]).
-export([call_element_function/3]).

%% Call item function and validate result
-spec call_item_function(ItemFun, Item) -> Html when
    ItemFun :: fun((term()) -> arizona_html:html()),
    Item :: term(),
    Html :: arizona_html:html().
call_item_function(ItemFun, Item) ->
    apply(ItemFun, [Item]).

%% Call dynamic element function for list item
-spec call_element_function(Fun, Item, Socket) -> Result when
    Fun :: fun((term(), arizona_socket:socket()) -> term()),
    Item :: term(),
    Socket :: arizona_socket:socket(),
    Result :: term().
call_element_function(Fun, Item, Socket) ->
    apply(Fun, [Item, Socket]).
