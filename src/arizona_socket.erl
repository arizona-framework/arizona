-module(arizona_socket).
-moduledoc """
Components state.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/3]).
-export([put_assigns/2]).
-export([put_assign/3]).
-export([get_assigns/1]).
-export([get_changes/1]).
-export([get_view/1]).
-export([get_id/1]).
-export([get_events/1]).
-export([get_assign/2]).
-export([get_assign/3]).
-export([push_event/3]).
-export([prune/1]).

%

-ignore_xref([put_assigns/2]).
-ignore_xref([put_assign/3]).
-ignore_xref([get_assigns/1]).
-ignore_xref([get_assign/2]).
-ignore_xref([get_assign/3]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-opaque t() :: map().
-export_type([t/0]).

-type assigns() :: map().
-export_type([assigns/0]).

-type changes() :: map().
-export_type([changes/0]).

-type view() :: module().
-export_type([view/0]).

-type events() :: list().
-export_type([events/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new(Id, View, Assigns) -> t()
    when Id :: atom(),
         View :: view(),
         Assigns :: assigns().
new(Id, View, Assigns) ->
    #{
        id => Id,
        view => View,
        assigns => Assigns,
        events => [],
        changes => #{}
    }.

-spec put_assigns(Assigns, Socket1) -> Socket2
    when Assigns :: assigns(),
         Socket1 :: t(),
         Socket2 :: t().
put_assigns(Assigns, Socket) ->
    maps:fold(fun put_assign/3, Socket, Assigns).

-spec put_assign(Key, Value, Socket1) -> Socket2
    when Key :: atom(),
         Value :: term(),
         Socket1 :: t(),
         Socket2 :: t().
put_assign(Key, Value, Socket) ->
    #{assigns := Assigns, changes := Changes} = Socket,
    case Assigns of
        #{Key := Value} ->
            Socket;
        #{} ->
            Socket#{
                assigns => Assigns#{Key => Value},
                changes => Changes#{Key => Value}
            }
    end.

-spec get_assigns(t()) -> assigns().
get_assigns(#{assigns := Assigns}) ->
    Assigns.

-spec get_changes(t()) -> changes().
get_changes(#{changes := Changes}) ->
    Changes.

-spec get_view(t()) -> view().
get_view(#{view := View}) ->
    View.

-spec get_id(t()) -> atom().
get_id(#{id := Id}) ->
    Id.

-spec get_events(t()) -> events().
get_events(#{events := Events}) ->
    Events.

-spec get_assign(Key, t()) -> Got
    when Key :: atom(),
         Got :: term().
get_assign(Key, Socket) ->
    #{assigns := Assigns} = Socket,
    maps:get(Key, Assigns).

-spec get_assign(Key, t(), Default) -> Got
    when Key :: atom(),
         Default :: term(),
         Got :: term().
get_assign(Key, Socket, Default) ->
    #{assigns := Assigns} = Socket,
    maps:get(Key, Assigns, Default).

-spec push_event(EventName, Payload, Socket1) -> Socket2
    when EventName :: binary(),
         Payload :: arizona:payload(),
         Socket1 :: t(),
         Socket2 :: t().
push_event(Name, Payload, #{events := Events} = Socket) ->
    Socket#{events => [[Name, Payload] | Events]}.

-spec prune(Socket1) -> Socket2
    when Socket1 :: t(),
         Socket2 :: t().
prune(Socket) ->
    Socket#{
        events => [],
        changes => #{}
    }.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

render_block_test() ->
    ?assertEqual(
       rendered(), arizona_tpl_render:render_block(block(#{}), #{
            title => <<"Arizona">>,
            view_count => 0,
            decr_btn_text => <<"Decrement">>})).

mount_test() ->
    {Render, Sockets} = arizona_tpl_render:mount(block(#{}), #{
            title => <<"Arizona">>,
            view_count => 0,
            decr_btn_text => <<"Decrement">>}),
    [?assertEqual(rendered(), Render),
     ?assertMatch(#{[0] :=
                       #{id := [0],
                         events := [], view := arizona_tpl_compile,
                         assigns :=
                             #{title := <<"Arizona">>, view_count := 0,
                               decr_btn_text := <<"Decrement">>},
                         changes := #{}},
                   [3] :=
                       #{id := [3],
                         events := [], view := arizona_tpl_compile,
                         assigns :=
                             #{id := <<"1">>, counter_count := 0,
                               btn_text := <<"Increment">>,
                               btn_event := <<"incr">>},
                         changes := #{}},
                   [4] :=
                       #{id := [4],
                         events := [], view := arizona_tpl_compile,
                         assigns :=
                             #{id := <<"2">>, label := <<"Rev. Counter:">>,
                               counter_count := 0, btn_text := <<"Decrement">>,
                               btn_event := <<"decr">>},
                         changes := #{}}}, Sockets)].

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

rendered() ->
    [<<"<main arz-id=\"root\"><h1>">>, <<"Arizona">>,
     <<"</h1>">>,
     [<<"<div arz-id=\"[3]\" id=\"">>, <<"1">>,
      <<"\"><span>">>, <<"Count:">>, <<"<b>">>, <<"0">>,
      <<"</b></span><br/>">>,
      [<<"Increment">>,
       <<"<button arz-target=\"[3]\" onclick=\"">>,
       <<"incr">>, <<"\" type=\"button\">">>, <<"Increment">>,
       <<"</button>">>, <<"Increment">>],
      <<"</div>">>],
     [<<"<div arz-id=\"[4]\" id=\"">>, <<"2">>,
      <<"\"><span>">>, <<"Rev. Counter:">>, <<"<b>">>, <<"0">>,
      <<"</b></span><br/>">>,
      [<<"Decrement">>,
       <<"<button arz-target=\"[4]\" onclick=\"">>,
       <<"decr">>, <<"\" type=\"button\">">>, <<"Decrement">>,
       <<"</button>">>, <<"Decrement">>],
      <<"</div>">>],
     <<"</main>">>].

block(Macros) ->
    arizona_tpl_compile:compile(arizona_tpl_compile, view, Macros).

-endif.
