-module(arizona_template_renderer).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([client_render/2]).
-export([server_render/2]).
-export([render_changes/3]).

%

-ignore_xref([client_render/2]).
-ignore_xref([server_render/2]).
-ignore_xref([render_changes/4]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type assigns() :: #{atom() := term()}.
-export_type([assigns/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec client_render(Block, Assigns) -> Rendered
    when Block :: arizona_template_compiler:block(),
         Assigns :: assigns(),
         Rendered :: iolist().
client_render(Block, Assigns0) ->
    Mod = maps:get(module, Block),
    Socket0 = arizona_socket:new(Mod, Assigns0),
    Socket = arizona_live_view:mount(Mod, Socket0),
    Assigns = arizona_socket:get_assigns(Socket),
    render_block(Block, Assigns).

-spec server_render(Block, Assigns) -> Result
    when Block :: arizona_template_compiler:block(),
         Assigns :: assigns(),
         Result :: {ok, {Rendered, Sockets}} | {error, timeout},
         Rendered :: iolist(),
         Sockets :: #{arizona_template_compiler:changeable_id() := arizona_socket:t()}.
server_render(Block, Assigns) ->
    Self = self(),
    Pid = spawn(fun() ->
        Rendered = server_render_block(Block, Assigns, Self),
        Self ! {self(), {done, Rendered}}
    end),
    Timeout = 5_000,
    server_render_loop(Pid, Timeout, _Sockets = []).

-spec render_changes(Block, ChangedVars, Assigns) -> Changes
    when Block :: arizona_template_compiler:block(),
         ChangedVars :: [atom()],
         Assigns :: assigns(),
         Changes :: [
            [arizona_template_compiler:changeable_id() | binary()]
            | [binary() | [[binary()]]]
        ].
render_changes(Block, ChangedVars, Assigns) ->
    case render_block_changes(Block, ChangedVars, Assigns) of
        {block, Rendered} ->
            [[~"block", Rendered]];
        [_, Bin] = Changes when is_binary(Bin) ->
            [Changes];
        Changes ->
            Changes
    end.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

render_changeable_indexes([Index | Indexes], Changeable, Assigns) ->
    [render_changeable(maps:get(Index, Changeable), Assigns)
     | render_changeable_indexes(Indexes, Changeable, Assigns)];
render_changeable_indexes([], _, _) ->
    [].

render_changeable({expr, Expr}, Assigns) ->
    render_expr(Expr, Assigns);
render_changeable({block, Block}, Assigns0) ->
    case is_block_visible(Block, Assigns0) of
        true ->
            Mod = maps:get(module, Block),
            Socket0 = arizona_socket:new(Mod, Assigns0),
            Socket = arizona_live_view:mount(Mod, Socket0),
            Assigns = arizona_socket:get_assigns(Socket),
            NormAssigns = maps:get(norm_assigns, Block),
            ChangeableAssigns = changeable_assigns(Assigns, NormAssigns),
            render_block(Block, ChangeableAssigns);
        false ->
            <<>>
    end.

render_expr(Expr, Assigns) ->
    Fun = maps:get(function, Expr),
    arizona_html:to_safe(Fun(Assigns)).

render_block(Block, Assigns) ->
    ChangeableIndexes = maps:get(changeable_indexes, Block),
    Changeable = maps:get(changeable, Block),
    Dynamic = render_changeable_indexes(ChangeableIndexes, Changeable, Assigns),
    zip(maps:get(static, Block), Dynamic).

changeable_assigns(Assigns, NormAssigns) ->
    maps:merge(Assigns, #{K => Fun(Assigns) || K := #{function := Fun} <- NormAssigns}).

zip([S | Static], [D | Dynamic]) ->
    [S, D | zip(Static, Dynamic)];
zip([S | Static], []) ->
    [S | zip(Static, [])];
zip([], [D | Dynamic]) ->
    [D | zip([], Dynamic)];
zip([], []) ->
    [].

server_render_changeable_indexes([Index | Indexes], Changeable, Assigns, Pid) ->
    [server_render_changeable(maps:get(Index, Changeable), Assigns, Pid)
     | server_render_changeable_indexes(Indexes, Changeable, Assigns, Pid)];
server_render_changeable_indexes([], _, _, _) ->
    [].

server_render_changeable({expr, Expr}, Assigns, _Pid) ->
    render_expr(Expr, Assigns);
server_render_changeable({block, Block}, Assigns, Pid) ->
    NormAssigns = maps:get(norm_assigns, Block),
    ChangeableAssigns = changeable_assigns(Assigns, NormAssigns),
    server_render_block(Block, ChangeableAssigns, Pid).

server_render_block(Block, Assigns0, Pid) ->
    Mod = maps:get(module, Block),
    Socket0 = arizona_socket:new(Mod, Assigns0),
    Socket = arizona_live_view:mount(Mod, Socket0),
    Assigns = arizona_socket:get_assigns(Socket),
    case is_block_visible(Block, Assigns) of
        true ->
            ChangeableIndexes = maps:get(changeable_indexes, Block),
            Changeable = maps:get(changeable, Block),
            Dynamic = server_render_changeable_indexes(ChangeableIndexes, Changeable, Assigns, Pid),
            Pid ! {self(), {socket, Block, Socket}},
            [maps:get(static, Block), Dynamic];
        false ->
            Pid ! {self(), {socket, Block, Socket}},
            []
    end.

server_render_loop(Pid, Timeout, Sockets) ->
    receive
        {Pid, {socket, #{id := Id} = Block, Socket0}} ->
            Socket = arizona_socket:set_block(Block, Socket0),
            server_render_loop(Pid, Timeout, [{Id, Socket} | Sockets]);
        {Pid, {done, Rendered}} ->
            {ok, {Rendered, maps:from_list(Sockets)}}
    after
        Timeout ->
            {error, timeout}
    end.

do_render_changes([Index], Block, ChangedVars, Assigns) ->
    Changeable = maps:get(changeable, Block),
    render_changeable_changes(maps:get(Index, Changeable), ChangedVars, Assigns);
do_render_changes([Index | Indexes], Block, ChangedVars, Assigns) ->
    Changeable = maps:get(changeable, Block),
    {block, NestedBlock} = maps:get(Index, Changeable),
    NormAssigns = maps:get(norm_assigns, NestedBlock),
    ChangeableAssigns = changeable_assigns(Assigns, NormAssigns),
    ChangeableAssignsKeys = [K || K := #{vars := Vars} <- NormAssigns,
                                  Var <- Vars, lists:member(Var, ChangedVars)],
    case changes_action(NestedBlock, ChangeableAssignsKeys, ChangeableAssigns) of
        render_changeable ->
            do_render_changes(Indexes, NestedBlock, ChangeableAssignsKeys, ChangeableAssigns);
        render_block ->
            do_render_block(NestedBlock, ChangeableAssigns);
        hide_block ->
            do_hide_block(NestedBlock)
    end.

render_changeable_changes({expr, Expr}, _ChangedVars, Assigns) ->
    render_expr(Expr, Assigns);
render_changeable_changes({block, Block}, ChangedVars, Assigns) ->
    render_block_changes(Block, ChangedVars, Assigns).

render_block_changes(Block, AssignsKeys, Assigns) ->
    case changes_action(Block, AssignsKeys, Assigns) of
        render_changeable ->
            do_render_changeable(Block, AssignsKeys, Assigns);
        render_block ->
            do_render_block(Block, Assigns);
        hide_block ->
            do_hide_block(Block)
    end.

is_block_visible(#{is_visible := true}, _Assigns) ->
    true;
is_block_visible(#{is_visible := {'if', Expr}}, Assigns) ->
    IfFun = maps:get(function, Expr),
    IfFun(Assigns).

changes_action(#{is_visible := true}, _, _) ->
    render_changeable;
changes_action(#{is_visible := {'if', Expr}}, ChangedVars, Assigns) ->
    IfFun = maps:get(function, Expr),
    case IfFun(Assigns) of
        true ->
            Vars = maps:get(vars, Expr),
            case lists:any(fun(Var) -> lists:member(Var, ChangedVars) end, Vars) of
                true ->
                    render_block;
                false ->
                    render_changeable
            end;
        false ->
            hide_block
    end.

do_render_changeable(Block, AssignsKeys, Assigns) ->
    Changes = maps:with(AssignsKeys, Assigns),
    ChangedVars = maps:keys(Changes),
    ChangeableVars = maps:get(changeable_vars, Block),
    Vars = maps:with(ChangedVars, ChangeableVars),
    case maps:values(Vars) of
        [Targets] ->
            render_block_changeable(Targets, Block, ChangedVars, Assigns);
        [] ->
            []
    end.

render_block_changeable(Targets, Block, ChangedVars, Assigns) ->
    lists:filtermap(fun(Indexes) ->
        case do_render_changes(Indexes, Block, ChangedVars, Assigns) of
            {block, Rendered} ->
                {true, [Indexes, Rendered]};
            [] ->
                false;
            Rendered ->
                {true, [Indexes, Rendered]}
        end
    end, Targets).

do_render_block(Block, Assigns) ->
    Mod = maps:get(module, Block),
    Socket0 = arizona_socket:new(Mod, Assigns),
    Socket = arizona_live_view:mount(Mod, Socket0),
    ChangeableAssigns = arizona_socket:get_assigns(Socket),
    self() ! {put_socket, Block, Socket},
    ChangeableIndexes = maps:get(changeable_indexes, Block),
    Changeable = maps:get(changeable, Block),
    Dynamic = render_changeable_indexes(ChangeableIndexes, Changeable, ChangeableAssigns),
    {block, [maps:get(static, Block), Dynamic]}.

do_hide_block(Block) ->
    BlockId = maps:get(id, Block),
    self() ! {remove_socket, BlockId},
    {block, [[], []]}.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

client_render_test() ->
    ?assertEqual([
        <<"<!DOCTYPE html=\"html\" />"
          "<html lang=\"en\">"
          "<head><meta charset=\"UTF-8\" />"
            "<title>Arizona Framework</title>"
            "<script src=\"assets/js/morphdom.min.js\">"
            "</script><script src=\"assets/js/arizona.js\"></script>"
            "<script src=\"assets/js/main.js\"></script>"
          "</head>"
          "<body>"
            "<h1>Arizona Counter:">>, <<"0">>, <<"</h1>">>,
            [<<"<div arizona-id=\"[0,1]\">"
                 "<span>Count:">>, <<"0">>, <<"</span>"
                 "<button arizona-target=\"[arizona-id='[0,1]']\" type=\"button\" "
                 "onclick=\"arizona.send.bind(this)('incr')\">Increment</button>"
               "</div>">>],
             <<>>,
            [<<"<div arizona-id=\"[0,2]\">"
                 "<span>Count:">>, <<"88">>, <<"</span>"
                 "<button arizona-target=\"[arizona-id='[0,2]']\" type=\"button\" "
                 "onclick=\"arizona.send.bind(this)('incr')\">Increment #2</button>"
               "</div>">>],
          <<"</body></html>">>
    ], client_render(block(), #{})).

server_render_test() ->
    ?assertMatch({ok, {
        [% Static
         [<<"<!DOCTYPE html=\"html\" />"
            "<html lang=\"en\">"
            "<head><meta charset=\"UTF-8\" />"
              "<title>Arizona Framework</title>"
              "<script src=\"assets/js/morphdom.min.js\">"
              "</script><script src=\"assets/js/arizona.js\">"
              "</script><script src=\"assets/js/main.js\"></script>"
            "</head>"
            "<body><h1>Arizona Counter:">>,
              % #1
              <<"</h1>">>,
              % #2
              <<>>, % Dummy binary to correctly zip elements
              % #3
            <<"</body></html>">>],
         % Dynamic
         [% #1
          <<"0">>,
          % #2
          [% Static
           [<<"<div arizona-id=\"[0,1]\">"
                "<span>Count:">>, <<"</span>"
                "<button arizona-target=\"[arizona-id='[0,1]']\" type=\"button\" "
                "onclick=\"arizona.send.bind(this)('incr')\">Increment</button>"
              "</div>">>],
           % Dynamic
           [<<"0">>]],
          % #3
          [% Static
           [<<"<div arizona-id=\"[0,2]\">"
                "<span>Count:">>, <<"</span>"
                "<button arizona-target=\"[arizona-id='[0,2]']\" type=\"button\" "
                "onclick=\"arizona.send.bind(this)('incr')\">Increment #2</button>"
              "</div>">>],
           % Dynamic
           [<<"88">>]]
        ]],
        #{[0] := _,
          [0, 1] := _,
          [0, 2] := _}
    }}, server_render(block(), #{})).

render_changes_test() ->
    [
        ?assertEqual([[[0], <<"1">>], [[1, 0], <<"1">>]],
                     render_changes(block(), [count], #{count => 1}))
    ].

render_if_directive_test() ->
    {ok, Block} = arizona_template_compiler:compile(?MODULE, render_if_directive, #{}),
    [
        ?assertEqual([<<>>],
                     client_render(Block, #{is_visible => false})),
        ?assertEqual([[<<"<div arizona-id=\"[0,0]\">">>, <<"Joe">>,
                       <<", can you see me?</div>">>]],
                     client_render(Block, #{is_visible => true,
                                            visible_for => ~"Joe"}))
    ].

render_if_directive_changes_test() ->
    {ok, Block} = arizona_template_compiler:compile(?MODULE, render_if_directive, #{}),
    [
        ?assertEqual([[[0, 0], <<"Joe">>]],
                     render_changes(Block, [visible_for], #{is_visible => true,
                                                            visible_for => ~"Joe"})),
        ?assertEqual([[[0, 0], [[], []]]],
                     render_changes(Block, [visible_for], #{is_visible => false,
                                                            visible_for => ~"Nobody"}))
    ].

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

block() ->
    Macros = #{
        title => ~"Arizona Framework",
        inc_btn_text => ~"Increment #2"
    },
    {ok, Block} = arizona_template_compiler:compile(?MODULE, render, Macros),
    Block.

mount(Socket) ->
    Count = arizona_socket:get_assign(count, Socket, 0),
    arizona_socket:put_assign(count, Count, Socket).

render(Macros) ->
    maybe_parse(~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>{_@title}</title>
        <script src="assets/js/main.js"></script>
    </head>
    <body>
        <h1>Arizona Counter: {_@count}</h1>
        <.counter
            count={_@count}
            btn_text="Increment"
        />
        <.counter
            count={88}
            btn_text={_@inc_btn_text}
        />
    </body>
    </html>
    """, Macros).

counter(Macros) ->
    maybe_parse(~"""
    <div :stateful>
        <span>Count: {_@count}</span>
        <.button event="incr" text={_@btn_text} />
    </div>
    """, Macros).

button(Macros) ->
    maybe_parse(~"""
    <button type="button" :onclick={arizona_js:send(_@event)}>
        {_@text}
    </button>
    """, Macros).

render_if_directive(Macros) ->
    maybe_parse(~"""
    <.render_if_directive_block
        :if={_@is_visible}
        name={_@visible_for}
    />
    """, Macros).

render_if_directive_block(Macros) ->
    maybe_parse(~"""
    <div :stateful>
        {_@name}, can you see me?
    </div>
    """, Macros).

maybe_parse(Template, Macros) ->
    maybe
        {ok, Tokens} ?= arizona_template_scanner:scan(Template),
        {ok, Elems} ?= arizona_template_parser:parse(Tokens),
        {ok, {Elems, Macros}}
    else
        {error, Reason} ->
            {error, Reason}
    end.

-endif.
