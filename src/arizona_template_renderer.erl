-module(arizona_template_renderer).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([render/2]).
-export([render_changes/4]).
-export([server_render/2]).

%

-ignore_xref([render/2]).
-ignore_xref([render_changes/4]).
-ignore_xref([server_render/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type assigns() :: #{atom() := term()}.
-export_type([assigns/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec render(Block, Assigns) -> Rendered
    when Block :: arizona_template_compiler:block(),
         Assigns :: assigns(),
         Rendered :: [binary() | [binary()]].
render(Block, Assigns) ->
    ChangeableIndexes = maps:get(changeable_indexes, Block),
    Changeable = maps:get(changeable, Block),
    Dynamic = render_changeable_indexes(ChangeableIndexes, Changeable, Assigns),
    zip(maps:get(static, Block), Dynamic).

-spec render_changes(Target, Block, ChangesVars, Assigns) -> Changes
    when Target :: arizona_template_compiler:changeable_id(),
         Block :: arizona_template_compiler:block(),
         ChangesVars :: [atom()],
         Assigns :: assigns(),
         Changes :: [{arizona_template_compiler:changeable_id(), binary()}].
render_changes(Target, Block, ChangesVars, Assigns) ->
    case do_render_changes(Target, Block, ChangesVars, Assigns) of
        Changes when is_tuple(Changes) ->
            [Changes];
        Changes ->
            Changes
    end.

server_render(Block, Assigns) ->
    Self = self(),
    Pid = spawn(fun() ->
        Rendered = server_render_block(Block, Assigns, Self),
        Self ! {self(), {done, Rendered}}
    end),
    Timeout = 5_000,
    server_render_loop(Pid, Timeout, _Sockets = []).

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
    Id = maps:get(id, Block),
    Mod = maps:get(module, Block),
    Socket0 = arizona_socket:new(Id, Mod, Assigns0),
    Socket = arizona_live_view:mount(Mod, Socket0),
    Assigns = arizona_socket:get_assigns(Socket),
    NormAssigns = maps:get(norm_assigns, Block),
    ChangeableAssigns = changeable_assigns(Assigns, NormAssigns),
    render(Block, ChangeableAssigns).

render_expr(Expr, Assigns) ->
    Fun = maps:get(function, Expr),
    arizona_html:to_safe(Fun(Assigns)).

changeable_assigns(Assigns, NormAssigns) ->
    #{K => Fun(Assigns) || K := #{function := Fun} <- NormAssigns}.

zip([S | Static], [D | Dynamic]) ->
    [S, D | zip(Static, Dynamic)];
zip([S | Static], []) ->
    [S | zip(Static, [])];
zip([], [D | Dynamic]) ->
    [D | zip([], Dynamic)];
zip([], []) ->
    [].

do_render_changes([Index], Block, ChangesVars, Assigns) ->
    Changeable = maps:get(changeable, Block),
    render_changeable_changes(maps:get(Index, Changeable), ChangesVars, Assigns);
do_render_changes([Index | Indexes], Block, ChangesVars, Assigns) ->
    Changeable = maps:get(changeable, Block),
    {block, NestedBlock} = maps:get(Index, Changeable),
    do_render_changes(Indexes, NestedBlock, ChangesVars, Assigns).

render_changeable_changes({expr, #{id := Id}  = Expr}, _ChangesVars, Assigns) ->
    {Id, render_expr(Expr, Assigns)};
render_changeable_changes({block, Block}, ChangesVars, Assigns) ->
    render_block_changes(Block, ChangesVars, Assigns).

render_block_changes(Block, AssignsKeys, Assigns) ->
    NormAssigns = maps:get(norm_assigns, Block),
    Changes = changeable_assigns(Assigns, maps:with(AssignsKeys, NormAssigns)),
    ChangesVars = maps:keys(Changes),
    ChangeableVars = maps:get(changeable_vars, Block),
    Vars = maps:with(ChangesVars, ChangeableVars),
    [Targets] = maps:values(Vars),
    [do_render_changes(Target, Block, ChangesVars, Changes) || Target <- Targets].

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
    Id = maps:get(id, Block),
    Mod = maps:get(module, Block),
    Socket0 = arizona_socket:new(Id, Mod, Assigns0),
    Socket = arizona_live_view:mount(Mod, Socket0),
    Assigns = arizona_socket:get_assigns(Socket),
    ChangeableIndexes = maps:get(changeable_indexes, Block),
    Changeable = maps:get(changeable, Block),
    Dynamic = server_render_changeable_indexes(ChangeableIndexes, Changeable, Assigns, Pid),
    Pid ! {self(), {socket, Id, Socket}},
    [maps:get(static, Block), Dynamic].

server_render_loop(Pid, Timeout, Sockets) ->
    receive
        {Pid, {socket, Id, Socket}} ->
            server_render_loop(Pid, Timeout, [{Id, Socket} | Sockets]);
        {Pid, {done, Rendered}} ->
            {ok, {Rendered, maps:from_list(Sockets)}}
    after
        Timeout ->
            {error, timeout}
    end.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

render_test() ->
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
            "<h1>Arizona Counter</h1>">>,
            [<<"<div arizona-id=\"[0,0]\"><span>Count:">>, <<"0">>, <<"</span>"
               "<button arizona-target=\"[0,0]\" type=\"button\" "
               "onclick=\"arizona.send.bind(this)('incr')\">Increment</button></div>">>],
             <<>>,
            [<<"<div arizona-id=\"[0,1]\"><span>Count:">>, <<"88">>, <<"</span>"
               "<button arizona-target=\"[0,1]\" type=\"button\" "
               "onclick=\"arizona.send.bind(this)('incr')\">Increment #2</button></div>">>],
          <<"</body></html>">>
    ], render(block(), #{count => 0})).

render_changes_test() ->
    [
        ?assertEqual([{[0, 0, 0], <<"1">>}],
                     render_changes([0], block(), [count], #{count => 1})),
        ?assertEqual([{[0, 0, 0], <<"1">>}],
                     render_changes([0, 0], block(), [count], #{count => 1})),
        ?assertEqual([{[0, 1, 0], <<"1">>}],
                     render_changes([1, 0], block(), [count], #{count => 1}))
    ].

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
            "<body><h1>Arizona Counter</h1>">>,
              % #1
              <<>>, % Dummy binary to correctly zip elements
              % #2
            <<"</body></html>">>],
         % Dynamic
         [ % #1
          [% Static
           [<<"<div arizona-id=\"[0,0]\"><span>Count:">>,
            <<"</span><button arizona-target=\"[0,0]\" type=\"button\" onclick=\"arizona.send.bind(this)('incr')\">Increment</button></div>">>],
           % Dynamic
           [<<"0">>]],
          % #2
          [% Static
           [<<"<div arizona-id=\"[0,1]\"><span>Count:">>,
            <<"</span><button arizona-target=\"[0,1]\" type=\"button\" onclick=\"arizona.send.bind(this)('incr')\">Increment #2</button></div>">>],
           % Dynamic
           [<<"88">>]]
        ]],
        #{[0] :=
              #{id := [0],
                events := [],
                assigns := #{count := 0},
                changes := #{},
                view := arizona_template_renderer},
          [0,0] :=
              #{id := [0,0],
                events := [],
                assigns := #{count := 0},
                changes := #{},
                view := arizona_template_renderer},
          [0,1] :=
              #{id := [0,1],
                events := [],
                assigns := #{count := 88},
                changes := #{},
                view := arizona_template_renderer}}
    }}, server_render(block(), #{count => 0})).

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
    Socket.

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
        <h1>Arizona Counter</h1>
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
