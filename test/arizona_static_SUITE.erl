-module(arizona_static_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, static_generation_tests},
        {group, error_handling_tests},
        {group, sitemap_tests}
    ].

groups() ->
    [
        {static_generation_tests, [parallel], [
            generate_simple_html_test,
            generate_with_assets_test,
            generate_nested_paths_test,
            generate_index_page_test,
            file_extension_handling_test,
            parallel_generation_test
        ]},
        {error_handling_tests, [parallel], [
            file_permission_error_test
        ]},
        {sitemap_tests, [parallel], [
            html_only_in_sitemap_test,
            sitemap_xml_structure_test,
            sitemap_base_url_test,
            empty_sitemap_test
        ]}
    ].

init_per_suite(Config) ->
    %% Setup test configuration
    TmpDir = proplists:get_value(priv_dir, Config),

    % Create test asset files
    AssetsDir = filename:join(TmpDir, "assets"),
    CssDir = filename:join(AssetsDir, "css"),
    JsDir = filename:join(AssetsDir, "js"),
    ok = filelib:ensure_dir(filename:join(CssDir, "dummy")),
    ok = filelib:ensure_dir(filename:join(JsDir, "dummy")),

    CssFile = filename:join(CssDir, "style.css"),
    JsFile = filename:join(JsDir, "app.js"),
    ok = file:write_file(CssFile, ~"/* Test CSS */"),
    ok = file:write_file(JsFile, ~"console.log('test');"),

    % Random port
    ServerPort = 0,
    MockHomeViewModule = arizona_static_mock_home_view,
    MockAboutViewModule = arizona_static_mock_about_view,
    MockPostViewModule = arizona_static_mock_post_view,
    MockLayoutModule = arizona_static_mock_layout,

    % Create mock layout module
    MockLayoutCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>{arizona_template:get_binding(page_title, Bindings)}</title>
        </head>
        <body>
            <nav>
                <a href="/">Home</a>
                <a href="/about">About</a>
            </nav>
            <main>
                {arizona_template:render_slot(arizona_template:get_binding(main_content, Bindings))}
            </main>
        </body>
        </html>
        """).
    """", [{module, merl:term(MockLayoutModule)}]),

    % Create mock home view
    MockHomeViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Args, _Req) ->
        Layout = {'@layout_module', render, main_content, #{
            page_title => ~"Test Home"
        }},
        arizona_view:new('@module', #{
            id => ~"home"
        }, Layout).

    render(_Bindings) ->
        arizona_template:from_string(~"""
        <div id="home">
            <h1>Welcome Home</h1>
            <p>This is the home page.</p>
        </div>
        """).
    """", [
        {module, merl:term(MockHomeViewModule)},
        {layout_module, merl:term(MockLayoutModule)}
    ]),

    % Create mock about view
    MockAboutViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Args, _Req) ->
        Layout = {'@layout_module', render, main_content, #{
            page_title => ~"About Us"
        }},
        arizona_view:new('@module', #{
            id => ~"about"
        }, Layout).

    render(_Bindings) ->
        arizona_template:from_string(~"""
        <div id="about">
            <h1>About Us</h1>
            <p>This is the about page.</p>
        </div>
        """).
    """", [
        {module, merl:term(MockAboutViewModule)},
        {layout_module, merl:term(MockLayoutModule)}
    ]),

    % Create mock post view
    MockPostViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(Posts, Req) ->
        {ReqBindings, _UpdatedReq} = arizona_request:get_bindings(Req),
        PostId = maps:get(post_id, ReqBindings),
        case Posts of
            #{PostId := Post} ->
                Layout = {'@layout_module', render, main_content, #{
                    page_title => maps:get(title, Post)
                }},
                arizona_view:new('@module', Post#{
                    id => ~"post"
                }, Layout);
            #{} ->
                error({404, PostId})
        end.

    render(Bindings) ->
        arizona_template:from_string(~"""
        <article id="post">
            <h1>{arizona_template:get_binding(title, Bindings)}</h1>
            <div class="content">
                {arizona_template:get_binding(content, Bindings)}
            </div>
        </article>
        """).
    """", [
        {module, merl:term(MockPostViewModule)},
        {layout_module, merl:term(MockLayoutModule)}
    ]),

    % Compile and load mock modules
    {ok, _LayoutBinary} = merl:compile_and_load(MockLayoutCode),
    {ok, _HomeViewBinary} = merl:compile_and_load(MockHomeViewCode),
    {ok, _AboutViewBinary} = merl:compile_and_load(MockAboutViewCode),
    {ok, _PostViewBinary} = merl:compile_and_load(MockPostViewCode),

    % Start test server
    Routes = [
        {view, ~"/", MockHomeViewModule, #{}},
        {view, ~"/about", MockAboutViewModule, #{}},
        {view, ~"/post/:post_id", MockPostViewModule, #{
            ~"hello-world" => #{
                title => ~"Hello World",
                content => ~"Welcome to my first blog post!"
            },
            ~"arizona-static" => #{
                title => ~"Arizona Static",
                content => ~"How to build static sites."
            }
        }},
        {asset, ~"/assets/css/style.css", {file, list_to_binary(CssFile)}},
        {asset, ~"/assets/js/app.js", {file, list_to_binary(JsFile)}},
        {websocket, ~"/live"}
    ],

    %% Ensure applications are started
    {ok, _} = application:ensure_all_started(cowboy),
    %% For gun WebSocket client
    {ok, _} = application:ensure_all_started(gun),
    %% Start arizona with complete configuration
    ok = arizona:start(#{
        server => #{
            transport_opts => [{port, ServerPort}],
            routes => Routes
        }
    }),

    [
        {server_port, ServerPort},
        {mock_home_view_module, MockHomeViewModule},
        {mock_about_view_module, MockAboutViewModule},
        {mock_post_view_module, MockPostViewModule},
        {mock_layout_module, MockLayoutModule}
        | Config
    ].

end_per_suite(Config) ->
    % Stop arizona
    _ = arizona:stop(),

    % Clean up asset files
    TmpDir = proplists:get_value(priv_dir, Config),
    AssetsDir = filename:join(TmpDir, "assets"),
    ok = file:del_dir_r(AssetsDir),

    % Clean up mock modules
    {mock_home_view_module, MockHomeViewModule} = proplists:lookup(mock_home_view_module, Config),
    {mock_about_view_module, MockAboutViewModule} = proplists:lookup(
        mock_about_view_module, Config
    ),
    {mock_post_view_module, MockPostViewModule} = proplists:lookup(mock_post_view_module, Config),
    {mock_layout_module, MockLayoutModule} = proplists:lookup(mock_layout_module, Config),

    Modules = [
        MockHomeViewModule,
        MockAboutViewModule,
        MockPostViewModule,
        MockLayoutModule
    ],

    lists:foreach(
        fun(Module) ->
            case code:is_loaded(Module) of
                {file, _} ->
                    code:delete(Module),
                    code:purge(Module);
                false ->
                    ok
            end
        end,
        Modules
    ),

    ok.

init_per_testcase(TestCase, Config) ->
    % Create unique temp directory for each test
    TestDir = filename:join([
        "/tmp",
        atom_to_list(?MODULE),
        atom_to_list(TestCase),
        integer_to_list(erlang:system_time())
    ]),
    [{test_output_dir, TestDir} | Config].

end_per_testcase(_TestCase, Config) ->
    % Clean up test directory
    {test_output_dir, TestDir} = proplists:lookup(test_output_dir, Config),
    _ = file:del_dir_r(TestDir),

    ok.

%% --------------------------------------------------------------------
%% Test Cases - Static Generation
%% --------------------------------------------------------------------

generate_simple_html_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            ~"/" => #{},
            ~"/about" => #{}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % Check files exist
    ?assert(filelib:is_file(filename:join(OutputDir, "index.html"))),
    ?assert(filelib:is_file(filename:join(OutputDir, "about.html"))),
    ?assert(filelib:is_file(filename:join(OutputDir, "sitemap.xml"))),

    % Check content
    {ok, IndexContent} = file:read_file(filename:join(OutputDir, "index.html")),
    ?assert(binary:match(IndexContent, ~"Welcome Home") =/= nomatch),
    ?assert(binary:match(IndexContent, ~"Test Home") =/= nomatch),

    {ok, AboutContent} = file:read_file(filename:join(OutputDir, "about.html")),
    ?assert(binary:match(AboutContent, ~"About Us") =/= nomatch),

    ok.

generate_with_assets_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            ~"/" => #{},
            ~"/assets/css/style.css" => #{},
            ~"/assets/js/app.js" => #{}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % Check HTML and asset files exist
    ?assert(filelib:is_file(filename:join(OutputDir, "index.html"))),
    ?assert(filelib:is_file(filename:join([OutputDir, "assets", "css", "style.css"]))),
    ?assert(filelib:is_file(filename:join([OutputDir, "assets", "js", "app.js"]))),

    % Check asset content
    {ok, CssContent} = file:read_file(filename:join([OutputDir, "assets", "css", "style.css"])),
    ?assert(binary:match(CssContent, ~"Test CSS") =/= nomatch),

    ok.

generate_nested_paths_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            ~"/post/hello-world" => #{},
            ~"/post/arizona-static" => #{}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % Check nested directory structure
    ?assert(filelib:is_file(filename:join([OutputDir, "post", "hello-world.html"]))),
    ?assert(filelib:is_file(filename:join([OutputDir, "post", "arizona-static.html"]))),

    % Check content
    {ok, PostContent} = file:read_file(filename:join([OutputDir, "post", "hello-world.html"])),
    ?assert(binary:match(PostContent, ~"Hello World") =/= nomatch),
    ?assert(binary:match(PostContent, ~"Welcome to my first blog post!") =/= nomatch),

    ok.

generate_index_page_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{~"/" => #{}},
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % Root path should create index.html
    ?assert(filelib:is_file(filename:join(OutputDir, "index.html"))),
    ?assertNot(filelib:is_file(filename:join(OutputDir, ".html"))),

    ok.

file_extension_handling_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            % Should become about.html
            ~"/about" => #{},
            % Should keep .css extension
            ~"/assets/css/style.css" => #{},
            % Should keep .js extension
            ~"/assets/js/app.js" => #{}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % HTML route should get .html extension
    ?assert(filelib:is_file(filename:join(OutputDir, "about.html"))),

    % Assets should keep original extensions
    ?assert(filelib:is_file(filename:join([OutputDir, "assets", "css", "style.css"]))),
    ?assert(filelib:is_file(filename:join([OutputDir, "assets", "js", "app.js"]))),

    ok.

parallel_generation_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            % Sequential generation
            ~"/" => #{},
            ~"/about" => #{parallel => false},
            % Parallel generation
            ~"/post/hello-world" => #{parallel => true},
            ~"/post/arizona-static" => #{parallel => true}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % Check all files were generated
    ?assert(filelib:is_file(filename:join(OutputDir, "index.html"))),
    ?assert(filelib:is_file(filename:join(OutputDir, "about.html"))),
    ?assert(filelib:is_file(filename:join([OutputDir, "post", "hello-world.html"]))),
    ?assert(filelib:is_file(filename:join([OutputDir, "post", "arizona-static.html"]))),

    ok.

%% --------------------------------------------------------------------
%% Test Cases - Error Handling
%% --------------------------------------------------------------------

file_permission_error_test(Config) when is_list(Config) ->
    % Try to write to read-only directory
    ReadOnlyDir = "/tmp/readonly_test_dir_" ++ integer_to_list(erlang:system_time()),
    ok = file:make_dir(ReadOnlyDir),
    % Read-only
    ok = file:change_mode(ReadOnlyDir, 8#444),

    StaticConfig = #{
        route_paths => #{~"/" => #{}},
        output_dir => list_to_binary(ReadOnlyDir)
    },

    Result = arizona_static:generate(StaticConfig),
    ?assertMatch({error, {file_operation_failed, _, _}}, Result),

    % Cleanup
    ok = file:change_mode(ReadOnlyDir, 8#755),
    ok = file:del_dir_r(ReadOnlyDir),

    ok.

%% --------------------------------------------------------------------
%% Test Cases - Sitemap
%% --------------------------------------------------------------------

html_only_in_sitemap_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            % HTML - should be in sitemap
            ~"/" => #{},
            % HTML - should be in sitemap
            ~"/about" => #{},
            % CSS - should NOT be in sitemap
            ~"/assets/css/style.css" => #{},
            % JS - should NOT be in sitemap
            ~"/assets/js/app.js" => #{}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    % Check sitemap content
    {ok, SitemapContent} = file:read_file(filename:join(OutputDir, "sitemap.xml")),

    % HTML pages should be in sitemap
    ?assert(binary:match(SitemapContent, ~"https://example.com/") =/= nomatch),
    ?assert(binary:match(SitemapContent, ~"https://example.com/about") =/= nomatch),

    % Assets should NOT be in sitemap
    ?assert(binary:match(SitemapContent, ~"style.css") =:= nomatch),
    ?assert(binary:match(SitemapContent, ~"app.js") =:= nomatch),

    ok.

sitemap_xml_structure_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{
            ~"/" => #{},
            ~"/about" => #{}
        },
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    {ok, SitemapContent} = file:read_file(filename:join(OutputDir, "sitemap.xml")),

    % Check XML structure
    ?assert(
        binary:match(SitemapContent, ~"<?xml version=\"1.0\" encoding=\"UTF-8\"?>") =/= nomatch
    ),
    ?assert(
        binary:match(
            SitemapContent, ~"<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        ) =/= nomatch
    ),
    ?assert(binary:match(SitemapContent, ~"<url>") =/= nomatch),
    ?assert(binary:match(SitemapContent, ~"<loc>") =/= nomatch),
    ?assert(binary:match(SitemapContent, ~"<changefreq>weekly</changefreq>") =/= nomatch),
    ?assert(binary:match(SitemapContent, ~"</urlset>") =/= nomatch),

    ok.

sitemap_base_url_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        route_paths => #{~"/" => #{}},
        output_dir => list_to_binary(OutputDir),
        base_url => ~"https://mysite.com"
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    {ok, SitemapContent} = file:read_file(filename:join(OutputDir, "sitemap.xml")),

    % Should use custom base URL
    ?assert(binary:match(SitemapContent, ~"https://mysite.com/") =/= nomatch),
    ?assert(binary:match(SitemapContent, ~"https://example.com/") =:= nomatch),

    ok.

empty_sitemap_test(Config) when is_list(Config) ->
    {test_output_dir, OutputDir} = proplists:lookup(test_output_dir, Config),

    StaticConfig = #{
        % Only assets, no HTML
        route_paths => #{~"/assets/css/style.css" => #{}},
        output_dir => list_to_binary(OutputDir)
    },

    ?assertEqual(ok, arizona_static:generate(StaticConfig)),

    {ok, SitemapContent} = file:read_file(filename:join(OutputDir, "sitemap.xml")),

    % Should be valid XML but with no URLs
    ?assert(
        binary:match(SitemapContent, ~"<?xml version=\"1.0\" encoding=\"UTF-8\"?>") =/= nomatch
    ),
    ?assert(
        binary:match(
            SitemapContent, ~"<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        ) =/= nomatch
    ),
    ?assert(binary:match(SitemapContent, ~"</urlset>") =/= nomatch),
    ?assert(binary:match(SitemapContent, ~"<url>") =:= nomatch),

    ok.
