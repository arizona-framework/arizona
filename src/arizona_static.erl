-module(arizona_static).
-moduledoc ~"""
Static site generation by crawling a running Arizona server.

Generates static HTML files by making HTTP requests to a running Arizona
server and saving the responses to disk. Useful for creating static builds
for deployment, SEO optimization, or JAMstack-style architectures.

## Generation Process

1. **Server Check**: Ensures Arizona server is running
2. **Directory Setup**: Creates output directory structure
3. **Page Crawling**: Fetches pages via HTTP (parallel or sequential)
4. **File Writing**: Saves HTML content with proper file extensions
5. **Sitemap Generation**: Creates `sitemap.xml` for SEO

## Configuration

```erlang
Config = #{
    route_paths => #{
        ~"/" => #{parallel => false},
        ~"/about" => #{parallel => true},
        ~"/users/123" => #{parallel => true}
    },
    output_dir => ~"./dist",
    base_url => ~"https://mysite.com",
    timeout => 10000  % 10 seconds per request
}.
```

## Route Options

- `parallel => true` - Process route in parallel worker
- `parallel => false` - Process route sequentially (default)

## File Generation

- `/` → `index.html`
- `/about` → `about.html`
- `/users/123` → `users/123.html`
- Files with extensions preserved as-is

## Sitemap Generation

Automatically generates `sitemap.xml` with:
- All HTML pages discovered during generation
- Configurable base URL for absolute links
- Weekly change frequency for SEO

## Example Usage

```erlang
%% Start Arizona server first
ok = arizona:start(ServerConfig),

%% Generate static site
StaticConfig = #{
    route_paths => #{~"/" => #{}, ~"/about" => #{}},
    output_dir => ~"./build"
},
ok = arizona_static:generate(StaticConfig).

%% Files created:
%% ./build/index.html
%% ./build/about.html
%% ./build/sitemap.xml
```

## Error Handling

Returns specific errors for different failure modes:
- `server_not_running` - Arizona server not started
- `{http_request_failed, Path, Reason}` - HTTP request failed
- `{file_operation_failed, FilePath, Reason}` - File write failed
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([generate/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([generate/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([config/0]).
-export_type([route_options/0]).
-export_type([generation_error/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal config() :: #{
    route_paths := #{arizona_server:path() => route_options()},
    output_dir := binary(),
    base_url => binary(),
    timeout => pos_integer()
}.

-type route_options() :: #{
    parallel => boolean()
}.

-nominal generation_error() ::
    server_not_running
    | {http_request_failed, arizona_server:path(), term()}
    | {file_operation_failed, binary(), term()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Generates a static site from the given configuration.

Crawls the running Arizona server, fetches HTML content, and writes
static files to the output directory. Also generates a sitemap.xml
for SEO purposes.
""".
-spec generate(Config) -> ok | {error, ErrReason} when
    Config :: config(),
    ErrReason :: generation_error().
generate(#{route_paths := RoutePaths, output_dir := OutputDir} = Config) ->
    maybe
        ok ?= check_server_running(),
        ok ?= ensure_output_dir(OutputDir),
        Timeout = maps:get(timeout, Config, 5_000),
        {ok, HtmlPages} ?= generate_pages(RoutePaths, OutputDir, Timeout),
        ok ?= generate_sitemap(HtmlPages, Config),
        ok
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Check if server is running
check_server_running() ->
    case arizona_server:is_running() of
        true ->
            ok;
        false ->
            {error, server_not_running}
    end.

%% Fetch page content from running server
fetch_page(RoutePath) ->
    maybe
        ok ?= application:ensure_started(inets),
        ServerConfig = arizona:get_config(server),
        Scheme = atom_to_binary(maps:get(scheme, ServerConfig)),
        {ok, IpAddress, Port} = arizona_server:get_address(),
        Address = inet:ntoa(IpAddress),
        Url = io_lib:format("~s://~s:~p~s", [Scheme, Address, Port, RoutePath]),
        {ok, {{_HttpVersion, 200, "OK"}, Headers, Body}} ?=
            httpc:request(get, {Url, []}, [], [{body_format, binary}]),
        ContentType = proplists:get_value("content-type", Headers),
        {ok, ContentType, Body}
    else
        {error, Reason} ->
            {error, {http_request_failed, RoutePath, Reason}}
    end.

%% Generate all pages from route paths map
generate_pages(RouteConfigs, OutputDir, Timeout) ->
    MainPid = self(),
    TotalTasks = map_size(RouteConfigs),

    % Process all routes
    _ = [
        case is_parallel_route(Options) of
            true ->
                % Spawn parallel worker
                spawn(fun() ->
                    Result = generate_file(RoutePath, OutputDir),
                    MainPid ! {task_complete, RoutePath, Result}
                end);
            false ->
                % Run sequential task in same process, then send message
                Result = generate_file(RoutePath, OutputDir),
                self() ! {task_complete, RoutePath, Result}
        end
     || RoutePath := Options <- RouteConfigs
    ],

    % Collect all results
    collect_task_results(TotalTasks, [], Timeout).

%% Check if route should be processed in parallel
is_parallel_route(#{parallel := true}) ->
    true;
is_parallel_route(_Options) ->
    false.

%% Collect results from all tasks
collect_task_results(0, HtmlPages, _Timeout) ->
    {ok, HtmlPages};
collect_task_results(Remaining, HtmlPages, Timeout) ->
    receive
        {task_complete, RoutePath, {ok, "text/html" ++ _}} ->
            collect_task_results(Remaining - 1, [RoutePath | HtmlPages], Timeout);
        {task_complete, _RoutePath, {ok, _FileType}} ->
            collect_task_results(Remaining - 1, HtmlPages, Timeout);
        {task_complete, _RoutePath, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.

generate_file(RoutePath, OutputDir) ->
    maybe
        {ok, Type, Content} ?= fetch_page(RoutePath),
        FilePath = build_file_path(RoutePath, OutputDir),
        ok ?= ensure_file_dir(FilePath),
        ok ?= write_file(FilePath, Content),
        {ok, Type}
    else
        {error, Reason} ->
            {error, Reason}
    end.

%% Build file path from route path
build_file_path(<<"/", Path/binary>>, OutputDir) ->
    % Remove leading slash and add .html extension
    case Path of
        <<>> ->
            add_html_extension(~"index", OutputDir);
        _ ->
            add_extension(Path, OutputDir)
    end.

add_extension(Filename, OutputDir) ->
    case filename:extension(Filename) of
        <<>> ->
            add_html_extension(Filename, OutputDir);
        _ ->
            filename:join(OutputDir, Filename)
    end.

add_html_extension(Filename, OutputDir) ->
    add_extension(Filename, ~".html", OutputDir).

%%  Add .html extension and join output dir
add_extension(Filename, Extension, OutputDir) ->
    filename:join(OutputDir, <<Filename/binary, Extension/binary>>).

%% Ensure output directory exists
ensure_output_dir(OutputDir) ->
    case filelib:ensure_dir(filename:join(OutputDir, "dummy")) of
        ok -> ok;
        {error, Reason} -> {error, {create_output_dir_failed, Reason}}
    end.

%% Ensure file directory exists
ensure_file_dir(FilePath) ->
    Dir = filename:dirname(FilePath),
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok -> ok;
        {error, Reason} -> {error, {create_file_dir_failed, Reason}}
    end.

%% Write content to file
write_file(FilePath, Content) ->
    case file:write_file(FilePath, Content) of
        ok -> ok;
        {error, Reason} -> {error, {file_operation_failed, FilePath, Reason}}
    end.

%% Generate sitemap.xml
generate_sitemap(HtmlPages, #{output_dir := OutputDir} = Config) ->
    BaseUrl = maps:get(base_url, Config, ~"https://example.com"),
    SitemapContent = build_sitemap_xml(HtmlPages, BaseUrl),
    SitemapPath = filename:join(OutputDir, ~"sitemap.xml"),
    case file:write_file(SitemapPath, SitemapContent) of
        ok -> ok;
        {error, Reason} -> {error, {file_operation_failed, SitemapPath, Reason}}
    end.

build_sitemap_xml(HtmlRoutePaths, BaseUrl) ->
    Urls = [build_sitemap_url(RoutePath, BaseUrl) || RoutePath <- HtmlRoutePaths],
    [
        ~"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        ~"<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
        Urls,
        ~"</urlset>\n"
    ].

build_sitemap_url(RoutePath, BaseUrl) ->
    Url = <<BaseUrl/binary, RoutePath/binary>>,
    [
        ~"  <url>\n",
        ~"    <loc>",
        Url,
        ~"</loc>\n",
        ~"    <changefreq>weekly</changefreq>\n",
        ~"  </url>\n"
    ].
