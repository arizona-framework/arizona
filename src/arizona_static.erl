-module(arizona_static).

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
-export_type([generation_error/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal config() :: #{
    route_paths := [arizona_server:path()],
    output_dir := binary(),
    base_url => binary()
}.

-nominal generation_error() ::
    server_not_running
    | {http_request_failed, arizona_server:path(), term()}
    | {file_operation_failed, binary(), term()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec generate(Config) -> ok | {error, ErrReason} when
    Config :: config(),
    ErrReason :: generation_error().
generate(#{route_paths := RoutePaths, output_dir := OutputDir} = Config) ->
    maybe
        ok ?= check_server_running(),
        ok ?= ensure_output_dir(OutputDir),
        {ok, HtmlPages} ?= generate_pages(RoutePaths, OutputDir),
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

generate_pages(RoutePaths, OutputDir) ->
    case do_generate_pages(RoutePaths, OutputDir) of
        {error, Reason} ->
            {error, Reason};
        HtmlPages ->
            {ok, HtmlPages}
    end.

%% Generate all pages from route paths
do_generate_pages([], _OutputDir) ->
    [];
do_generate_pages([RoutePath | T], OutputDir) ->
    case generate_file(RoutePath, OutputDir) of
        {ok, "text/html" ++ _} ->
            [RoutePath | do_generate_pages(T, OutputDir)];
        {ok, _FileType} ->
            do_generate_pages(T, OutputDir);
        {error, Reason} ->
            {error, Reason}
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
