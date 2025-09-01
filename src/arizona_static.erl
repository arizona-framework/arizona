-module(arizona_static).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([generate/1]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal page() :: binary().

-nominal config() :: #{
    pages := [page()],
    output_dir := binary(),
    base_url => binary()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec generate(Config) -> ok | {error, Reason} when
    Config :: config(),
    Reason :: term().
generate(#{pages := Pages, output_dir := OutputDir} = Config) ->
    maybe
        ok ?= check_server_running(),
        ok ?= ensure_output_dir(OutputDir),
        {ok, RealPages} ?= generate_pages(Pages, OutputDir),
        ok ?= generate_sitemap(RealPages, Config),
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

%% Render a complete page to HTML string
render_file(Page) ->
    maybe
        ok ?= application:ensure_started(inets),
        ServerConfig = arizona:get_config(server),
        Scheme = atom_to_binary(maps:get(scheme, ServerConfig)),
        {ok, IpAddress, Port} = arizona_server:get_address(),
        Address = inet:ntoa(IpAddress),
        Url = io_lib:format("~s://~s:~p~s", [Scheme, Address, Port, Page]),
        {ok, {{_HttpVersion, 200, "OK"}, Headers, Body}} ?=
            httpc:request(get, {Url, []}, [], [{body_format, binary}]),
        ContentType = proplists:get_value("content-type", Headers),
        {ok, ContentType, Body}
    else
        {error, Reason} ->
            {error, Reason}
    end.

generate_pages(Pages, OutputDir) ->
    case do_generate_pages(Pages, OutputDir) of
        {error, Reason} ->
            {error, Reason};
        RealPages ->
            {ok, RealPages}
    end.

%% Generate all pages
do_generate_pages([], _OutputDir) ->
    [];
do_generate_pages([Page | T], OutputDir) ->
    case generate_file(Page, OutputDir) of
        {ok, "text/html" ++ _} ->
            [Page | do_generate_pages(T, OutputDir)];
        {ok, _FileType} ->
            do_generate_pages(T, OutputDir);
        {error, Reason} ->
            {error, Reason}
    end.

generate_file(Url, OutputDir) ->
    maybe
        {ok, Type, Content} ?= render_file(Url),
        FilePath = build_file_path(Url, OutputDir),
        ok ?= ensure_file_dir(FilePath),
        ok ?= write_file(FilePath, Content),
        {ok, Type}
    else
        {error, Reason} ->
            {error, {generate_page_failed, Url, Reason}}
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

%% Write HTML content to file
write_file(FilePath, Html) ->
    case file:write_file(FilePath, Html) of
        ok -> ok;
        {error, Reason} -> {error, {write_file_failed, FilePath, Reason}}
    end.

%% Generate sitemap.xml
generate_sitemap(Pages, #{output_dir := OutputDir} = Config) ->
    BaseUrl = maps:get(base_url, Config, ~"https://example.com"),
    SitemapContent = build_sitemap_xml(Pages, BaseUrl),
    SitemapPath = filename:join(OutputDir, ~"sitemap.xml"),
    case file:write_file(SitemapPath, SitemapContent) of
        ok -> ok;
        {error, Reason} -> {error, {write_sitemap_failed, Reason}}
    end.

build_sitemap_xml(Pages, BaseUrl) ->
    Urls = [build_sitemap_url(Url, BaseUrl) || Url <- Pages],
    [
        ~"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        ~"<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
        Urls,
        ~"</urlset>\n"
    ].

build_sitemap_url(PageUrl, BaseUrl) ->
    Url = <<BaseUrl/binary, PageUrl/binary>>,
    [
        ~"  <url>\n",
        ~"    <loc>",
        Url,
        ~"</loc>\n",
        ~"    <changefreq>weekly</changefreq>\n",
        ~"  </url>\n"
    ].
