-module(arizona_static).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([generate/1]).
-export([generate_page/2]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal page() :: #{
    path := binary(),
    view := module(),
    bindings => arizona_binder:map()
}.

-nominal static_config() :: #{
    pages := [page()],
    output_dir := binary(),
    assets_dir => binary(),
    base_url => binary()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec generate(Config) -> ok | {error, Reason} when
    Config :: static_config(),
    Reason :: term().
generate(#{pages := Pages, output_dir := OutputDir} = Config) ->
    maybe
        ok ?= ensure_output_dir(OutputDir),
        ok ?= maybe_copy_assets(Config),
        ok ?= generate_pages(Pages, OutputDir),
        ok ?= generate_sitemap(Pages, Config),
        ok
    else
        {error, Reason} ->
            {error, Reason}
    end.

-spec generate_page(Page, OutputDir) -> ok | {error, Reason} when
    Page :: page(),
    OutputDir :: binary(),
    Reason :: term().
generate_page(#{path := Path, view := ViewModule} = Page, OutputDir) ->
    maybe
        Bindings = maps:get(bindings, Page, #{}),
        {ok, Html} ?= render_page_html(ViewModule, Bindings),
        FilePath = build_file_path(Path, OutputDir),
        ok ?= ensure_file_dir(FilePath),
        ok ?= write_html_file(FilePath, Html),
        ok
    else
        {error, Reason} ->
            {error, {generate_page_failed, Path, Reason}}
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Render a complete page to HTML string
render_page_html(ViewModule, Bindings) ->
    try
        % Create a mock request for mounting
        ArizonaRequest = arizona_request:new(?MODULE, undefined, #{
            bindings => Bindings
        }),

        % Mount the view (without live functionality)
        View = arizona_view:call_mount_callback(ViewModule, ArizonaRequest),

        % Render the complete view including layout
        {Html, _UpdatedView} = arizona_renderer:render_layout(View),

        {ok, Html}
    catch
        error:Reason:StackTrace ->
            {error, {render_failed, ViewModule, Reason, StackTrace}}
    end.

%% Generate all pages
generate_pages([], _OutputDir) ->
    ok;
generate_pages([Page | Rest], OutputDir) ->
    case generate_page(Page, OutputDir) of
        ok ->
            generate_pages(Rest, OutputDir);
        {error, Reason} ->
            {error, Reason}
    end.

%% Build file path from route path
build_file_path(<<"/", _/binary>> = Path, OutputDir) ->
    % Remove leading slash and add .html extension
    CleanPath = binary:part(Path, 1, byte_size(Path) - 1),
    case CleanPath of
        <<>> ->
            filename:join(OutputDir, ~"index.html");
        _ ->
            filename:join(OutputDir, <<CleanPath/binary, ".html">>)
    end;
build_file_path(Path, OutputDir) ->
    filename:join(OutputDir, <<Path/binary, ".html">>).

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
write_html_file(FilePath, Html) ->
    case file:write_file(FilePath, Html) of
        ok -> ok;
        {error, Reason} -> {error, {write_file_failed, FilePath, Reason}}
    end.

%% Copy assets if specified
maybe_copy_assets(#{assets_dir := AssetsDir, output_dir := OutputDir}) ->
    TargetDir = filename:join(OutputDir, ~"assets"),
    copy_directory(AssetsDir, TargetDir);
maybe_copy_assets(_Config) ->
    ok.

%% Simple directory copy (recursive)
copy_directory(SourceDir, TargetDir) ->
    case filelib:ensure_dir(filename:join(TargetDir, "dummy")) of
        ok ->
            case file:list_dir(SourceDir) of
                {ok, Files} ->
                    copy_files(Files, SourceDir, TargetDir);
                {error, Reason} ->
                    {error, {copy_assets_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {create_assets_dir_failed, Reason}}
    end.

copy_files([], _SourceDir, _TargetDir) ->
    ok;
copy_files([File | Rest], SourceDir, TargetDir) ->
    SourcePath = filename:join(SourceDir, File),
    TargetPath = filename:join(TargetDir, File),
    case filelib:is_dir(SourcePath) of
        true ->
            case copy_directory(SourcePath, TargetPath) of
                ok -> copy_files(Rest, SourceDir, TargetDir);
                Error -> Error
            end;
        false ->
            case file:copy(SourcePath, TargetPath) of
                {ok, _} -> copy_files(Rest, SourceDir, TargetDir);
                {error, Reason} -> {error, {copy_file_failed, File, Reason}}
            end
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
    Urls = [build_sitemap_url(Page, BaseUrl) || Page <- Pages],
    [
        ~"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        ~"<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n",
        Urls,
        ~"</urlset>\n"
    ].

build_sitemap_url(#{path := Path}, BaseUrl) ->
    Url = <<BaseUrl/binary, Path/binary>>,
    [
        ~"  <url>\n",
        ~"    <loc>",
        Url,
        ~"</loc>\n",
        ~"    <changefreq>weekly</changefreq>\n",
        ~"  </url>\n"
    ].
