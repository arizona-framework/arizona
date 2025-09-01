-module(static_blog_example).
%-behaviour(arizona_static).

%% Example of how to use arizona_static to generate a static blog site

-export([generate_site/0]).

%% Generate the complete static site
generate_site() ->
    OutputDir = output_dir(),

    % Configuration for static site generation
    Config = #{
        pages => pages(),
        output_dir => OutputDir
    },

    % Generate the static site
    case arizona_static:generate(Config) of
        ok ->
            io:format("Static site generated successfully in '~s' directory!~n", [OutputDir]),
            ok;
        {error, Reason} ->
            io:format("Failed to generate static site: ~p~n", [Reason]),
            {error, Reason}
    end.

% Define the pages to generate
pages() ->
    [
        ~"/",
        ~"/about",
        ~"/post/hello-world",
        ~"/post/arizona-static",
        ~"/assets/js/arizona.min.js",
        ~"/assets/js/arizona-worker.min.js"
    ].

% Define the output dir
output_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, ~"priv/static/site").
