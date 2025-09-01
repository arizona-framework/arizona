-module(static_blog_example).
-export([generate_site/0]).

%% Generate the complete static site
generate_site() ->
    {ok, Cwd} = file:get_cwd(),
    OutputDir = filename:join(Cwd, ~"examples/static/blog"),
    ok = file:del_dir_r(OutputDir),

    % Configuration for static site generation
    Config = #{
        route_paths => #{
            ~"/" => #{parallel => true},
            ~"/about" => #{parallel => true},
            ~"/post/hello-world" => #{parallel => true},
            ~"/post/arizona-static" => #{parallel => true},
            ~"/assets/js/arizona.min.js" => #{parallel => true},
            ~"/assets/js/arizona-worker.min.js" => #{parallel => true}
        },
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
