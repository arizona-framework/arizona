-module(static_blog_example).

%% Example of how to use arizona_static to generate a static blog site

-export([generate_site/0]).

%% Generate the complete static site
generate_site() ->
    % Define the pages to generate
    Pages = [
        #{
            path => ~"/",
            view => blog_home_view,
            bindings => #{title => ~"My Arizona Blog"}
        },
        #{
            path => ~"/about",
            view => blog_about_view,
            bindings => #{title => ~"About Me"}
        },
        #{
            path => ~"/post/hello-world",
            view => blog_post_view,
            bindings => #{
                title => ~"Hello World",
                post_id => ~"hello-world",
                content => ~"Welcome to my first blog post!"
            }
        },
        #{
            path => ~"/post/arizona-static",
            view => blog_post_view,
            bindings => #{
                title => ~"Arizona Static Site Generation",
                post_id => ~"arizona-static",
                content => ~"How to build static sites with Arizona framework."
            }
        }
    ],

    % Configuration for static site generation
    Config = #{
        pages => Pages,
        output_dir => ~"/tmp/arizona-static",
        assets_dir => ~"priv/static/assets",
        base_url => ~"https://myblog.com"
    },

    % Generate the static site
    case arizona_static:generate(Config) of
        ok ->
            io:format("Static site generated successfully in '/tmp/arizona-static' directory!~n"),
            ok;
        {error, Reason} ->
            io:format("Failed to generate static site: ~p~n", [Reason]),
            {error, Reason}
    end.
