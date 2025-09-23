-module(arizona_blog_app_view).
-behaviour(arizona_view).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/2]).
-export([render/1]).
-export([handle_event/3]).
-export([render_post_list/1]).
-export([render_post_detail/1]).
-export([render_post_preview/1]).
-export([render_tag/1]).

mount(_Arg, Req) ->
    Posts = get_posts(),
    Layout =
        {arizona_blog_app_layout, render, main_content, #{
            active_url => arizona_request:get_path(Req)
        }},
    arizona_view:new(
        ?MODULE,
        #{
            id => ~"blog",
            posts => Posts,
            selected_post => undefined,
            selected_tag => all,
            view_mode => list
        },
        Layout
    ).

render(Bindings) ->
    Module = ?MODULE,
    arizona_template:from_html(~"""
    <div id="{arizona_template:get_binding(id, Bindings)}">
    {
        ViewMode = arizona_template:get_binding(view_mode, Bindings),
        case ViewMode of
            list ->
                arizona_template:render_stateless(Module, render_post_list, #{
                    posts => arizona_template:get_binding(posts, Bindings),
                    selected_tag => arizona_template:get_binding(selected_tag, Bindings)
                });
            detail ->
                case arizona_template:find_binding(selected_post, Bindings) of
                    {ok, Post} ->
                        arizona_template:render_stateless(Module, render_post_detail, #{
                            post => Post
                        });
                    error ->
                        ~"<p>Post not found</p>"
                end
        end
    }
    </div>
    """).

render_post_list(Bindings) ->
    arizona_template:from_markdown(~""""
    ## Arizona Blog

    A simple blog demonstrating **Arizona Framework** with markdown templates.

    ---

    ### Filter by Tag

    {
        Posts = arizona_template:get_binding(posts, Bindings),
        SelectedTag = arizona_template:get_binding(selected_tag, Bindings),
        AllTags = lists:usort(lists:flatten([maps:get(tags, Post) || Post <- Posts])),
        arizona_template:render_list(fun(Tag) ->
            arizona_template:from_html(~"""
            {arizona_template:render_stateless(arizona_blog_app_view, render_tag, #{
                tag => Tag,
                selected => Tag =:= SelectedTag
            })}
            """)
        end, [all | AllTags])
    }

    ---

    ### Recent Posts

    {
        Posts = arizona_template:get_binding(posts, Bindings),
        SelectedTag = arizona_template:get_binding(selected_tag, Bindings),
        FilteredPosts = filter_posts_by_tag(Posts, SelectedTag),
        arizona_template:render_list(fun(Post) ->
            arizona_template:from_html(~"""
            {arizona_template:render_stateless(arizona_blog_app_view, render_post_preview, #{
                post => Post
            })}
            """)
        end, FilteredPosts)
    }
    """").

render_post_detail(Bindings) ->
    #{title := Title, content := Content, author := Author, date := Date, tags := Tags} = arizona_binder:get(
        post, Bindings
    ),
    arizona_template:from_markdown(~""""
    <button onclick="arizona.sendEvent('back_to_list')" style="margin-bottom: 1rem;">
        ← Back to Posts
    </button>

    # {Title}

    **By {Author}** • *{Date}*

    {arizona_template:render_list(fun(Tag) ->
        arizona_template:from_html(~"""
        <span
            class="tag"
            style="{[
                ~"background: #e3f2fd; padding: 2px 8px; margin: 0 4px; ",
                ~"border-radius: 4px; font-size: 0.8em;"
            ]}"
        >
            {Tag}
        </span>
        """)
    end, Tags)}

    ---

    {Content}
    """").

render_post_preview(Bindings) ->
    Post = arizona_binder:get(post, Bindings),
    #{
        id := Id,
        title := Title,
        excerpt := Excerpt,
        author := Author,
        date := Date,
        tags := Tags
    } = Post,
    arizona_template:from_markdown(~""""
    ### {Title}

    **By {Author}** • *{Date}*

    {Excerpt}

    {arizona_template:render_list(fun(Tag) ->
        arizona_template:from_html(~"""
        <span
            class="tag"
            style="{[
                ~"background: #e3f2fd; padding: 2px 8px; margin: 0 4px; ",
                ~"border-radius: 4px; font-size: 0.8em;"
            ]}"
        >
            {Tag}
        </span>
        """)
    end, Tags)}

    <button onclick="arizona.sendEvent('view_post', \{id: {Id}})" style="margin-top: 0.5rem;">
        Read More →
    </button>

    ---
    """").

render_tag(Bindings) ->
    Tag = maps:get(tag, Bindings),
    Selected = maps:get(selected, Bindings),
    Style =
        case Selected of
            true ->
                [
                    ~"background: #1976d2; color: white; padding: 4px 12px; ",
                    ~"margin: 0 4px; border: none; border-radius: 4px; cursor: pointer;"
                ];
            false ->
                [
                    ~"background: #f5f5f5; color: #333; padding: 4px 12px; ",
                    ~"margin: 0 4px; border: 1px solid #ddd; border-radius: 4px; cursor: pointer;"
                ]
        end,
    TagDisplay =
        case Tag of
            all -> ~"All";
            _ -> atom_to_binary(Tag)
        end,
    arizona_template:from_html(~"""
    <button onclick="arizona.sendEvent('filter_by_tag', \{tag: '{Tag}'})" style="{Style}">
        {TagDisplay}
    </button>
    """).

handle_event(~"view_post", #{~"id" := Id}, View) ->
    State = arizona_view:get_state(View),
    Post = get_post_by_id(Id),
    UpdatedState = arizona_stateful:merge_bindings(
        #{
            selected_post => Post,
            view_mode => detail
        },
        State
    ),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView};
handle_event(~"back_to_list", _Params, View) ->
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:merge_bindings(
        #{
            selected_post => undefined,
            view_mode => list
        },
        State
    ),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView};
handle_event(~"filter_by_tag", #{~"tag" := Tag}, View) ->
    State = arizona_view:get_state(View),
    TagAtom =
        case Tag of
            ~"all" -> all;
            _ -> binary_to_existing_atom(Tag)
        end,
    UpdatedState = arizona_stateful:put_binding(selected_tag, TagAtom, State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {[], UpdatedView}.

get_posts() ->
    [
        #{
            id => 1,
            title => ~"Getting Started with Arizona Framework",
            excerpt =>
                ~"Learn how to build reactive web applications with Arizona Framework and Erlang.",
            content =>
                arizona_template:from_markdown(~""""
                Arizona Framework brings the power of **Erlang/OTP** to web development with a React-like approach.

                ### Key Features

                - **LiveView**: Real-time updates without JavaScript
                - **Fault Tolerance**: Built on Erlang's proven supervision trees
                - **Hot Code Reloading**: Update code without stopping the server
                - **Distributed**: Scale across multiple nodes seamlessly

                ### Example: Counter Component

                ```erlang
                render(Bindings) ->
                    arizona_template:from_html(~"""
                    <div>
                        <h1>Count: \{arizona_template:get_binding(count, Bindings)}</h1>
                        <button onclick="arizona.sendEvent('increment')">+</button>
                        <button onclick="arizona.sendEvent('decrement')">-</button>
                    </div>
                    """).
                ```

                This creates a reactive counter that updates in real-time!
                """"),
            author => ~"Arizona Team",
            date => ~"2025-01-15",
            tags => [erlang, web, tutorial]
        },
        #{
            id => 2,
            title => ~"Markdown Templates in Arizona",
            excerpt => ~"Discover how to use markdown with dynamic content in Arizona templates.",
            content =>
                arizona_template:from_markdown(~"""""
                Arizona now supports **markdown templates** with full GitHub Flavored Markdown support!

                ### Why Markdown Templates?

                1. **Content-First**: Write content naturally in markdown
                2. **Dynamic**: Embed Erlang expressions anywhere
                3. **Rich Formatting**: Tables, code blocks, lists, and more
                4. **Type Safe**: Full compile-time checking

                ### Example: Blog Post

                ```erlang
                render_post(Bindings) ->
                    arizona_template:from_markdown(~""""
                    # \{arizona_template:get_binding(title, Bindings)}

                    By **\{arizona_template:get_binding(author, Bindings)}**

                    \{arizona_template:get_binding(content, Bindings)}

                    ### Comments (\{length(arizona_template:get_binding(comments, Bindings))})

                    \{arizona_template:render_list(fun(Comment) ->
                        arizona_template:from_markdown(~"""
                        #### \{maps:get(author, Comment)}
                        \{maps:get(text, Comment)}
                        """)
                    end, arizona_template:get_binding(comments, Bindings))}
                    """").
                ```

                Markdown templates make content management a breeze!
                """""),
            author => ~"Content Team",
            date => ~"2025-01-20",
            tags => [markdown, templates, content]
        },
        #{
            id => 3,
            title => ~"Building Real-time Applications",
            excerpt => ~"Learn how to build real-time features with Arizona's LiveView system.",
            content =>
                arizona_template:from_markdown(~""""
                Arizona's **LiveView** system enables real-time applications without complex JavaScript.

                ### Real-time Features Made Easy

                | Feature | Traditional | Arizona LiveView |
                |---------|-------------|------------------|
                | Chat | WebSocket + JS | Just Erlang |
                | Live Updates | Polling + AJAX | Automatic |
                | State Management | Redux/MobX | Built-in |
                | Error Handling | Try/Catch | Supervision Trees |

                ### Chat Example

                ```erlang
                handle_event(~"send_message", #\{~"text" := Text}, View) ->
                    State = arizona_view:get_state(View),
                    RoomId = arizona_stateful:get_binding(room_id, State),

                    % Broadcast to all users in room
                    arizona_pubsub:broadcast(
                        \{chat_room, RoomId},
                        \{new_message, #\{text => Text, user => get_current_user()}}
                    ),

                    \{[], View}.
                ```

                The system handles all the WebSocket plumbing for you!
                """"),
            author => ~"Engineering Team",
            date => ~"2025-01-22",
            tags => [realtime, liveview, erlang]
        }
    ].

get_post_by_id(Id) ->
    Posts = get_posts(),
    case lists:keyfind(Id, 2, [{P, maps:get(id, P)} || P <- Posts]) of
        {Post, _} -> Post;
        false -> undefined
    end.

filter_posts_by_tag(Posts, all) ->
    Posts;
filter_posts_by_tag(Posts, Tag) ->
    lists:filter(
        fun(#{tags := Tags}) ->
            lists:member(Tag, Tags)
        end,
        Posts
    ).
