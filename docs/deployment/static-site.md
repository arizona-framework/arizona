# Static Site Generation

- [generate/1](#generate1)
- [Route Paths](#route-paths)
- [Parallel Generation](#parallel-generation)
- [Output Directory](#output-directory)

## generate/1

Generate static HTML files from your Arizona routes using `arizona_static:generate/1`:

```erlang
Config = #{
    route_paths => #{
        ~"/" => #{},
        ~"/about" => #{},
        ~"/posts/:id" => #{parallel => true}
    },
    output_dir => ~"_site",
    base_url => ~"https://example.com",
    timeout => 30000
},
ok = arizona_static:generate(Config).
```

The generator starts a temporary server instance, fetches each route via HTTP, and writes the
resulting HTML to disk. A `sitemap.xml` is generated automatically, listing all generated pages with
the configured `base_url`.

For cases where you need to customize the temporary server's configuration (e.g., to provide
additional routes, middleware, or data sources), use `arizona_static:generate/2`:

```erlang
ok = arizona_static:generate(StaticConfig, ArizonaConfig).
```

The `ArizonaConfig` parameter accepts a full Arizona config map (the same type returned by
`arizona_config:get/0`), allowing you to control exactly how pages are rendered during generation.

The config map accepts the following options:

| Option | Type | Description |
| -------- | ------ | ------------- |
| `route_paths` | `#{path() => map()}` | Routes to generate, with per-route options |
| `output_dir` | `path()` | Directory where HTML files are written |
| `base_url` | `binary()` | Base URL used in `sitemap.xml` entries |
| `timeout` | `integer()` | Timeout in milliseconds for fetching each page |

## Route Paths

The `route_paths` map specifies which routes to generate. Keys are path patterns matching your
router definitions, and values are option maps:

```erlang
route_paths => #{
    ~"/" => #{},
    ~"/about" => #{},
    ~"/blog" => #{},
    ~"/blog/:slug" => #{parallel => true}
}
```

For routes with bindings (like `:slug`), the generator resolves them based on the data available
during the view's mount callback. Each binding value produces a separate HTML file. For example, if
the blog has three posts with slugs `hello-world`, `getting-started`, and `advanced-patterns`, the
generator will produce three HTML files under `blog/`.

Views can access route parameters using `arizona_request:get_bindings/1` to determine which content
to render:

```erlang
mount(#{} = _MountArg, Request) ->
    {Bindings, _Req} = arizona_request:get_bindings(Request),
    Slug = maps:get(slug, Bindings),
    Post = load_post(Slug),
    arizona_view:new(?MODULE, #{
        title => maps:get(title, Post),
        body => maps:get(body, Post)
    }, none).
```

## Parallel Generation

Enable parallel generation per route pattern for faster builds:

```erlang
route_paths => #{
    ~"/posts/:id" => #{parallel => true}
}
```

When `parallel => true`, multiple pages matching that pattern are generated concurrently. This is
particularly beneficial for content-heavy sites with many pages under a single route pattern, such
as blog posts, product pages, or documentation entries.

Sequential generation (the default when `parallel` is omitted or set to `false`) processes pages one
at a time. This is safer for routes where the generation process has side effects or shared resource
constraints, but slower for large numbers of independent pages.

Choose parallel generation for route patterns with many independent pages, and sequential generation
for routes that depend on shared state or external services with rate limits.

## Output Directory

The `output_dir` option specifies where generated HTML files are written. Each route produces an
HTML file named after the route path:

```text
_site/
  index.html                    <- /
  about.html                    <- /about
  blog.html                     <- /blog
  blog/
    hello-world.html            <- /blog/hello-world
    getting-started.html        <- /blog/getting-started
  sitemap.xml
```

The output directory is created if it does not exist. Existing files are overwritten on each
generation run, so the output always reflects the current state of your routes and content.

The `sitemap.xml` file is generated at the root of the output directory and includes an entry for
every generated page, using the `base_url` to construct fully qualified URLs. This file is ready to
be submitted to search engines for indexing.

The generated site is fully static HTML -- it can be deployed to any static file host (GitHub Pages,
Netlify, S3, Cloudflare Pages, a simple nginx server, etc.) without requiring an Erlang runtime in
production.

See also: [Routing](../server/routing.md), [Configuration](../server/configuration.md)
