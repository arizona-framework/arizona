-module(arizona_layout).
-include("arizona_stateless.hrl").
-export([render/1]).

-spec render(az:bindings()) -> az:template().
render(Bindings) ->
    ?html([
        ~"<!DOCTYPE html>",
        {html, [], [
            {head, [], [
                {meta, [{charset, ~"utf-8"}], []},
                {title, [], [maps:get(title, Bindings, ~"Arizona")]}
            ]},
            {body, [], [
                {nav, [], [
                    {a, [{href, ~"/"}, az_navigate], [~"Home"]},
                    ~" | ",
                    {a, [{href, ~"/about"}, az_navigate], [~"About"]},
                    ~" | ",
                    {a, [{href, ~"/datatable"}, az_navigate], [~"DataTable"]},
                    ~" | ",
                    {a, [{href, ~"/mixed"}, az_navigate], [~"Mixed"]},
                    ~" | ",
                    {a, [{href, ~"/chat"}, az_navigate], [~"Chat"]}
                ]},
                ?inner_content,
                {script, [{type, ~"module"}], [
                    ~"""
                import { hooks, connect } from '/priv/arizona.min.js';
                hooks.Tick = {
                    mounted() {
                        this.el.dataset.hookMounted = 'true';
                        this.pushEvent('tick_started', {});
                    },
                    updated() {
                        this.el.dataset.hookUpdated =
                            (parseInt(this.el.dataset.hookUpdated || '0', 10) + 1).toString();
                    },
                    destroyed() { window._tickDestroyed = true; },
                };
                connect('/ws');
                """
                ]}
            ]}
        ]}
    ]).
