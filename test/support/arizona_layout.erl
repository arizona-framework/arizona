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
                    {a, [{href, ~"/chat/lobby"}, az_navigate], [~"Chat"]},
                    ~" | ",
                    {a, [{href, ~"/local"}, az_navigate], [~"Local"]},
                    ~" | ",
                    {a, [{href, ~"/local-nested"}, az_navigate], [~"Nested"]},
                    ~" | ",
                    {a, [{href, ~"/local-app"}, az_navigate], [~"App"]},
                    ~" | ",
                    {a, [{href, ~"/pip"}, az_navigate], [~"PiP"]},
                    ~" | ",
                    {a, [{href, ~"/transitions"}, az_navigate], [~"Transitions"]},
                    ~" | ",
                    {a, [{href, ~"/patch-demo/overview"}, az_navigate], [~"Patch"]}
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
                // A client-owned widget (arizona_nodiff_widget): it replaces the
                // server's markup with its own and keeps state in it.
                hooks.Widget = {
                    mounted() {
                        const log = (window._widget ||= { mounts: 0, destroys: 0, events: [] });
                        log.mounts += 1;
                        log.events.push(`mount:${this.el.dataset.kind}`);
                        const input = document.createElement('input');
                        input.className = 'widget-input';
                        const kind = document.createElement('span');
                        kind.className = 'widget-kind';
                        kind.textContent = this.el.dataset.kind;
                        this.el.replaceChildren(kind, input);
                    },
                    destroyed() {
                        const log = window._widget;
                        log.destroys += 1;
                        log.events.push(`destroy:${this.el.dataset.kind}:${this.el.isConnected}`);
                    },
                };
                connect('/ws');
                """
                ]}
            ]}
        ]}
    ]).
