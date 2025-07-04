-module(request_info_live_component).
-behaviour(arizona_live).
-export([mount/2, render/1]).

%% Component that displays arizona_request information for testing

mount(Req, Socket) ->
    Method = arizona_request:get_method(Req),
    Path = arizona_request:get_path(Req),
    {Params, Req1} = arizona_request:get_params(Req),
    {Bindings, _Req} = arizona_request:get_bindings(Req1),

    % Format params and bindings as strings for display
    ParamsText = format_params(Params),
    BindingsText = format_bindings(Bindings),

    % Put all bindings at once
    arizona_socket:put_bindings(
        #{
            method => Method,
            path => Path,
            params_text => ParamsText,
            bindings_text => BindingsText
        },
        Socket
    ).

render(Socket) ->
    arizona_html:render_live(~"""
    <div class="request-info">
        <h2>Request Information</h2>
        <p>Method: {arizona_socket:get_binding(method, Socket)}</p>
        <p>Path: {arizona_socket:get_binding(path, Socket)}</p>
        <p>Params: {arizona_socket:get_binding(params_text, Socket)}</p>
        <p>Bindings: {arizona_socket:get_binding(bindings_text, Socket)}</p>
    </div>
    """, Socket).

%% Helper functions for mount
format_params([]) ->
    ~"none";
format_params(Params) ->
    ParamsList = [iolist_to_binary([Key, ~"=", Value]) || {Key, Value} <- Params],
    iolist_to_binary(lists:join(~", ", ParamsList)).

format_bindings(Bindings) when map_size(Bindings) =:= 0 ->
    ~"none";
format_bindings(Bindings) ->
    BindingsList = [
        iolist_to_binary([atom_to_binary(Key), ~"=", Value])
     || {Key, Value} <- maps:to_list(Bindings)
    ],
    iolist_to_binary(lists:join(~", ", BindingsList)).
