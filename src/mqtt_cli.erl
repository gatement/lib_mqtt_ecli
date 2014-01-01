-module(mqtt_cli).
-include("mqtt_cli.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([
    mqtt_connect/2
    ]).
-vsn("0.1.0").

%% Implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

mqtt_connect(Socket, Params) ->
    Packet = mqtt_util:build_connect(Params),
    ok = gen_tcp:send(Socket, Packet),
    receive
        Data ->
            ok
    after
        3000 -> error
    end.
    

%% ===================================================================
%% Local Functions
%% ===================================================================


%% ===================================================================
%% Eunit Tests
%% ===================================================================

mqtt_connect_test() ->
    Socket = open_socket(),

    {ok, Username} = application:get_env(?APPNAME, test_mqtt_username),
    {ok, Password} = application:get_env(?APPNAME, test_mqtt_password),
    {ok, ClientId} = application:get_env(?APPNAME, test_mqtt_client_id),
    {ok, KeepAlive} = application:get_env(?APPNAME, test_mqtt_keep_alive),

    Params = #mqtt_connect_params {
        username = Username,
        password = Password,
        clean_session = true,
        keep_alive = KeepAlive,
        client_id = ClientId 
    },

    try
        ok = mqtt_connect(Socket, Params)
    after
        close_socket(Socket)
    end.


%% ===================================================================
%% Eunit Test Helpers
%% ===================================================================

open_socket() ->
    application:start(mqtt_cli),
    {ok, Server} = application:get_env(?APPNAME, test_mqtt_server),
    {ok, Port} = application:get_env(?APPNAME, test_mqtt_port),
    {ok, Socket} = gen_tcp:connect(Server, Port, [binary, {active, true}]),
    Socket.


close_socket(Socket) ->
    gen_tcp:close(Socket).
