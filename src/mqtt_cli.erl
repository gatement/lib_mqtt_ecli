-module(mqtt_cli).
-include("mqtt_cli.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([
    mqtt_connect/2,
    mqtt_disconnect/1,
    mqtt_publish/3,
    mqtt_subscribe/3,
    mqtt_unsubscribe/3
    ]).
-vsn("0.1.0").

-define(APPNAME, mqtt_cli).

%% Implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

mqtt_connect(Socket, Params) ->
    Packet = mqtt_util:build_connect(Params),
    ok = gen_tcp:send(Socket, Packet),
    receive
        {tcp, Socket, RawData} ->
            case RawData of
                <<2:4/integer, _Byte1L:4/integer, 2, _Reserved:8/integer, 0>> ->
                    ok;
                _ ->
                    error
            end
    after
        3000 -> error
    end.
    

mqtt_disconnect(Socket) ->
    Packet = mqtt_util:build_disconnect(),
    ok = gen_tcp:send(Socket, Packet).


%% only support QoS 0, no Retain
mqtt_publish(Socket, Topic, Payload) ->
    Packet = mqtt_util:build_publish(Topic, Payload),
    ok = gen_tcp:send(Socket, Packet).


%% only subscribe one topic at a single SUBSCRIBE request
%% only subscribe QoS 0 topic
mqtt_subscribe(Socket, Topic, MessageId) ->
    Packet = mqtt_util:build_subscribe(Topic, MessageId),
    ok = gen_tcp:send(Socket, Packet),
    receive
        {tcp, Socket, RawData} ->
            case RawData of
                %% todo: judge the SUBACK by MessageId and QoS
                <<9:4/integer, _Byte1L:4/integer, _/binary>> ->
                    ok;
                _ ->
                    error
            end
    after
        3000 -> error
    end.


%% only unsubscribe one topic at a single UNSUBSCRIBE request
mqtt_unsubscribe(Socket, Topic, MessageId) ->
    Packet = mqtt_util:build_unsubscribe(Topic, MessageId),
    ok = gen_tcp:send(Socket, Packet),
    receive
        {tcp, Socket, RawData} ->
            case RawData of
                <<11:4/integer, _Byte1L:4/integer, 2, MessageId:16/integer-big>> ->
                    ok;
                _ ->
                    error
            end
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

    try
        ok = connect_mqtt_broker(Socket)
    after
        close_socket(Socket)
    end.


mqtt_disconnect_test() ->
    Socket = open_socket(),

    try
        ok = connect_mqtt_broker(Socket),
        mqtt_disconnect(Socket),
        receive
            {tcp_closed, Socket} ->
                ok
        after 
            3000 ->
                exit(fail)
        end
    after
        close_socket(Socket)
    end.


mqtt_subscribe_test() ->
    Socket = open_socket(),
    Topic = "a/b",
    MessageId = 1,

    try
        ok = connect_mqtt_broker(Socket),
        ok = mqtt_subscribe(Socket, Topic, MessageId)
    after
        close_socket(Socket)
    end.


mqtt_unsubscribe_test() ->
    Socket = open_socket(),
    Topic = "a/b",
    MessageId = 1,

    try
        ok = connect_mqtt_broker(Socket),
        ok = mqtt_subscribe(Socket, Topic, MessageId),
        ok = mqtt_unsubscribe(Socket, Topic, MessageId)
    after
        close_socket(Socket)
    end.


%% ===================================================================
%% Eunit Test Helpers
%% ===================================================================

open_socket() ->
    application:start(?APPNAME),
    {ok, Server} = application:get_env(?APPNAME, test_mqtt_server),
    {ok, Port} = application:get_env(?APPNAME, test_mqtt_port),
    {ok, Socket} = gen_tcp:connect(Server, Port, [binary, {active, true}]),
    Socket.


connect_mqtt_broker(Socket) ->
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

    mqtt_connect(Socket, Params).


close_socket(Socket) ->
    gen_tcp:close(Socket).
