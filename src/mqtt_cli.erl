-module(mqtt_cli).
-include("mqtt_cli.hrl").
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
    
    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
