-module(mqtt_util).
-include("mqtt_cli.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([
    build_connect/1
    ]).
-vsn("0.1.0").

%% Implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

build_connect(Params) ->
    #mqtt_connect_params {
        keep_alive = KeepAlive,
        client_id = ClientId
    } = Params,

	%% Connect flags.
	%% bit     |7    |6        |5           |4    3   |2         |1             |0
	%% byte 1  |User |Password |Will Retain |Will QoS |Will Flag |Clean Session |Reserved
	ConnectFlags = 2#00000000,
	VariableHeader = build_connect_variable_header(KeepAlive, ConnectFlags),

	Payload = build_connect_payload(ClientId),

	Length = erlang:size(VariableHeader) + erlang:size(Payload),
	FixedHeader = build_fixed_header(?MQTT_MSG_CONNECT, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload]).

%% ===================================================================
%% Local Functions
%% ===================================================================

%% Fixed header.
%% bit     |7  6  5  4   |3        |2    1    |0
%% byte 1  |Message Type |DUP flag |QoS level |RETAIN
%% byte 2  |Remaining Length
build_fixed_header(MsgType, Dup, Qos, Retain, Length) ->
	Header = MsgType bor Dup bor Qos bor Retain,
	RemainingLength = build_remaining_length(Length, []),
	[Header|RemainingLength].


build_connect_variable_header(KeepAliveTimer, ConnectFlags) ->
	ProtocalName = [0, 6, <<"MQIsdp">>],
	ProtocalVer = 3,

	KeepAliveTimerH = KeepAliveTimer div 256,
	KeepAliveTimerL = KeepAliveTimer rem 256,

	erlang:list_to_binary([ProtocalName, ProtocalVer, ConnectFlags, KeepAliveTimerH, KeepAliveTimerL]).


build_connect_payload(ClientId) ->
    ClientId.


build_remaining_length(0, Result) ->
	lists:reverse(Result);
build_remaining_length(Length, Result) ->
	Digit0 = Length rem 128,
	X = Length div 128,
	Digit = if
		X > 0 ->
			Digit0 bor 16#80;
		true ->
			Digit0
	end,
	build_remaining_length(X, [Digit|Result]).


%% ===================================================================
%% Eunit Tests
%% ===================================================================

build_remaining_length_test_() ->
    [
        ?_assert(build_remaining_length(1, []) =:= [1]),
        ?_assert(build_remaining_length(255, []) =:= [1, 255])
    ].
