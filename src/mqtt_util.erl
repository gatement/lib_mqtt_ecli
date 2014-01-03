-module(mqtt_util).
-include("mqtt_cli.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([
    build_connect/1,
    build_disconnect/0,
    build_publish/2,
    build_subscribe/2,
    build_unsubscribe/2,
    extract_fixed_header/2,
    extract_publish_topic/1,
    extract_publish_payload/1,
    get_message_type/1
    ]).
-vsn("0.1.0").

%% Implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

%% Do not support Will
build_connect(Params) ->
    #mqtt_connect_params {
        username = Username,
        password = Password,
        clean_session = CleanSession,
        keep_alive = KeepAlive,
        client_id = ClientId
    } = Params,

	%% Connect flags.
	%% bit     |7        |6        |5           |4    3   |2         |1             |0
	%% byte 1  |Username |Password |Will Retain |Will QoS |Will Flag |Clean Session |Reserved
	ConnectFlags0 = 2#00000000,
    ConnectFlags1 = case Username of
        [] ->
            ConnectFlags0;
        _ ->
            ConnectFlags0 bor 2#10000000
    end,
    ConnectFlags2 = case Password of
        [] ->
            ConnectFlags1;
        _ ->
            ConnectFlags1 bor 2#01000000
    end,
    ConnectFlags3 = case CleanSession of
        false ->
            ConnectFlags2;
        _ ->
            ConnectFlags2 bor 2#00000010
    end,

	VariableHeader = build_connect_variable_header(KeepAlive, ConnectFlags3),
	Payload = build_connect_payload(Username, Password, ClientId),

	Length = erlang:size(VariableHeader) + erlang:size(Payload),
	FixedHeader = build_fixed_header(?MQTT_MSG_CONNECT, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload]).


build_disconnect() ->
    FixedHeader = [?MQTT_MSG_DISCONNECT, 0],
	erlang:list_to_binary([FixedHeader]).


%% only support QoS 0, no Retain
build_publish(Topic, Payload) ->
	VariableHeader = build_publish_variable_header(Topic),
	Payload2 = erlang:list_to_binary([Payload]),

	Length = erlang:size(VariableHeader) + erlang:size(Payload2),
	FixedHeader = build_fixed_header(?MQTT_MSG_PUBLISH, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload2]).


%% only subscribe one topic at a single SUBSCRIBE request
%% only subscribe QoS 0 topic
build_subscribe(Topic, MessageId) ->
	VariableHeader = build_subscribe_variable_header(MessageId),
    Payload = build_subscribe_payload(Topic),

	Length = erlang:size(VariableHeader) + erlang:size(Payload),
	FixedHeader = build_fixed_header(?MQTT_MSG_SUBSCRIBE, ?DUP0, ?QOS1, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload]).


%% only unsubscribe one topic at a single UNSUBSCRIBE request
build_unsubscribe(Topic, MessageId) ->
	VariableHeader = build_unsubscribe_variable_header(MessageId),
    Payload = build_unsubscribe_payload(Topic),

	Length = erlang:size(VariableHeader) + erlang:size(Payload),
	FixedHeader = build_fixed_header(?MQTT_MSG_UNSUBSCRIBE, ?DUP0, ?QOS1, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload]).


%% return {FixedHeader, RestData}
extract_fixed_header(<<>>, Acc) ->
    FixedHeader = erlang:list_to_binary(lists:reverse(Acc)),
    {FixedHeader, <<>>};
extract_fixed_header(Packet, []) ->
    <<Byte1:8/integer, Rest/binary>> = Packet,
    extract_fixed_header(Rest, [Byte1]); 
extract_fixed_header(Packet, Acc) ->
    <<Byte1:8/integer, Rest/binary>> = Packet,
    if
        Byte1 > 127 ->
            extract_fixed_header(Rest, [Byte1 | Acc]); 
        true ->
            FixedHeader = erlang:list_to_binary(lists:reverse([Byte1 | Acc])),
            {FixedHeader, Rest}
    end.


%% return {Topic, RestData}
extract_publish_topic(Packet) ->
    {FixedHeader, Rest} = extract_fixed_header(Packet, []),
    case get_message_type(FixedHeader) of
        ?MQTT_MSG_PUBLISH bsr 4 ->
            <<TopicLen:16/integer-big, Rest2/binary>> = Rest,
            <<Topic0:TopicLen/binary, Rest3/binary>> = Rest2,
            {erlang:binary_to_list(Topic0), Rest3};
        _ ->
            error
    end.


%% Only support QoS 0
extract_publish_payload(Packet) ->
    {_Topic, Rest} = extract_publish_topic(Packet),
    Rest.


get_message_type(Packet) ->
    <<MsgType:4/integer, _Byte1L:4/integer, _/binary>> = Packet,
    MsgType.


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
	[Header | RemainingLength].


build_connect_variable_header(KeepAlive, ConnectFlags) ->
	ProtocalName = [0, 6, <<"MQIsdp">>],
	ProtocalVer = 3,
	KeepAliveData = integer_to_2b(KeepAlive),
    erlang:list_to_binary([ProtocalName, ProtocalVer, ConnectFlags, KeepAliveData]).


build_publish_variable_header(Topic) ->
    TopicData = encode_string(Topic),
    erlang:list_to_binary([TopicData]).


build_subscribe_variable_header(MessageId) ->
    MessageIdData = integer_to_2b(MessageId),
    erlang:list_to_binary([MessageIdData]).


build_unsubscribe_variable_header(MessageId) ->
    MessageIdData = integer_to_2b(MessageId),
    erlang:list_to_binary([MessageIdData]).


%% Do not support Will
build_connect_payload(Username, Password, ClientId) ->
    Result0 = [],
    Result1 = case Password of
        [] ->
            Result0;
        _ ->
            PasswordData = encode_string(Password),
            [PasswordData | Result0]
    end,
    Result2 = case Username of
        [] ->
            Result1;
        _ ->
            UsernameData = encode_string(Username),
            [UsernameData | Result1]
    end,
    
    ClientIdData = encode_string(ClientId),
    Result3 = [ClientIdData | Result2],

    erlang:list_to_binary(Result3).


%% only subscribe one topic at a single SUBSCRIBE request
%% only subscribe QoS 0 topic
build_subscribe_payload(Topic) ->
    TopicData = encode_string(Topic),
    erlang:list_to_binary([TopicData, ?QOS0]).


%% only unsubscribe one topic at a single UNSUBSCRIBE request
build_unsubscribe_payload(Topic) ->
    TopicData = encode_string(Topic),
    erlang:list_to_binary([TopicData]).


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


integer_to_2b(I) ->
    B1 = I band 16#FF,
    B2 = (I band 16#FF00) bsr 8,
    [B2, B1].


integer_to_4b(I) ->
    B1 = I band 16#FF,
    B2 = (I band 16#FF00) bsr 8,
    B3 = (I band 16#FF0000) bsr 16,
    B4 = (I band 16#FF000000) bsr 24,
    [B4, B3, B2, B1].


encode_string(Content) ->
    ContentBin = unicode:characters_to_binary(Content, latin1),
    ContentBinLen = integer_to_2b(erlang:size(ContentBin)),
    [ContentBinLen, ContentBin].


%% ===================================================================
%% Eunit Tests
%% ===================================================================

build_connect_test() ->
    Params = #mqtt_connect_params {
        username = "guest",
        password = "123456",
        clean_session = true,
        keep_alive = 600,
        client_id = "johnson"
    },
    Actual = build_connect(Params),

    Expected0 = [16#10, 36, 16#00, 16#06, "MQIsdp", 3, 16#C2, 16#02, 16#58, 16#00, 16#07, "johnson", 16#00, 16#05, "guest", 16#00, 16#06, "123456"],
    Expected1 = erlang:list_to_binary(Expected0),

	Expected1 = Actual.


build_publish_test() ->
    Topic = "a/b",
    Payload = 123,
    Actual = build_publish(Topic, Payload),

    Expected0 = [16#30, 6, 0, 3, 16#61, 16#2F, 16#62, 123],
    Expected1 = erlang:list_to_binary(Expected0),

	Expected1 = Actual.


build_subscribe_test() ->
    Topic = "a/b",
    MessageId = 1,
    Actual = build_subscribe(Topic, MessageId),

    Expected0 = [16#82, 8, 0, 1, 0, 3, 16#61, 16#2F, 16#62, 0],
    Expected1 = erlang:list_to_binary(Expected0),

	Expected1 = Actual.


build_unsubscribe_test() ->
    Topic = "a/b",
    MessageId = 1,
    Actual = build_unsubscribe(Topic, MessageId),

    Expected0 = [16#A2, 7, 0, 1, 0, 3, 16#61, 16#2F, 16#62],
    Expected1 = erlang:list_to_binary(Expected0),

	Expected1 = Actual.

build_remaining_length_test_() ->
    [
        ?_assert(build_remaining_length(0, []) =:= []),
        ?_assert(build_remaining_length(1, []) =:= [1]),
        ?_assert(build_remaining_length(255, []) =:= [255, 1]),
        ?_assert(build_remaining_length(321, []) =:= [193, 2]),
        ?_assert(build_remaining_length(127, []) =:= [16#7F]),
        ?_assert(build_remaining_length(128, []) =:= [16#80, 16#01]),
        ?_assert(build_remaining_length(16383, []) =:= [16#FF, 16#7F]),
        ?_assert(build_remaining_length(16384, []) =:= [16#80, 16#80, 16#01]),
        ?_assert(build_remaining_length(2097151, []) =:= [16#FF, 16#FF, 16#7F]),
        ?_assert(build_remaining_length(2097152, []) =:= [16#80, 16#80, 16#80, 16#01]),
        ?_assert(build_remaining_length(268435455, []) =:= [16#FF, 16#FF, 16#FF, 16#7F])
    ]. 


integer_to_2b_test_() ->
    [
        ?_assert(integer_to_2b(10) =:= [0, 16#0A]),
        ?_assert(integer_to_2b(10000) =:= [16#27, 16#10]),
        ?_assert(integer_to_2b(11206964) =:= [16#01, 16#34]) %% lost high bytes
    ].


integer_to_4b_test_() ->
    [
        ?_assert(integer_to_4b(10) =:= [0, 0, 0, 16#0A]),
        ?_assert(integer_to_4b(10000) =:= [0, 0, 16#27, 16#10]),
        ?_assert(integer_to_4b(11206964) =:= [0, 16#AB, 16#01, 16#34]),
        ?_assert(integer_to_4b(3759289430) =:= [16#E0, 16#12, 16#34, 16#56]),
        ?_assert(integer_to_4b(660889285718) =:= [16#E0, 16#12, 16#34, 16#56]) %% lost high bytes
    ].


encode_string_test_() ->
    [
        ?_assert(encode_string("") =:= [[16#00, 16#00], <<"">>]),
        ?_assert(encode_string("a") =:= [[16#00, 16#01], <<"a">>]),
        ?_assert(encode_string("abc") =:= [[16#00, 16#03], <<"abc">>])
    ].


get_message_type_test_() ->
    [
        ?_assert(get_message_type(<<16#12>>) =:= 1),
        ?_assert(get_message_type(<<16#31>>) =:= 3),
        ?_assert(get_message_type(<<16#12, 1, 2, 3>>) =:= 1),
        ?_assert(get_message_type(<<16#31, 1, 2, 3>>) =:= 3)
    ].


extract_fixed_header_test() ->
    [
        ?_assert(extract_fixed_header(<<>>, []) =:= {<<>>, <<>>}),
        ?_assert(extract_fixed_header(<<16#FF>>, []) =:= {<<16#FF>>, <<>>}),
        ?_assert(extract_fixed_header(<<16#01, 16#02, 16#03, 16#04>>, []) =:= {<<16#01, 16#02>>, <<16#03, 16#04>>}),
        ?_assert(extract_fixed_header(<<16#01, 16#F2, 16#03, 16#04>>, []) =:= {<<16#01, 16#F2, 16#03>>, <<16#04>>}),
        ?_assert(extract_fixed_header(<<16#01, 16#F2, 16#F3, 16#04>>, []) =:= {<<16#01, 16#F2, 16#F3, 16#04>>, <<>>})
    ].


extract_publish_topic_test_() ->
    [
        ?_assert(extract_publish_topic(<<16#30, 3, 0, 1, $a>>) =:= {"a", <<>>}),
        ?_assert(extract_publish_topic(<<16#30, 5, 0, 1, $a, 1, 2>>) =:= {"a", <<1, 2>>}),
        ?_assert(extract_publish_topic(<<16#30, 6, 0, 2, $a, $b, 1, 2>>) =:= {"ab", <<1, 2>>})
    ].


extract_publish_payload_test_() ->
    [
        ?_assert(extract_publish_payload(<<16#30, 3, 0, 1, $a>>) =:= <<>>),
        ?_assert(extract_publish_payload(<<16#30, 5, 0, 1, $a, 1, 2>>) =:= <<1, 2>>),
        ?_assert(extract_publish_payload(<<16#30, 6, 0, 2, $a, $b, 1, 2>>) =:= <<1, 2>>)
    ].
