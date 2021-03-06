%% CONNECT PARAM
-record(mqtt_connect_params, {username, password, clean_session, keep_alive, client_id}).

%% MQTT_V3.1 MSG
-define(MQTT_MSG_CONNECT,     2#00010000).
-define(MQTT_MSG_CONNACK,     2#00100000).
-define(MQTT_MSG_PUBLISH,     2#00110000).
-define(MQTT_MSG_PUBACK,      2#01000000).
-define(MQTT_MSG_PUBREC,      2#01010000).
-define(MQTT_MSG_PUBREL,      2#01100000).
-define(MQTT_MSG_PUBCOMP,     2#01110000).
-define(MQTT_MSG_SUBSCRIBE,   2#10000000).
-define(MQTT_MSG_SUBACK,      2#10010000).
-define(MQTT_MSG_UNSUBSCRIBE, 2#10100000).
-define(MQTT_MSG_UNSUBACK,    2#10110000).
-define(MQTT_MSG_PINGREQ,     2#11000000).
-define(MQTT_MSG_PINGRESP,    2#11010000).
-define(MQTT_MSG_DISCONNECT,  2#11100000).

%% CONNACK CODE
-define(CONNACK_CODE_ACCEPTED,                    0).
-define(CONNACK_CODE_UNACCEPTABLE_PROTOCOL_VER,   1).
-define(CONNACK_CODE_IDENTIFIER_REJECTED,         2).
-define(CONNACK_CODE_SERVER_UNAVAILABLE,          3).
-define(CONNACK_CODE_BAD_USERNAME_OR_PASSWORD,    4).
-define(CONNACK_CODE_NOT_AUTHORIZED,              5).

%% FLAGS
-define(DUP0,        2#00000000).
-define(DUP1,        2#00001000).

-define(QOS0,        2#00000000).
-define(QOS1,        2#00000010).
-define(QOS2,        2#00000100).

-define(RETAIN0,     2#00000000).
-define(RETAIN1,     2#00000001).

