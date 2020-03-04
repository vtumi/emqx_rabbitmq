-module(emqx_rabbitmq).

-include("emqx_rabbitmq.hrl").

-include_lib("emqx/include/emqx.hrl").

-import(emqx_rabbitmq_cli, [ensure_exchange/1, publish/3]).

-export([description/0]).
-export([on_message_publish/2]).

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message = #message{id = Id, topic = Topic, qos = Qos, payload = Payload, timestamp = Timestamp, headers = #{username := Username, peername := {Peerhost, _}}, flags = #{retain := Retain}}, ExchangeName) ->
  if
    Qos > 0 ->
      Params = #{ mid => emqx_guid:to_hexstr(Id)
                , topic => Topic
                , sender => Username
                , node => atom_to_binary(node(), utf8)
                , ipaddr => iolist_to_binary(inet_parse:ntoa(Peerhost))
                , qos => Qos
                , retain => format_retain(Retain)
                , payload => Payload
                , create_at => format_timestamp(Timestamp)
      },
      emqx_rabbitmq_cli:publish(ExchangeName, emqx_json:encode(Params), <<"message.publish">>);
    true ->
      true
  end,
  {ok, Message}.

format_retain(Retain) ->
  case Retain of
    true -> 1;
    false -> 0
  end.

format_timestamp(TS) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(TS),
  lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])).

description() -> "Bridge of RabbitMQ".
