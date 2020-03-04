-module(emqx_rabbitmq).

-include("emqx_rabbitmq.hrl").

-include_lib("emqx/include/emqx.hrl").

-import(emqx_rabbitmq_cli, [ensure_exchange/1, publish/3]).

-export([description/0]).
-export([on_message_publish/2]).

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}, ExchangeName) ->
  if
    Qos > 0 ->
      Username = case maps:find(username, Message#message.headers) of
        {ok, Value} -> Value;
        _ -> undefined
      end,
      Params = #{ client_id => Message#message.from
                , username => Username
                , topic => Message#message.topic
                , qos => Message#message.qos
                , retain => Retain
                , payload => Message#message.payload
                , publish_at => Message#message.timestamp
      },
      emqx_rabbitmq_cli:publish(ExchangeName, emqx_json:encode(Params), <<"message.publish">>);
    true ->
      true
  end,
  {ok, Message}.

description() -> "Bridge of RabbitMQ".
