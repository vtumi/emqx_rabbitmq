-module(emqx_rabbitmq).

-include_lib("emqx/include/emqx.hrl").

-export([on_message_publish/2
]).

-import(emqx_rabbitmq_cli, [ensure_exchange/1, publish/3]).

-include("emqx_rabbitmq.hrl").

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};
on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}, ExchangeName) ->
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
  emqx_rabbitmq_cli:publish(ExchangeName, emqx_json:encode(Params), <<"message.publish">>),
  {ok, Message}.

description() -> "Bridge of RabbitMQ".
