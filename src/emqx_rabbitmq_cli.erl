-module(emqx_rabbitmq_cli).

-behaviour(ecpool_worker).

-include("emqx_rabbitmq.hrl").
-include("../../amqp_client/include/amqp_client.hrl").

-export([connect/1]).
-export([ensure_exchange/1, publish/3]).

connect(Options) ->
  Params = #amqp_params_network{
    host = proplists:get_value(host, Options),
    port = proplists:get_value(port, Options),
    username = proplists:get_value(username, Options),
    password = proplists:get_value(password, Options),
    heartbeat = proplists:get_value(heartbeat, Options)
  },
  {ok, C} = amqp_connection:start(Params),
  {ok, C}.

ensure_exchange(ExchangeName) ->
  ecpool:with_client(?APP, fun(C) -> ensure_exchange(ExchangeName, C) end).

ensure_exchange(ExchangeName, Conn) ->
  {ok, Channel} = amqp_connection:open_channel(Conn),
  Declare = #'exchange.declare'{exchange = ExchangeName, durable = true},
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
  amqp_channel:close(Channel).

publish(ExchangeName, Payload, RoutingKey) ->
  ecpool:with_client(?APP, fun(C) -> publish(ExchangeName, Payload, RoutingKey, C) end).

publish(ExchangeName, Payload, RoutingKey, Conn) ->
  {ok, Channel} = amqp_connection:open_channel(Conn),
  Publish = #'basic.publish'{exchange = ExchangeName, routing_key = RoutingKey},
  Props = #'P_basic'{delivery_mode = 2},
  Msg = #amqp_msg{props = Props, payload = Payload},
  amqp_channel:cast(Channel, Publish, Msg),
  amqp_channel:close(Channel).
