-module(emqx_rabbitmq_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-include("emqx_rabbitmq.hrl").

-define(EXCHANGE_NAME, <<"mqtt.events">>).

-export([start/2, prep_stop/1, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = emqx_rabbitmq_sup:start_link(),
  register_metrics(),
  load(application:get_all_env()),
  emqx_rabbitmq_cfg:register(),
  {ok, Sup}.

prep_stop(State) ->
  unload(),
  emqx_rabbitmq_cfg:unregister(),
  State.

stop(_State) ->
  ok.

register_metrics() ->
  [emqx_metrics:new(MetricName) || MetricName <- ['rabbitmq.message.publish']].

load(_Env) ->
  emqx_rabbitmq_cli:ensure_exchange(?EXCHANGE_NAME),
  emqx:hook('message.publish', fun emqx_rabbitmq:on_message_publish/2, [?EXCHANGE_NAME]).

unload() ->
  emqx:unhook('message.publish', fun emqx_rabbitmq:on_message_publish/2).
