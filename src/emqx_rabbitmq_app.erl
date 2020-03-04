-module(emqx_rabbitmq_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-include("emqx_rabbitmq.hrl").

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
  {ok, ExchangeName} = application:get_env(?APP, exchange),
  emqx_rabbitmq_cli:ensure_exchange(ExchangeName),
  emqx:hook('message.publish', fun emqx_rabbitmq:on_message_publish/2, [ExchangeName]).

unload() ->
  emqx:unhook('message.publish', fun emqx_rabbitmq:on_message_publish/2).
