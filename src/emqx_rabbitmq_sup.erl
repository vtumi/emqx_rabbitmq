-module(emqx_rabbitmq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("emqx_rabbitmq.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  application:set_env(amqp_client, prefer_ipv6, false),
  {ok, Server} = application:get_env(?APP, server),
  PoolSpec = ecpool:pool_spec(?APP, ?APP, emqx_rabbitmq_cli, Server),
  {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.
