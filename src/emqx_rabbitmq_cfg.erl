%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_rabbitmq_cfg).

-export([register/0, unregister/0]).

-define(APP, emqx_rabbitmq).

register() ->
  clique_config:load_schema([code:priv_dir(?APP)], ?APP),
  register_config().

unregister() ->
  unregister_config(),
  clique_config:unload_schema(?APP).

register_config() ->
  Keys = keys(),
  [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
  clique:register_config_whitelist(Keys, ?APP).

config_callback([_, _, "address"], Value0) ->
  {Host, Port} = parse_servers(Value0),
  {ok, Env} = application:get_env(?APP, server),
  Env1 = lists:keyreplace(host, 1, Env, {host, Host}),
  Env2 = lists:keyreplace(port, 1, Env1, {port, Port}),
  application:set_env(?APP, server, Env2),
  " successfully\n";
config_callback([_, _, "pool"], Value) ->
  {ok, Env} = application:get_env(?APP, server),
  application:set_env(?APP, server, lists:keyreplace(pool_size, 1, Env, {pool_size, Value})),
  " successfully\n";
config_callback([_, _, Key0], Value) ->
  Key = list_to_atom(Key0),
  {ok, Env} = application:get_env(?APP, server),
  application:set_env(?APP, server, lists:keyreplace(Key, 1, Env, {Key, Value})),
  " successfully\n".

unregister_config() ->
  Keys = keys(),
  [clique:unregister_config(Key) || Key <- Keys],
  clique:unregister_config_whitelist(Keys, ?APP).

keys() ->
  ["rabbitmq.address",
   "rabbitmq.username",
   "rabbitmq.password",
   "rabbitmq.heartbeat",
   "rabbitmq.connection_timeout",
   "rabbitmq.auto_reconnect",
   "rabbitmq.pool"].

parse_servers(Value) ->
  case string:tokens(Value, ":") of
    [Domain]       -> {Domain, 5672};
    [Domain, Port] -> {Domain, list_to_integer(Port)}
  end.
