%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% Define the default actions.
-module(emqx_rabbitmq_actions).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-define(RESOURCE_TYPE_RABBITMQ, 'rabbitmq').
-define(RESOURCE_CONFIG_SPEC, #{
            address => #{
                order => 1,
                type => string,
                required => true,
                default => <<"127.0.0.1:5672">>,
                title => #{en => <<"Server Address">>, zh => <<"服务器地址"/utf8>>},
                description => #{en => <<"Server Address">>, zh => <<"服务器地址"/utf8>>}
            },
            pool_size => #{
                order => 2,
                type => number,
                required => true,
                default => <<"8">>,
                title => #{en => <<"Pool Size">>, zh => <<"连接池大小"/utf8>>},
                description => #{en => <<"Pool Size">>, zh => <<"连接池大小"/utf8>>}
            },
            username => #{
                order => 3,
                type => string,
                required => false,
                default => <<"guest">>,
                title => #{en => <<"Username">>, zh => <<"用户名"/utf8>>},
                description => #{en => <<"Username">>, zh => <<"用户名"/utf8>>}
            },
            password => #{
                order => 4,
                type => string,
                required => false,
                default => <<"guest">>,
                title => #{en => <<"Password">>, zh => <<"密码"/utf8>>},
                description => #{en => <<"Password">>, zh => <<"密码"/utf8>>}
            },
            connect_timeout => #{
                order => 5,
                type => string,
                required => false,
                default => <<"5s">>,
                title => #{en => <<"Connect Timeout">>, zh => <<"连接超时时间"/utf8>>},
                description => #{en => <<"Connect Timeout">>, zh => <<"连接超时时间"/utf8>>}
            },
            exchange => #{
                order => 6,
                type => string,
                required => false,
                default => <<"/">>,
                title => #{en => <<"Exchange">>, zh => <<"虚拟主机"/utf8>>},
                description => #{en => <<"Exchange">>, zh => <<"虚拟主机"/utf8>>}
            },
            keepalive => #{
                order => 7,
                type => string,
                required => false,
                default => <<"30s">>,
                title => #{en => <<"Keepalive">>, zh => <<"心跳间隔"/utf8>>},
                description => #{en => <<"Keepalive">>, zh => <<"心跳间隔"/utf8>>}
            },
            reconnect_interval => #{
                order => 8,
                type => string,
                required => false,
                default => <<"2s">>,
                title => #{en => <<"Reconnect Interval">>, zh => <<"重连间隔"/utf8>>},
                description => #{en => <<"Reconnect Interval">>, zh => <<"重连间隔"/utf8>>}
            }
        }).

-define(ACTION_PARAM_RESOURCE, #{
            type => string,
            required => true,
            title => #{en => <<"Resource ID">>,
                       zh => <<"资源 ID"/utf8>>},
            description => #{en => <<"Bind a resource to this action">>,
                             zh => <<"给动作绑定一个资源"/utf8>>}
        }).

-define(ACTION_DATA_SPEC, #{
            '$resource' => ?ACTION_PARAM_RESOURCE
        }).

-define(JSON_REQ(URL, HEADERS, BODY), {(URL), (HEADERS), "application/json", (BODY)}).

-resource_type(#{name => ?RESOURCE_TYPE_RABBITMQ,
                 create => on_resource_create,
                 status => on_get_resource_status,
                 destroy => on_resource_destroy,
                 params => ?RESOURCE_CONFIG_SPEC,
                 title => #{en => <<"RabbitMQ">>,
                            zh => <<"RabbitMQ"/utf8>>},
                 description => #{en => <<"RabbitMQ">>,
                                  zh => <<"RabbitMQ"/utf8>>}
                }).

-rule_action(#{name => data_to_rabbitmq,
               for => '$any',
               create => on_action_create_data_to_rabbitmq,
               params => ?ACTION_DATA_SPEC,
               types => [?RESOURCE_TYPE_RABBITMQ],
               title => #{en => <<"Data to RabbitMQ">>,
                          zh => <<"发送数据到 RabbitMQ 服务"/utf8>>},
               description => #{en => <<"Forward Messages to RabbitMQ">>,
                                zh => <<"将数据转发给 RabbitMQ 服务"/utf8>>}
              }).

-type(action_fun() :: fun((Data :: map(), Envs :: map()) -> Result :: any())).

-type(url() :: binary()).

-export_type([action_fun/0]).

-export([ on_resource_create/2
        , on_get_resource_status/2
        , on_resource_destroy/2
        ]).

-export([ on_action_create_data_to_rabbitmq/2
        ]).

on_resource_create(ResId, Params) ->
    ?LOG(info, "Initiating Resource ~p, ResId: ~p", [?RESOURCE_TYPE_RABBITMQ, ResId]),
    {ok, _} = application:ensure_all_started(ecpool),
    PoolName = pool_name(ResId),
    Options = options(Params, PoolName),
    start_resource(ResId, PoolName, Options),
    case test_resource_status(PoolName) of
        true -> ok;
        false ->
            on_resource_destroy(ResId, #{<<"pool">> => PoolName}),
            error({{?RESOURCE_TYPE_RABBITMQ, ResId}, connection_failed})
    end,
    #{<<"pool">> => PoolName}.

start_resource(ResId, PoolName, Options) ->
    case ecpool:start_sup_pool(PoolName, ?MODULE, Options) of
        {ok, _} ->
            ?LOG(info, "Initiated Resource ~p Successfully, ResId: ~p", [?RESOURCE_TYPE_RABBITMQ, ResId]);
        {error, {already_started, _Pid}} ->
            on_resource_destroy(ResId, #{<<"pool">> => PoolName}),
            start_resource(ResId, PoolName, Options);
        {error, Reason} ->
            ?LOG(error, "Initiate Resource ~p failed, ResId: ~p, ~p", [?RESOURCE_TYPE_RABBITMQ, ResId, Reason]),
            on_resource_destroy(ResId, #{<<"pool">> => PoolName}),
            error({{?RESOURCE_TYPE_RABBITMQ, ResId}, create_failed})
    end.

test_resource_status(PoolName) ->
    IsConnected = fun(Worker) ->
                          case ecpool_worker:client(Worker) of
                              {ok, Bridge} ->
                                  try emqx_bridge_worker:status(Bridge) of
                                      connected -> true;
                                      _ -> false
                                  catch _Error:_Reason ->
                                          false
                                  end;
                              {error, _} ->
                                  false
                          end
                  end,
    Status = [IsConnected(Worker) || {_WorkerName, Worker} <- ecpool:workers(PoolName)],
    lists:any(fun(St) -> St =:= true end, Status).

-spec(on_get_resource_status(ResId::binary(), Params::map()) -> Status::map()).
on_get_resource_status(_ResId, #{<<"pool">> := PoolName}) ->
    IsAlive = test_resource_status(PoolName),
    #{is_alive => IsAlive}.

on_resource_destroy(ResId, #{<<"pool">> := PoolName}) ->
    ?LOG(info, "Destroying Resource ~p, ResId: ~p", [?RESOURCE_TYPE_RABBITMQ, ResId]),
        case ecpool:stop_sup_pool(PoolName) of
            ok ->
                ?LOG(info, "Destroyed Resource ~p Successfully, ResId: ~p", [?RESOURCE_TYPE_RABBITMQ, ResId]);
            {error, Reason} ->
                ?LOG(error, "Destroy Resource ~p failed, ResId: ~p, ~p", [?RESOURCE_TYPE_RABBITMQ, ResId, Reason]),
                error({{?RESOURCE_TYPE_RABBITMQ, ResId}, destroy_failed})
        end.

on_action_create_data_to_rabbitmq(_Id, #{<<"pool">> := PoolName}) ->
    ?LOG(info, "Initiating Action ~p.", [?FUNCTION_NAME]),
    fun(Msg, _Env = #{id := Id, from := From, flags := Flags,
                      topic := Topic, timestamp := TimeStamp}) ->
            BrokerMsg = #message{id = Id,
                                 qos = 1,
                                 from = From,
                                 flags = Flags,
                                 topic = Topic,
                                 payload = jsx:encode(Msg),
                                 timestamp = TimeStamp},
            ecpool:with_client(PoolName, fun(BridgePid) ->
                                             BridgePid ! {dispatch, rule_engine, BrokerMsg}
                                         end)
    end.
