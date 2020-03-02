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

-module(emqx_rabbitmq_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-define(APP, emqx_rabbitmq).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_rabbitmq_sup:start_link(),
    ?APP:register_metrics(),
    ?APP:load(),
    emqx_rabbitmq_cfg:register(),
    {ok, Sup}.

stop(_State) ->
    emqx_rabbitmq:unload(),
    emqx_rabbitmq_cfg:unregister(),
    ok.

