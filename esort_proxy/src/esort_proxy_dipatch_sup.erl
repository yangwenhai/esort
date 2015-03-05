-module(esort_proxy_dipatch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    case supervisor:start_child(?SERVER, []) of
         {ok,Child}->
                {ok,Child};
         {ok,Child,_} ->
                {ok,Child};
         _ ->
                {err,other}
    end.
    

init([]) ->

     %%根据配置生成连接池   
     {ok,Count} = application:get_env(esort_proxy, network_process_num),

    %%根据配置个数循环创建
     F=fun(Index,Child)->
           Id=lists:concat([esort_proxy_dipatch_,Index]),
           T={tool:to_atom(Id), {esort_proxy_dipatch, start_link, [Index]},
             temporary, brutal_kill, worker, [esort_proxy_dipatch]},
           NewChild=[T|Child],
           {ok,NewChild}
       end,
    {ok, Children}=tool:for(1,Count,F,[]),

    Restart = {one_for_all, 10, 10},
    {ok, {Restart, Children}}.

