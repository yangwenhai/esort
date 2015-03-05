-module(esort_proxy_app).

-behaviour(application).

-include("mslog.hrl").

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->

      %%设置日志目录
      {ok,LogLevel}=application:get_env(esort_proxy, log_level),
      ok=mslog:set(tool:to_integer(LogLevel), "./log/erlang_log/", esort_proxy),

      %%启动根监督树
      {ok, SupPid} = esort_proxy_sup:start_link(),

      %%启动客户端tcp连接监督树
      ok=start_connection_sup(),

      %%启动远程排序服务器连接的监督树
      ok=start_dispatch_sup(),

      {ok, SupPid}.


stop(_State) ->
    ok.



%%开启客户端的tcp 连接监控树
start_connection_sup() ->

    Ip = {0, 0, 0, 0},
    {ok, Port} = application:get_env(esort_proxy, local_port),
    Option=[binary,
           {packet,4},%%应用程序消息都从一个4字节长的头部开始
           {ip, Ip},{reuseaddr, true},{active, true},{backlog, 64}],

    case gen_tcp:listen(Port,Option) of
         {ok, LSock} -> 
    		{ok,_}=supervisor:start_child(
               		esort_proxy_sup,
               		{esort_proxy_connection_sup,
               		{esort_proxy_connection_sup, start_link, [LSock]},
                	transient, infinity, supervisor, [esort_proxy_connection_sup]}),

		 %%启动一个accept进程，等待客户端连接
    		esort_proxy_connection_sup:start_child(),
    		ok;
	{error, Reason} ->
                ?CRITICAL_MSG("esort_proxy_app err!listen err!port:~p reason:~p",[Port,Reason]),
                throw(esort_proxy_listen_fail)
   end.


%%开启与远程排序服务器的监控树
start_dispatch_sup() ->
    {ok,_}=supervisor:start_child(
               esort_proxy_sup,
               {esort_proxy_dipatch_sup,
                {esort_proxy_dipatch_sup, start_link, []},
                transient, infinity, supervisor, [esort_proxy_dipatch_sup]}),

    %%把和远程排序服务器的tcp连接池都启动起来
    esort_proxy_dipatch_sup:start_child(),

    ok.







