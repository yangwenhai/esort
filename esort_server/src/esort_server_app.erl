-module(esort_server_app).

-behaviour(application).

-include("mslog.hrl").

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->

      %%设置日志目录
      {ok,LogLevel}=application:get_env(esort_server, log_level),
      ok=mslog:set(tool:to_integer(LogLevel), "./log/erlang_log/", esort_server),

      %%启动根监督树
      {ok, SupPid} = esort_server_sup:start_link(),
     
      %%启动客户端tcp连接监督树
      ok=start_connection_sup(),
       
      {ok, SupPid}.  


stop(_State) ->
    ok.

%%开启客户端的tcp 连接监控树
start_connection_sup() ->

    Ip = {0, 0, 0, 0},
    {ok, Port} = application:get_env(esort_server, port),
    Option=[binary,{ip, Ip},{reuseaddr, true},
           {active, true},{packet, 4},{backlog, 256}],

    case gen_tcp:listen(Port,Option) of 
    	 {ok, LSock} ->
		  {ok,_}=supervisor:start_child(
               		  	esort_server_sup,
               			{esort_server_connection_sup,
               			{esort_server_connection_sup, start_link, [LSock]},
                		transient, infinity, supervisor, [esort_server_connection_sup]}) ,
		  %%启动一个accept进程，等待客户端连接
    		  {ok,_}=esort_server_connection_sup:start_child(),
		  ok;

	 {error, Reason} ->
		
		?CRITICAL_MSG("esort_proxy_app err!listen err!port:~p reason:~p",[Port,Reason]),
		throw(esort_server_listen_fail)  		 		
    end.

