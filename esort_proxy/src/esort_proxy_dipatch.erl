-module(esort_proxy_dipatch).

-behaviour(gen_server).

-include("mslog.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {remote,index}).


start_link(Index) ->
    {ok,Pid}=gen_server:start_link(?MODULE, [Index], []),
    {ok,Pid}.

init([Index]) ->
    {ok, #state{index=Index}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%客户端的请求通过这个接口中转发给远程排序服务器
handle_cast(Request, State) ->
    ?INFO_MSG("dispatch handle_cast!receive from child,start sending to remote:~p ~n",[Request]),
    Remote=State#state.remote,
    case gen_tcp:send(Remote, Request) of
        ok ->
            ?INFO_MSG("send to remote ok!~n"),
    {noreply, State}
    end.

%%初始化的时候连接到远程服务器
handle_info(timeout, #state{remote=undefined}=State) ->

    {ok, RemoteAddr} = application:get_env(esort_proxy, remote_addr),
    {ok, RemotePort} = application:get_env(esort_proxy, remote_port),
    {ok, Addr} = inet:getaddr(RemoteAddr, inet),
    Option=[binary,{active, true},{packet, 4}],
    
    case gen_tcp:connect(Addr, RemotePort, Option ) of
         {ok, RemoteSocket} ->
	    ReName=lists:concat([esort_sc_,State#state.index]),
	    register(tool:to_atom(ReName), self()),
	    {ok, {PeerAddress, PeerPort}} = inet:peername(RemoteSocket),
	    ?INFO_MSG("dspatch connected to remote:~p port:~p ~n",[PeerAddress,PeerPort]),
	    {noreply, State#state{remote=RemoteSocket}};
          _ ->
	    ?INFO_MSG("dispatch fail to connect to remote!~n"),
	    throw(dispatch_fail_to_connect_to_remote)
   end;

%%接收服务器端消息，我们已经在esort_proxy_app里设置了{packet,4}，这里收到的Request就是一个完整包体
handle_info({tcp, Socket, Request}, #state{remote=Socket} = State) ->
     ?INFO_MSG("dspatch receive from remote,start sending to child  ~p ~n",[Request]),
     {ok,[DecodeRet],_Rest}=rfc4627:decode(Request),

     %%取出这个消息里的pid，将消息中转给这个pid的进程
     {ok,Pidval} = rfc4627:get_field(DecodeRet, "Pid"),
     NewDecod=tool:delFromJson(DecodeRet,"Pid"),
     NewEncod=rfc4627:encode([NewDecod]),
     gen_server:cast(list_to_atom(Pidval),NewEncod),
     {noreply, State};

%% send by OPT timeout
handle_info(timeout, #state{remote=Socket} = State) when is_port(Socket) ->
    {stop, timeout, State};

handle_info({'EXIT', _, Reason}, State) ->
    ?INFO_MSG("exit:~p~n", [Reason]),
    {stop, normal, State};

handle_info({tcp_closed, Reason}, State) ->
    ?INFO_MSG("tcp_closed ~p ~n",[Reason]),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    ?INFO_MSG("tcp_error ~p ~n",[Reason]),
    {stop, Reason, State}.

terminate(_Reason, #state{remote=Remote}) ->
    ?INFO_MSG(" dspatch enter terminate ~p ~n",[_Reason]),
    case is_port(Remote) of
        true -> gen_tcp:close(Remote);
        false -> ok
    end.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

