-module(esort_server_connection).

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

-record(state, {lsock, socket}).

-define(TIMEOUT, 1000 * 60 * 5).

%%返回值模版
-define(RET_OK, {obj,[{"Ret",<<"ok">>},{"Val",<<>>} ,{"Method",<<>>}]} ).
-define(RET_ERR,{obj,[{"Ret",<<"err">>},{"Msg",<<>>},{"Method",<<>>}]} ).


start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock=LSock}, 0}.%%0表示立即超时，触发下面的handle_info(timeout

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

%%accept一个连接,这个连接是来自esort_proxy的
handle_info(timeout, #state{lsock=LSock, socket=undefined}=State) ->

    {ok, Socket} = gen_tcp:accept(LSock),

    %%启一个新的进程继续accept
    esort_server_connection_sup:start_child(),
    
    %%记录一个日志
    {ok, {PeerAddress, PeerPort}} = inet:peername(Socket),
    ?INFO_MSG("server_connection accept client_ip:~w port:~p ~n", [PeerAddress,PeerPort]),
    {noreply, State#state{socket=Socket}, ?TIMEOUT};

%%接收客户端消息，我们已经在esort_server_app里设置了{packet,4}，这里收到的Request就是一个完整包体
handle_info({tcp, Socket, Request}, #state{socket=Socket} = State) ->

    {ok,[DecodeRequest],_Rest}=rfc4627:decode(Request),
    {ok,Method}=getMethodFromJson(DecodeRequest),
    {ok,SortKey}=getSortKeyFromJson(DecodeRequest),
    {ok,Pidval} = rfc4627:get_field(DecodeRequest, "Pid"),
 
    case tool:to_atom(Method) of

	%%如果是init方法，则通知创建进程并初始化
         init ->
    	     case initSortProcess(SortKey,DecodeRequest) of
    		{ok,SortPid}  when is_pid(SortPid)  ->
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
    			{noreply, State, ?TIMEOUT};		
    		_ ->
			sendErrMsgToClient(Socket,Pidval,init,can_not_creat_process),
    			{noreply, State, ?TIMEOUT}
    
     	     end;

	%%更新或插入某个玩家的排序数据 
	update ->
		case isSortProcessExist (SortKey) of	
		     {ok,SortPid} ->
			ok=gen_server:call(SortPid, [update,DecodeRequest]),
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
			{noreply, State, ?TIMEOUT};
		     _ ->
			sendErrMsgToClient(Socket,Pidval,update,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
		end;

	%%更新或者插入玩家数据，同时返回自己的排行和指定的top列表
	updateAndGetRank ->
		case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
                        {ok,TopList,MyRank}=gen_server:call(SortPid, [updateAndGetRank,DecodeRequest]),
			RetVal={obj,[{"TopN",{obj,TopList} },{"MyRank",MyRank}] },
			sendOkMsgToClient(Socket,Method,RetVal,Pidval),
                        {noreply, State, ?TIMEOUT};
                     _ ->
			sendErrMsgToClient(Socket,Pidval,updateAndGetRank,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
                end;

	%%获得所有排序列表
	getAllSortList ->
		case isSortProcessExist (SortKey) of
		     {ok,SortPid} ->
		     	{ok,SortList}=gen_server:call(SortPid, [getAllSortList]),
			RetVal={obj,SortList},
			sendOkMsgToClient(Socket,Method,RetVal,Pidval),
			{noreply, State, ?TIMEOUT};
		     _ ->
			sendErrMsgToClient(Socket,Pidval,getAllSortList,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
		end;

	 %%获得自己的排名
	getMyRank ->
		case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
			{ok,MyRank}=gen_server:call(SortPid, [getMyRank,DecodeRequest]),
			sendOkMsgToClient(Socket,Method,MyRank,Pidval),			
                        {noreply, State, ?TIMEOUT};
		     _ ->
			sendErrMsgToClient(Socket,Pidval,getMyRank,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
		end;		

	%%获得前N~M项
        getTop ->
                case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
                        {ok,SortList}=gen_server:call(SortPid, [getTop,DecodeRequest]),
			RetVal={obj,SortList},
			sendOkMsgToClient(Socket,Method,RetVal,Pidval), 
                        {noreply, State, ?TIMEOUT};
                     _ ->
			sendErrMsgToClient(Socket,Pidval,getTop,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
                end;

	%%获得当前服务器内排序列表的总元素个数
	getAllCount -> 
		case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
                        {ok,Count}=gen_server:call(SortPid, [getAllCount]),
			sendOkMsgToClient(Socket,Method,Count,Pidval),
                        {noreply, State, ?TIMEOUT};
                     _ ->
			sendErrMsgToClient(Socket,Pidval,getAllCount,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
                end;

	%%删除某个玩家的数据
	delete ->
		 case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
                        ok=gen_server:call(SortPid, [delete,DecodeRequest]),
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
                        {noreply, State, ?TIMEOUT};
                     _ ->
			sendErrMsgToClient(Socket,Pidval,delete,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
                end;	

	%%获得某个玩家的信息和排名
	getMyInfo ->
		case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
			{ok,MyInfo,MyRank}=gen_server:call(SortPid, [getMyInfo,DecodeRequest]),
                        RetVal={obj,[{"MyInfo",{obj,MyInfo} },{"MyRank",MyRank}] },
                        sendOkMsgToClient(Socket,Method,RetVal,Pidval),
                        {noreply, State, ?TIMEOUT};
                     _ ->
                        sendErrMsgToClient(Socket,Pidval,getMyInfo,sort_process_not_exist),
                        {noreply, State, ?TIMEOUT}
                end;
	%%删除所有玩家的数据
	clearAll -> 
		case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
                        ok=gen_server:call(SortPid, [clearAll]),
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
                        {noreply, State, ?TIMEOUT};
                     _ ->
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
                        {noreply, State, ?TIMEOUT}
                end;

	%%停止排序服务
	destroy ->
		case isSortProcessExist (SortKey) of
                     {ok,SortPid} ->
			gen_server:cast(SortPid, [destroy]),
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
                        {noreply, State, ?TIMEOUT};
                     _ ->
			sendOkMsgToClient(Socket,Method,<<>>,Pidval),
                        {noreply, State, ?TIMEOUT}
	  	end;
        _ ->
            	{stop, nomethod, State}
    end;


%% send by OPT timeout
handle_info(timeout, #state{socket=Socket} = State) when is_port(Socket) ->
    {ok, {PeerAddress, PeerPort}} = inet:peername(Socket),
    ?INFO_MSG("server_connection close client_ip:~p port:~p  ~n",[PeerAddress,PeerPort]),
    {stop, timeout, State};

handle_info({'EXIT', _, Reason}, State) ->
    ?INFO_MSG("exit:~p~n", [Reason]),
    {stop, normal, State};

handle_info({tcp_closed, _}, State) ->
    ?INFO_MSG("tcp_closed ~n"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    ?INFO_MSG("tcp_error ~p ~n",[Reason]),
    {stop, Reason, State}.

terminate(_Reason, #state{socket=Socket}) ->
    case is_port(Socket) of
	true -> gen_tcp:close(Socket);
        false -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
getMethodFromJson(Json) ->

    case rfc4627:get_field(Json, "Method") of
        {ok,Method}->
                AtomMethod=tool:to_atom(Method),
		{ok,AtomMethod};
	not_found->
		{error,not_found}
    end.

getSortKeyFromJson(Json) ->
    case rfc4627:get_field(Json, "Sortkey") of
        {ok,Sortkey}->
                AtomSortkey=tool:to_atom(Sortkey),
                {ok,AtomSortkey};
        not_found->
                {error,not_found}
    end.

isSortProcessExist(SortKey) ->
    case tool:whereis_name({global,SortKey}) of
	Pid when is_pid(Pid)->
	     {ok,Pid};
	undefined ->
	     {undefined,SortKey}

   end.

initSortProcess(SortKey,InitMsg)->
    case isSortProcessExist (SortKey) of
          %%如果已经存在了，则返回
         {ok,SortPid}->
             ?INFO_MSG("sortkey:~p already exist ~p ~n",[SortKey,SortPid]),
	     {ok,SortPid};
          %%如果不存在则重新创建
         {undefined,_}->
             case esort_server_worker:createSortProcess(SortKey,InitMsg) of
	          SortPid when is_pid(SortPid)->
		         ?INFO_MSG("sortkey:~p not exist,create now:~p ~n",[SortKey,SortPid]),
		         {ok,SortPid};
	           _ ->
			 {error,fail}
	    end
   end.
sendErrMsgToClient(Socket,Pidval,Method,Reason)->
	Err_ret1=rfc4627:set_field(?RET_ERR,"Method",Method),
        Err_ret2=rfc4627:set_field(Err_ret1,"Msg",Reason),
	Err_ret3=tool:addToJson(Err_ret2,{"Pid",Pidval}),
        gen_tcp:send(Socket, rfc4627:encode([ Err_ret3 ])).

sendOkMsgToClient(Socket,Method,RetVal,Pidval)->
	RetOk1=rfc4627:set_field(?RET_OK,"Method",Method),
	RetOk2=rfc4627:set_field(RetOk1,"Val",RetVal),
        RetOk3=tool:addToJson(RetOk2,{"Pid",Pidval}),
        gen_tcp:send(Socket, rfc4627:encode([ RetOk3 ])).


