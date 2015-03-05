-module(esort_server_worker).

-behaviour(gen_server).

-include("mslog.hrl").

%% API
-export([start_link/2]).

-export([createSortProcess/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {sortkey,initmsg,timeout,sortlist}).


start_link(SortKey,InitMsg) ->
    Ret=gen_server:start_link({local, ?MODULE}, ?MODULE, [SortKey,InitMsg], []),
    ?INFO_MSG("ret ~p ~n",[Ret]),
    Ret.

init([Sortkey,InitMsg]) ->
    process_flag(trap_exit, true),
    %%排序进程到期时间(秒)
    {ok,TimeOut}=rfc4627:get_field(InitMsg, "TimeOut"),
    TimeOutVal=tool:to_integer(TimeOut)*1000,
    case tool:register(unique, Sortkey, self()) of
	 yes ->
    	 	{ok, #state{sortkey=Sortkey,initmsg=InitMsg,timeout=TimeOutVal,sortlist=[]}, TimeOutVal};
   	 _ ->
		{stop,normal,#state{sortkey=Sortkey,initmsg=[],sortlist=[]}}
   end.


%%动态创建排序处理进程 
createSortProcess(SortKey,InitMsg) ->
	case tool:whereis_name({global, SortKey}) of
		Pid when is_pid(Pid) ->
			case tool:is_process_alive(Pid) of
				true -> 
					Pid;
				false -> 
					start_esort_worker(SortKey,InitMsg)
			end;
		_ ->
			start_esort_worker(SortKey,InitMsg)
	end.

%%启动对应的排序模块(加锁保证全局唯一)
start_esort_worker(SortKey,InitMsg) ->
	global:set_lock({SortKey, undefined}),	
	ProcessPid = start_worker(SortKey,InitMsg),
	global:del_lock({SortKey, undefined}),
	ProcessPid.

%%启动对应的排序模块
start_worker(SortKey,InitMsg) ->
       case supervisor:start_child(
                esort_server_sup, {esort_server_worker,
                        {esort_server_worker, start_link,[SortKey,InitMsg]},
                        temporary, 10000,supervisor, [esort_server_worker]}) of
                {ok, Pid} ->
                        timer:sleep(500),
                        Pid;
                Other->
			?INFO_MSG("~n start_worker ~p ~n",[Other]),
                        undefined
       end.


%%更新某个玩家的排序数据
handle_call([update,Msg], _From, State) ->
    
    %?INFO_MSG("~n worker handle_cast! :~p ~p ~n",[update,Msg]),
    {ok,SortVal}=rfc4627:get_field(Msg,"Val"),

    %%根据排序信息生成唯一的一个key，来单独标识这个排序的信息    
    {ok,PrimaryKey}=rfc4627:get_field(State#state.initmsg, "PrimaryKey"),
    UniqKey=getUniqKeyByPrimaryKey(SortVal,PrimaryKey),
    %%?INFO_MSG("~n UniqKey ~p ~n",[UniqKey]),

    %%如果UniqKey对应的值不存在则添加，否则更新原来的值
    NewList=updateSortVal(UniqKey,SortVal,State#state.sortlist),

    %%重新排序
    {ok,{obj,SortPriority}}=rfc4627:get_field(State#state.initmsg, "SortPriority"),
    NewSortList=sortList(NewList,SortPriority),

    %%替换原来的list
    %NewState=State#state{sortlist=NewSortList},

    ?INFO_MSG("handle_call NewSortList:~p ~n",[NewSortList]),

    {reply, ok, State#state{sortlist=NewSortList} };

%%更新某个玩家的数据，同时返回排行榜信息
handle_call([updateAndGetRank,Msg], _From, State) ->
 
  %%更新信息 
  {ok,SortVal}=rfc4627:get_field(Msg,"Val"),
  {ok,PrimaryKey}=rfc4627:get_field(State#state.initmsg, "PrimaryKey"),
  UniqKey=getUniqKeyByPrimaryKey(SortVal,PrimaryKey),
  NewList=updateSortVal(UniqKey,SortVal,State#state.sortlist),
  %%排序
  {ok,{obj,SortPriority}}=rfc4627:get_field(State#state.initmsg, "SortPriority"),
  NewSortList=sortList(NewList,SortPriority),
 
  %%返回需要的topn数据
  {ok,N}=rfc4627:get_field(Msg, "Start"),
  {ok,M}=rfc4627:get_field(Msg, "End"),
  TopSortList=getTop(NewSortList,tool:to_integer(N),tool:to_integer(M)),
 
  %%返回需要的我的排行
  MyRank=getMyRank(NewSortList,UniqKey),
 
  ?INFO_MSG("handle_call updateAndGetRank:~p ~n",[TopSortList]), 
  {reply, {ok,TopSortList,MyRank}, State#state{sortlist=NewSortList} };


%%获得所有排序数据
handle_call([getAllSortList], _From, State) ->

   SortList=getAllSortList(State#state.sortlist),
   ?INFO_MSG("handle_call getAllSortList:~p ~n",[SortList]),
   {reply, {ok,SortList}, State};


%%获得我自己的排名
handle_call([getMyRank,Msg], _From, State) ->

   {ok,PrimaryKey}=rfc4627:get_field(State#state.initmsg, "PrimaryKey"),
   {ok,UserPrimaryVal}=rfc4627:get_field(Msg, "PrimaryKey"),
   UniqKey=getUniqKeyByPrimaryKey(UserPrimaryVal,PrimaryKey),
   MyRank=getMyRank(State#state.sortlist,UniqKey),
   ?INFO_MSG("handle_call getMyRank:~p ~n",[MyRank]),
   {reply, {ok,MyRank}, State};

%%获得有序列表里的前N~M
handle_call([getTop,Msg], _From, State) ->
   {ok,N}=rfc4627:get_field(Msg, "Start"),
   {ok,M}=rfc4627:get_field(Msg, "End"),
   SortList=getTop(State#state.sortlist,tool:to_integer(N),tool:to_integer(M)),
   ?INFO_MSG("handle_call getTop:~p ~n",[SortList]), 
   {reply, {ok,SortList}, State};
  
%%获得当前排序列表里的玩家个数
handle_call([getAllCount], _From, State) ->
   Count=length(State#state.sortlist),
   ?INFO_MSG("handle_call getAllCount:~p ~n",[Count]),
   {reply, {ok,Count}, State};

%%从排序列表里删除某个玩家的数据
handle_call([delete,Msg], _From, State) ->
   %%删除
   {ok,PrimaryKey}=rfc4627:get_field(State#state.initmsg, "PrimaryKey"),
   {ok,UserPrimaryVal}=rfc4627:get_field(Msg, "PrimaryKey"),
   UniqKey=getUniqKeyByPrimaryKey(UserPrimaryVal,PrimaryKey),
   NewList=delete(State#state.sortlist,UniqKey),
   %%排序
   {ok,{obj,SortPriority}}=rfc4627:get_field(State#state.initmsg, "SortPriority"),
   NewSortList=sortList(NewList,SortPriority),
  
   ?INFO_MSG("handle_call delete:~p ~n",[NewSortList]),
   {reply, ok, State#state{sortlist=NewSortList}};

handle_call([getMyInfo,Msg], _From, State) ->
   {ok,PrimaryKey}=rfc4627:get_field(State#state.initmsg, "PrimaryKey"),
   {ok,UserPrimaryVal}=rfc4627:get_field(Msg, "PrimaryKey"),
   UniqKey=getUniqKeyByPrimaryKey(UserPrimaryVal,PrimaryKey),
   MyInfo=getUserInfoByKey(State#state.sortlist,UniqKey),
   MyRank=getMyRank(State#state.sortlist,UniqKey),
   ?INFO_MSG("handle_call getMyInfo:~p ~p ~n",[MyInfo,MyRank]),
   {reply, {ok,MyInfo,MyRank}, State};

%%清空所有排序数据
handle_call([clearAll], _From, State) ->
   ?INFO_MSG("handle_call clearAll  ~n"),
   {reply, ok, State#state{sortlist=[]}};

%%其他
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.


%%停止排序服务
handle_cast([destroy], State) ->
    ?INFO_MSG("handle_cast destroy  ~n"),
    {stop, normal, State#state{sortlist=[]}};


handle_cast(_Msg, State) ->
    {noreply, State}.


%% the first message send to this child
handle_info(timeout, State) ->
    %%?INFO_MSG("~n worker call timerout! key:~p ~n",[Sortkey]),
    {stop, normal, State};

handle_info({'EXIT', _, _Reason}, State) ->
    %%?INFO_MSG("exit:~p~n", [Reason]),
    {stop, normal, State};

handle_info({tcp_closed, _}, State) ->
    %%?INFO_MSG("tcp_closed ~n"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State}.

terminate(_Reason, _) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%根据主键的key返回一个唯一值，比如主键为server_id和uid,
%%则返回类似这样的一个串："#uid#20106##server_id#1#"
getUniqKeyByPrimaryKey(SortVal,PrimaryKey) ->
	F=fun(X) -> 
		Key=tool:to_list(X),
        	{ok,TmpVal}=rfc4627:get_field(SortVal,Key ),
		['#',Key,'#',TmpVal,'#'] 
  	end,
	KeyLlist=lists:flatmap(F,PrimaryKey),
        lists:concat(KeyLlist).

%%更新列表里的值
updateSortVal(UniqKey,SortVal,SortList) -> 
	{obj,NewVal}=SortVal,
	case lists:keysearch(UniqKey, 1, SortList) of
             {value, {_K, Val}} ->
                     D=lists:delete({UniqKey,Val},SortList),
                     [{UniqKey, NewVal}|D]; 
             false ->
                     [{UniqKey, NewVal}|SortList]
        end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%对列表进行排序
%%sort([],_) ->
%%        [];
%%sort([H|T],F) ->
%%        sort([X||X<-T,true==F(X,H)],F) ++ [H] ++ sort([X||X<-T,false==F(X,H)],F).
foreach([],_)->
    false;
foreach(List,F)->
    [Head|Tail]=List,
    case F(Head) of
        true ->
            true;
	false ->
	    false;
        equal ->
           foreach(Tail,F)
   end.

sortList(SortList,SortPriority)->
	
	SortF=fun(X,Y,E) -> 
		{Key,Compare}=E,
		{_XKey,XList}=X,
		{_YKey,YList}=Y,
		{ok,XVal}=rfc4627:get_field({obj,XList},Key),
		{ok,YVal}=rfc4627:get_field({obj,YList},Key), 
		XInt=tool:to_integer(XVal),
		YInt=tool:to_integer(YVal),
		%%?INFO_MSG("sortList if key:~p x:~p y:~p c:~p ~n",[Key,XInt,XInt,Compare]),
		if  XInt == YInt -> equal;
		    XInt > YInt,Compare == <<"asc">> -> false; 
		    XInt > YInt,Compare == <<"desc">> -> true;
                    XInt < YInt,Compare == <<"asc">> -> true;
		    XInt < YInt,Compare == <<"desc">> -> false
		end
	  end,

	F=fun(X,Y) ->foreach(SortPriority,fun(E)-> SortF(X,Y,E) end) end ,

	%%NewList=sort(SortList,F),
	lists:sort(F,SortList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%把有序的一个列表转换成json格式能识别的list
loopSortList([],State,_)->
    State;
loopSortList(List,State,Num)->
    [Head|Tail]=List,
    {_Key,Val}=Head,
    NewElement={tool:to_list(Num),{obj,Val}},
    Len=length(State),
    if Len < 1 -> 
	  loopSortList(Tail,[NewElement],Num+1);
       Len >= 1 ->
    	  NewState=State ++ [NewElement] ,  
    	  loopSortList(Tail,NewState,Num+1)
    end.

getAllSortList(SortList) ->
	loopSortList(SortList,[],1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%给定一个key，获得有序列表里这个key对应的排名
findMyRank([],_,_)->
    0;
findMyRank(List,Key,Num)->
    [Head|Tail]=List,
    {CurKey,_Val}=Head,
    case  tool:to_list(CurKey) of 
	  Key ->
		Num;
          _ ->
		findMyRank(Tail,Key,Num+1)
    end.

getMyRank(SortList,UniqKey) ->
       findMyRank(SortList,tool:to_list(UniqKey),1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%获得有序列表里的 从N 到 M 项
getSectionSortList([],State,_,_,_)->
    State;
getSectionSortList(List,State,N,M,Index)->
    [Head|Tail]=List,
    {_Key,Val}=Head,
    Len=length(State),
    
    if Index < N ->
	  getSectionSortList(Tail,State,N,M,Index+1);
       Index >= N,Index=< M,Len < 1 -> 
	  NewElement={tool:to_list(Index),{obj,Val}},
	  NewState=[NewElement],
	  getSectionSortList(Tail,NewState,N,M,Index+1);
       Index >= N,Index =< M, Len >= 1 ->
	  NewElement={tool:to_list(Index),{obj,Val}},
	  NewState=State ++ [NewElement],
	  getSectionSortList(Tail,NewState,N,M,Index+1);

       Index > M ->
	   State
   end. 

getTop(SortList,N,M) ->
	%%从列表的第1项开始找，获得 N 到 M 项
	getSectionSortList(SortList,[],N,M,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete( SortList,UniqKey )->
	case lists:keysearch(UniqKey, 1, SortList) of
             {value, {_K, Val}} ->
                     lists:delete({UniqKey,Val},SortList);
             false ->
                     SortList
        end.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getUserInfoByKey(SortList,UniqKey )->
	case lists:keysearch(UniqKey, 1, SortList) of
	     {value, {_K, Val}} ->
		     Val;
	     false ->
		     []
	end.

