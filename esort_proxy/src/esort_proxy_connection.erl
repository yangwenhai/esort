-module(esort_proxy_connection).

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

-record(state, {lsock, socket,key,reterr,retok,dispatch_count}).

-define(TIMEOUT, 1000 * 60 * 5).


start_link(LSock) ->
    {ok,Pid}=gen_server:start_link(?MODULE, [LSock], []),
    {ok,Pid}.
    
init([LSock]) ->
	Err={obj,[{"ret",<<"err">>},{"msg",<<>>},{"method",<<>>}]},
	Ok={obj,[{"ret",<<"ok">>},{"val",[]} ]},
	{ok,Count} = application:get_env(esort_proxy, network_process_num),
	{ok, #state{lsock=LSock,reterr=Err,retok=Ok,dispatch_count=Count}, 0}.%%这里参数为0表示立即超时，触发handle_info

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%将最终结果返回个应用程序(php)
handle_cast(Request, State) ->
    ?INFO_MSG("child handle_cast!receive from sc,send to client ~p ~n",[Request]),
    case gen_tcp:send(State#state.socket, Request) of
        ok ->
            ?INFO_MSG("send to client ok!~n"),
       {noreply, State}
    end.


%% init的参数0返回后会立即触发这里，当accept 成功后在另外开一个进程去accept 
handle_info(timeout, #state{lsock=LSock, socket=undefined}=State) ->

    {ok, Socket} = gen_tcp:accept(LSock),

    %%启一个新的进程继续accept
    esort_proxy_connection_sup:start_child(),
  
    %%初始化随机种子(因为每个进程的随机算法一样，如果不初始化种子则每个进程随机出来的值可能一样)
    initRandomSeed(Socket),
 
    case genUniqKeyBySocket(Socket) of
	{ok,ReKey}->
        	register(list_to_atom(ReKey), self()),
           	{noreply, State#state{socket=Socket,key=ReKey}, ?TIMEOUT};
       {error,Reason}->
	   	{stop, Reason, State}
    end;

%%接收客户端消息，我们已经在esort_proxy_app里设置了{packet,4}，这里收到的Request就是一个完整包体
handle_info({tcp, Socket, Request}, #state{socket=Socket} = State) ->

    ?INFO_MSG("child receive from client,start sending to sc ~p~n",[Request]),
    {ok,[DecodeRet],_Rest}=rfc4627:decode(Request),

    %%检查该方法传上来的数据是否包括所需要的所有key
    case checkMethodKeyOk(DecodeRet) of
	{ok,_}->
		%%在解出来的消息里把当前进程的pid带上，
		%%当远程服务器返回消息时直接根据pid即可回传过来
		NewDecod=tool:addToJson(DecodeRet,{"Pid",State#state.key}),
		NewEncod=rfc4627:encode([NewDecod]),
		sendMsgToDispatch(State#state.dispatch_count,NewEncod),
		{noreply, State, ?TIMEOUT};

	{not_found,Key}->

		%把报错信息返回给前端
		Errmsg=lists:flatten(io_lib:format("not_found Key:~p",[Key])),
		Errtmpmsg=rfc4627:set_field(State#state.reterr,"msg",list_to_binary(Errmsg)),
 		{ok,Method}=rfc4627:get_field(DecodeRet, "Method"),
		ErrRetmsg=rfc4627:set_field(Errtmpmsg,"method",Method),
                EncodeMsg=rfc4627:encode([ErrRetmsg]),
		
		gen_tcp:send(Socket, EncodeMsg),
	 	{noreply, State, ?TIMEOUT}
    end;


%% send by OPT timeout
handle_info(timeout, #state{socket=Socket} = State) when is_port(Socket) ->
    {stop, normal, State};

handle_info({'EXIT', _, Reason}, State) ->
    ?INFO_MSG("exit:~p~n", [Reason]),
    {stop, normal, State};

handle_info({tcp_closed, Reason}, State) ->
    ?INFO_MSG("tcp_closed ~p ~n",[Reason]),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    ?INFO_MSG("tcp_error ~p ~n",[Reason]),
    {stop, Reason, State}.

terminate(_Reason, #state{socket=Socket}) ->
    ?INFO_MSG("enter terminate ~p ~n",[_Reason]),
    case is_port(Socket) of
	true -> gen_tcp:close(Socket);
        false -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%根据socke生成一个唯一key，当server端收到消息后需要根据这个key来找到真正的pid，然后发给对应child
genUniqKeyBySocket(Socket) ->
   case inet:peername(Socket) of 
      {ok, {PeerAddress, PeerPort}} ->
      	 	ReKey=lists:flatten(io_lib:format("~p:~p",[PeerAddress,PeerPort])),
        	{ok,ReKey};    
      {error,Reason} ->
        {error,Reason}
  end.


checkKeyExist(Json,Key) ->
     case rfc4627:get_field(Json, Key) of
	{ok,_}->
         	{ok,ok};
	not_found ->
        	{not_found,Key}
     end.


foreach([],_)->
    {ok,ok};

foreach(Mylist,Json)->
    [Head|Tail]=Mylist,
    case checkKeyExist(Json,Head) of
     	{ok,_}->
	    foreach(Tail,Json);
 	{not_found,Key}->
	   {not_found,Key}
   end.

%%根据请求的方法名，检查该方法内是否有必要的key
checkMethodKeyOk(Json)->

    case rfc4627:get_field(Json, "Method") of
	{ok,Method}->
		AtomMethod=tool:to_atom(Method),
		case application:get_env(esort_proxy, AtomMethod) of
			{ok,Keys} ->
				LlistKeys=tuple_to_list(Keys),
				case foreach(LlistKeys,Json) of
					{ok,_}->
						{ok,ok};
					{not_found,Key} ->
						{not_found,Key}
				end;
			_  ->
				{not_found,AtomMethod}
		end;
	 _  ->
	        {not_found,method_key_not_exist}
    end.

%%初始化随机种子
initRandomSeed(Socket)->
    {Val1,Val2,Val3}  = erlang:now(),
    {ok, {IP, Port}}  = inet:peername(Socket),
    {Ip0,Ip1,Ip2,Ip3} = IP,
    random:seed({Val1+Ip0+Ip1+Ip2+Ip3+Port,Val2+random:uniform(Ip0+Ip1+Ip2+Ip3),Val3+random:uniform(Port)}).   


%%随机往一个dispatch发消息
sendMsgToDispatch(Count,Msg)->
    Num=tool:rand(1, Count),
    Name=lists:concat([esort_sc_,Num]),
    ?INFO_MSG(" ~n sendMsgToDispatch ~p ~n",[Name]),
    gen_server:cast(tool:to_atom(Name), Msg).

