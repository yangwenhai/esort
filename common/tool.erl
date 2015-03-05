-module(tool).

-compile(export_all).

%% @doc get IP address string from Socket
ip(Socket) ->
  	{ok, {IP, _Port}} = inet:peername(Socket),
  	{Ip0,Ip1,Ip2,Ip3} = IP,
	list_to_binary(integer_to_list(Ip0)++"."++integer_to_list(Ip1)++"."++integer_to_list(Ip2)++"."++integer_to_list(Ip3)).


ip_str(IP) ->
	case IP of
		{A, B, C, D} ->
			lists:concat([A, ".", B, ".", C, ".", D]);
		{A, B, C, D, E, F, G, H} ->
			lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
		Str when is_list(Str) ->
			Str;
		_ ->
			[]
	end.


%% @doc quick sort
sort([]) ->
	[];
sort([H|T]) -> 
	sort([X||X<-T,X<H]) ++ [H] ++ sort([X||X<-T,X>=H]).


%% get the pid of a registered name
whereis_name({local, Atom}) -> 
	erlang:whereis(Atom);

whereis_name({global, Atom}) ->
	global:whereis_name(Atom).

register(local, Name, Pid) ->
	erlang:register(Name, Pid);

register(global, Name, Pid) ->
	global:re_register_name(Name, Pid);

register(unique,Name,Pid) ->
	global:register_name(Name, Pid).

unregister(Name) ->
	global:unregister_name(Name).

is_process_alive(Pid) ->    
	try 
		if is_pid(Pid) ->
     			case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
					{badrpc, _Reason}  -> false;
					Res -> Res
				end;
			true -> false
		end
	catch 
		_:_ -> false
	end.


%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> 
	{ok, State};
for(Max, Max, F, State) ->
	F(Max, State);
for(I, Max, F, State)   -> 
	{ok, NewState} = F(I, State), 
	for(I+1, Max, F, NewState).


%% for
for(Max,Max,F)->[F(Max)];
for(I,Max,F)->[F(I)|for(I+1,Max,F)].


%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) -> 
	Msg;
to_atom(Msg) when is_binary(Msg) -> 
	tool:list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) -> 
    tool:list_to_atom2(Msg);
to_atom(_) -> 
    throw(other_value).  %%list_to_atom("").

list_to_atom2(List) when is_list(List) ->
	case catch(list_to_existing_atom(List)) of
		{'EXIT', _} -> erlang:list_to_atom(List);
		Atom when is_atom(Atom) -> Atom
	end.

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> 
    round(Msg);
to_integer(_Msg) ->
    throw(other_value).

%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
	A.

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    f2s(Msg);
to_list(_) ->
    throw(other_value).



%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) -> 
    Msg;
to_binary(Msg) when is_atom(Msg) ->
	list_to_binary(atom_to_list(Msg));
	%%atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) -> 
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> 
	list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) -> 
	list_to_binary(f2s(Msg));
to_binary(_Msg) ->
    throw(other_value).


%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) ->
    M = Min - 1,
	if
		Max - M =< 0 ->
			0;
		true ->
			random:uniform(Max - M) + M
	end.


%%在解开的Json串里增加一项内容
addToJson({obj, Props}, Element) ->
	D = [ Element| Props ],
	{obj,D}.
%%在解开的Json串里删除一项内容
delFromJson({obj, Props}, Key) ->
	case lists:keysearch(Key, 1, Props) of
        	{value, {_K, Val}} ->
			[ {Key, Val }| D ] = Props,
			{obj,D};
             false ->
           	 {obj,Props}
    	end.

