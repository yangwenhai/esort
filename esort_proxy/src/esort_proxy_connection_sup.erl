-module(esort_proxy_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    case supervisor:start_child(?SERVER, []) of
         {ok,Child}->
		{ok,Child};
         {ok,Child,_} ->
		{ok,Child};
	 _ ->
	        {err,other}
    end.


init([LSock]) ->
    Child = {esort_proxy_connection, {esort_proxy_connection, start_link, [LSock]},
             temporary, brutal_kill, worker, [esort_proxy_connection]},

    Children = [Child],
    Restart = {simple_one_for_one, 0, 1},
    {ok, {Restart, Children}}.

