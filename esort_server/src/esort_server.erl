-module(esort_server).
-export([start/0]).

start() ->
    application:start(esort_server).

