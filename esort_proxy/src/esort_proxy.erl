-module(esort_proxy).
-export([start/0]).

start() ->
    application:start(esort_proxy).

