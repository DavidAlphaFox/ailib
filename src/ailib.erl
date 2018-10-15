-module(ailib).

%% API
-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(poolboy),
    ok = application:start(ailib).
