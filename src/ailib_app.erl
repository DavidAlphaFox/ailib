-module(ailib_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	application:ensure_started(crypto),
    application:ensure_started(poolboy),
    application:ensure_started(cowlib),
	ailib_sup:start_link().

stop(_State) ->
	ok.
