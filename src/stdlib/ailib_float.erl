-module(ailib_float).
-behaviour(ailib_string).

-include("ailib.hrl").
-export([to_binary/1]).


-spec to_binary(float())->binary().
to_binary(Val)-> erlang:list_to_binary(io_lib:format("~w", [Val])).