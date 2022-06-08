
-module(ailib_integer).
-behaviour(ailib_string).

-export([to_binary/1]).

-spec to_binary(integer())-> binary().
to_binary(Val)-> erlang:integer_to_binary(Val).
