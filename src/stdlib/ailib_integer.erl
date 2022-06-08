
-module(ailib_integer).
-behaviour(ailib_string).

-export([to_integer/1]).
-export([to_binary/1]).

-spec to_integer(binary()|list()|integer()) -> integer().
to_integer(Val) when erlang:is_binary(Val) -> erlang:binary_to_integer(Val);
to_integer(Val) when erlang:is_list(Val) -> erlang:list_to_integer(Val);
to_integer(Val) when erlang:is_integer(Val) -> Val.

-spec to_binary(integer())-> binary().
to_binary(Val)-> erlang:integer_to_binary(Val).