-module(ailib_boolean).
-include("ailib.hrl").
-behaviour(ailib_string).

-export([to_binary/1,to_boolean/1]).
-spec to_binary(boolean()) -> binary().
to_binary(Val)-> erlang:atom_to_binary(Val,latin1).


-define(BOOLEAN_TRUE, ["T","1","TRUE"]).
-define(BOOLEAN_FALSE,["F","0","FALSE"]).

-spec to_boolean(binary()|iolist()|1|0|true|false) -> true|false.
to_boolean(1) -> true;
to_boolean(0) -> false;
to_boolean(true)-> true;
to_boolean(false)-> false;
to_boolean(Str) when erlang:is_binary(Str) ->
  Str0 = erlang:binary_to_list(Str),
  Str1 = string:to_upper(Str0),
  to_boolean(Str1);
to_boolean(Str) ->
  case lists:member(Str,?BOOLEAN_TRUE) of
    true -> true;
    false ->
      case lists:member(Str,?BOOLEAN_FALSE) of
        true -> false;
        false -> error({ailib_boolean,not_boolean,Str})
      end
  end.