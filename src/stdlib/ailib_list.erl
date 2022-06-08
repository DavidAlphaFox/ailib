-module(ailib_list).
-behaviour(ailib_string).

-include("ailib.hrl").
-export([to_iolist/1]).
-export([to_binary/1]).


-spec to_iolist(list()|binary())-> iolist().
to_iolist(L) 
  when erlang:is_list(L)->
  erlang:binary_to_list(to_binary(L));
to_iolist(B)-> erlang:binary_to_list(B).

-spec to_binary(list())->binary().
to_binary(Val)->
    %% iolist场景
  case catch erlang:iolist_to_binary(Val) of 
    {_,_} -> unicode:characters_to_binary(Val);
    Bin -> Bin
  end.