-module(ailib_list).
-behaviour(ailib_string).

-include("ailib.hrl").
-export([to_binary/1]).

-spec to_binary(list())->binary().
to_binary(Val)->
    %% iolist场景
  case catch erlang:iolist_to_binary(Val) of 
    {_,_} -> unicode:characters_to_binary(Val);
    Bin -> Bin
  end.