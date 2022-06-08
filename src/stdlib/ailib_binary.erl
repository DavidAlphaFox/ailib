-module(ailib_binary).
-include("ailib.hrl").
-behaviour(ailib_string).

-export([to_binary/1]).

-spec to_binary(binary()) -> binary().
to_binary(Val)-> Val.