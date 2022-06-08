-module(ailib_boolean).
-include("ailib.hrl").
-behaviour(ailib_string).

-export([to_binary/1]).
-spec to_binary(boolean()) -> binary().
to_binary(Val)-> erlang:atom_to_binary(Val,latin1).