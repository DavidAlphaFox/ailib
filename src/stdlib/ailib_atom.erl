-module(ailib_atom).
-behaviour(ailib_string).

-include("ailib.hrl").
-export([to_atom/1,to_atom/2]).
-export([to_binary/1]).
-export([suffix/3,
         prefix/3,
         suffix/2,
         prefix/2]).

-spec suffix(Name:: string()|binary()|atom(),
             Suffix:: string()|binary()|atom()) -> atom().
suffix(Name,Suffix)-> suffix(Name,Suffix,false).


-spec prefix(Name:: string()|binary()|atom(),
             Prefix:: string()|binary()|atom()) -> atom().
prefix(Name,Prefix)-> prefix(Name,Prefix,false).

-spec suffix(Name :: string() | binary() | atom(),
             Suffix :: string() | binary() | atom(),
             Exist :: boolean()) -> atom().
suffix(Name,Suffix,Exist)->
  Name0 = ailib_string:to_binary(Name),
  Suffix0 = ailib_string:to_binary(Suffix),
  StrName  = <<Name0/binary,Suffix0/binary>>,
  ailib_string:to_atom(StrName,Exist).

-spec prefix(Name :: string() | binary() | atom(),
                  Suffix :: string() | binary() | atom(),
                  Exist :: boolean()) -> atom().
prefix(Name,Prefix,Exist)->
  Name0 = ailib_string:to_binary(Name),
  Prefix0 = ailib_string:to_binary(Prefix),
  StrName  = <<Prefix0/binary,Name0/binary>>,
  ailib_string:to_atom(StrName,Exist).


-spec to_atom(Name:: string() | binary() | atom()) -> atom().
to_atom(Name)-> to_atom(Name,false).

-spec to_atom(Name:: string() | binary() | atom(),true|false) -> atom().
to_atom(Name,_) when erlang:is_atom(Name) -> Name;
to_atom(Name,false) ->
  BName = ailib_string:to_string(Name),
  erlang:binary_to_atom(BName,utf8);
to_atom(Name,true)->
  BName = ailib_string:to_string(Name),
  erlang:binary_to_existing_atom(BName,utf8).

-spec to_binary(atom()) -> binary().
to_binary(Val)-> erlang:atom_to_binary(Val,utf8).