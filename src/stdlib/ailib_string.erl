%% @doc ailib_string 将Erlang的binary视为字符串，从而对其进行常用的处理和操作

-module(ailib_string).

-export([to_binary/1,to_binary/2]).
-export([to_integer/1,to_boolean/1]).
-export([to_hex/1,to_hex/2]).
-export([to_atom/1,to_atom/2]).
-export([to_iolist/1]).
-export([join/2]).

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
        false -> throw({error,not_boolean})
      end
  end.



-spec to_integer(binary()|list()|integer()) -> integer().
to_integer(Val) when erlang:is_binary(Val) -> erlang:binary_to_integer(Val);
to_integer(Val) when erlang:is_list(Val) -> erlang:list_to_integer(Val);
to_integer(Val) when erlang:is_integer(Val) -> Val.


%% @doc 将各种Erlang简单的类型转化成binary.
%% <li>对于float型，会使用 `io_lib:format("~f")' 去处理浮点数，
%% 之后再使用 `erlang:list_to_binary' 转为binary。</li>
%% <li>对于boolean型，将会直接认为是atom，所以需要使用标准的true/false。</li>
%% @since 0.5.0

-spec to_binary(binary()|list()|integer()
                |float()|atom()|boolean()) -> binary().
to_binary(Val) when erlang:is_integer(Val) -> erlang:integer_to_binary(Val);
to_binary(Val) when erlang:is_float(Val) -> erlang:list_to_binary(io_lib:format("~f", [Val]));
to_binary(Val) when erlang:is_boolean(Val) -> erlang:atom_to_binary(Val,latin1);
to_binary(Val) when erlang:is_atom(Val) -> erlang:atom_to_binary(Val,utf8);
%% iolist场景
to_binary(Val) when erlang:is_list(Val)  ->
  try
    erlang:iolist_to_binary(Val)
  catch
    _Reason:_Error -> unicode:characters_to_binary(Val)
  end;
to_binary(Val) when erlang:is_binary(Val)-> Val.

-spec to_binary(string(),
                 binary()|list()|integer()
                |float()|atom()|boolean()) -> binary().
to_binary(Format,Val) ->to_binary(io_lib:format(Format,[Val])).

-spec to_iolist(list()|binary())-> iolist().
to_iolist(L) when erlang:is_list(L)->
  B =
    try
      erlang:iolist_to_binary(L)
    catch
      _Reason:_Error -> unicode:characters_to_binary(L)
    end,
  to_iolist(B);
to_iolist(B)-> erlang:binary_to_list(B).

-spec to_hex(binary()|list()) -> binary().
to_hex(L)-> to_hex(L,upper).

-spec to_hex(binary()|list(),upper|lower) -> binary().
to_hex(L,Case)->
  B = to_binary(L),
  ByteSize = erlang:byte_size(B),
  Size = ByteSize * 2,
  Bits = ByteSize * 8,
  <<S:Bits/unsigned-integer>> = B,
  FormatStr = hex_format(Size,Case),
  erlang:list_to_binary(io_lib:format(FormatStr,[S])).


-spec to_atom(Name:: string() | binary() | atom()) -> atom().
to_atom(Name)-> to_atom(Name,false).

-spec to_atom(Name:: string() | binary() | atom(),true|false) -> atom().
to_atom(Name,_) when erlang:is_atom(Name) -> Name;
to_atom(Name,false) ->
  BName = to_binary(Name),
  erlang:binary_to_atom(BName,utf8);
to_atom(Name,true)->
  BName = to_binary(Name),
  erlang:binary_to_existing_atom(BName,utf8).


-spec join(list(),binary()|list())->binary().
join([], _) -> <<>>;
join([H|T],Sep)->
  SepBin = to_binary(Sep),
  Fun = fun(B)-> Str = to_binary(B),<<SepBin/binary,Str/binary>> end,
  L = [ Fun(B) || B <- T ],
  lists:foldl(fun(I,Acc)-> <<Acc/binary,I/binary>> end,to_binary(H),L).


%% internal function
hex_format(Size,Case)->
  F =
    case Case of
      upper ->  "~X.16.0B";
      lower ->  "~X.16.0b"
    end,
  io_lib:format(F,[Size,"~"]).
