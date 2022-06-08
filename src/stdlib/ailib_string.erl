%% @doc ailib_string 将Erlang的binary视为字符串，从而对其进行常用的处理和操作

-module(ailib_string).
-include("ailib.hrl").

-export([join/2]).
-export([format/2,format_hex_string/2]).
-export([to_string/1]).

-callback to_binary(any()) -> binary().
-option_callbacks([to_binary/1]).

%% @doc 将各种Erlang简单的类型转化成binary.
%% <li>对于float型，会使用 `io_lib:format("~f")' 去处理浮点数，
%% 之后再使用 `erlang:list_to_binary' 转为binary。</li>
%% <li>对于boolean型，将会直接认为是atom，所以需要使用标准的true/false。</li>
%% @since 0.5.0
-spec to_string(any()) -> binary().
to_string(X) ->
  case X of
    #{?TYPE := ailib_hash} ->
      ailib_hash:to_binary(X);
    #{?TYPE := ailib_error} ->
      ailib_error:to_binary(X);
    X_ when erlang:is_binary(X_) ->
      ailib_binary:to_binary(X);
    X_ when erlang:is_integer(X_) ->
      ailib_integer:to_binary(X);
    X_ when erlang:is_float(X_) ->
      ailib_float:to_binary(X);
    X_ when erlang:is_boolean(X_) ->
      ailib_boolean:to_binary(X);
    X_ when erlang:is_list(X_) ->
      ailib_list:to_binary(X);
    _ ->
      ailib:not_implemented(?MODULE, to_binary, X)
  end.

-spec format(string(),any()) -> binary().
format(Format,Val) ->to_string(io_lib:format(Format,[Val])).

%% internal function
-spec format_hex_string(binary()|list(),upper|lower) -> binary().
format_hex_string(L,Case)->
  Binary = ailib_string:to_string(L),
  ByteSize = erlang:byte_size(Binary),
  Size = ByteSize * 2,
  Bits = ByteSize * 8,
  <<S:Bits/unsigned-integer>> = Binary,
  F =
    case Case of
      upper ->  "~X.16.0B";
      lower ->  "~X.16.0b"
    end,
  FormatStr = io_lib:format(F,[Size,"~"]),
  ailib_string:format(FormatStr,S).


-spec join(list(),binary()|list())->binary().
join([], _) -> <<>>;
join([H|T],Sep)->
  SepBin = to_string(Sep),
  Fun = fun(B)-> Str = to_string(B),<<SepBin/binary,Str/binary>> end,
  L = [ Fun(B) || B <- T ],
  lists:foldl(fun(I,Acc)-> <<Acc/binary,I/binary>> end,to_string(H),L).


