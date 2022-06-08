-module(ailib_hash).
-behaviour(ailib_string).
-include("ailib.hrl").
-export([new/1,new/2,new/3]).
-export([hash/2]).
-export([to_binary/1]).

-export_type([type/0]).
-type type() :: #{
	?TYPE => ?M,
	alog => undefined,
  data => undefined,
  format => uppper,
  hash => undefined
}.

-spec new(atom())->type().
new(Alog)->new(Alog,undefined,upper).

-spec new(atom(),any()) -> type().
new(Alog,Data)-> new(Alog,Data,upper).

-spec new(atom(),any(),uppper|lower) -> type().
new(Alog,Data,Format)->
  Hash = do_hash(Alog,Data,Format),
  #{
    ?TYPE => ?M,
		alog => Alog,
    format => Format,
    data => Data,
    hash => Hash
  }.

-spec hash(type(),any()) -> type().
hash(#{?TYPE := ?M,alog := Alog,
  format := Format} = O,Data)->
  Hash = do_hash(Alog,Data,Format),
  O#{data := Data,hash := Hash}.

-spec to_binary(type())->binary().
to_binary(#{?TYPE := ?M, data := Data, 
  alog := Alog,format := Format,hash := undefined})->
  do_hash(Alog,Data,Format);
to_binary(#{?TYPE := ?M,hash := Hash})-> Hash.


-spec do_hash(atom(),any(),upper|lower) -> binary().
do_hash(Alog,Data,Format)->
  case Data of
    undefined -> undefined;
    _ -> 
      Hash = crypto:hash(Alog,Data),
      ailib_string:format_hex_string(Hash,Format)
  end.
   