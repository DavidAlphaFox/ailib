-module(ailib_error).
-include("ailib.hrl").

-export([new/2]).

-export([message/1]).
-export([hash/1]).
-export([to_binary/1]).

-export_type([type/0]).
-type type() :: #{
	?TYPE => ?M,
	module => undefined,
  message => binary()
}.

-spec new(binary(),module()) -> type().
new(Message,Module) 
	when is_binary(Message),
			 is_atom(Module) ->
  #{
    ?TYPE => ?M,
		module => Module,
    message => Message
  }.
-spec message(type()) -> binary().
message(#{?TYPE := ?M, message := Message}) ->
  Message.

-spec hash(type()) -> integer().
hash(#{?TYPE := ?M} = Error) ->
  erlang:phash2(Error).

-spec to_binary(type()) -> binary().
to_binary(#{?TYPE := ?M,module := Module,message := Message}) ->
  ModuleBin = atom_to_binary(Module, utf8),
  <<ModuleBin/binary, ": ", Message/binary>>.

