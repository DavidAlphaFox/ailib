-module(ailib).
-include("ailib.hrl").

-export([not_implemented/3]).


-spec type_module(any()) -> module().
type_module(#{?TYPE := Name})       -> Name;
type_module(X) when is_binary(X)    -> ailib_binary;
type_module(X) when is_bitstring(X) -> ailib_bitstring;
type_module(X) when is_integer(X)   -> ailib_integer;
type_module(X) when is_float(X)     -> ailib_float;
type_module(X) when is_boolean(X)   -> ailib_boolean;
type_module(X) when is_list(X)      -> ailib_list;
type_module(X) when is_map(X)       -> ailib_map;
type_module(X) when is_tuple(X)     -> ailib_tuple;
type_module(X) when is_function(X)  -> ailib_fn;
type_module(undefined)              -> ailib_nil;
type_module(X) when is_atom(X)      -> ailib_atom;
type_module(X) when is_port(X)      -> ailib_port;
type_module(X) when is_pid(X)       -> ailib_proc;
type_module(X) when is_reference(X) -> ailib_ref;
type_module(Value) -> throw({type_module,unsupported_type,Value}).

-spec format_error(any()) -> binary().
format_error(List) when is_list(List) ->
  erlang:iolist_to_binary(lists:map(fun ailib_string:to_binary/1, List));
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) ->
  iolist_to_binary(io_lib:format("~p", [Reason])).

-spec not_implemented(module(), atom(), any()) -> no_return().
not_implemented(Module, Function, Value) ->
  Type = type_module(Value),
  Message = format_error(
    [ <<"No implementation of callback: '">>, 
      atom_to_binary(Function, utf8), 
      <<"' of module: ">>, 
      atom_to_binary(Module, utf8), 
      <<" found in type: ">>, 
      atom_to_binary(Type, utf8)
    ]),
  Error = ailib_error:new(Message,Type),
  erlang:error({not_implemented,Error}).
  