-module(ai_pq).

-export([remove/3, add/3, move/4]).

%% @spec delete_value(Priority, Value, Tree) -> Tree1
%% @doc Delete a `Value' associated with `Priority' from `Tree'
remove(Priority, Value, Tree) ->
    case gb_trees:lookup(Priority, Tree) of
        {value, [Value]} ->
            gb_trees:delete(Priority, Tree);
        {value, Values} ->
            gb_trees:enter(Priority, lists:delete(Value, Values), Tree)
    end.

%% @spec insert_value(Priority, Value, Tree) -> Tree1
%% @doc Insert a `Value' with associated `Priority' into `Tree'
add(Priority, Value, Tree) ->
    NewVal = case gb_trees:lookup(Priority, Tree) of
        none -> [Value];
        {value, ValueList} -> [Value|ValueList]
    end,
    gb_trees:enter(Priority, NewVal, Tree).

%% @spec move_value(OldPriority, NewPriority, Value, Tree) -> Tree1
%% @doc Change the priority of `Value' from `OldPriority' to `NewPriority'
move(OldPriority, NewPriority, Value, Tree) ->
    add(NewPriority, Value, remove(OldPriority, Value, Tree)).
