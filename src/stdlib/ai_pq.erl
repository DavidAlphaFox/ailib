-module(ai_pq).

-export([remove/3, add/3, move/4]).

%% @spec remove(Priority, Value, Tree) -> Tree1
%% @doc Delete a `Value' associated with `Priority' from `Tree'
-spec remove(Priority :: integer(),Value :: term(),
             Tree :: term())-> term().
remove(Priority, Value, Tree) ->
  case gb_trees:lookup(Priority, Tree) of
    {value, [Value]} ->
      gb_trees:delete(Priority, Tree);
    {value, Values} ->
      gb_trees:enter(Priority, lists:delete(Value, Values), Tree)
  end.

%% @spec add(Priority, Value, Tree) -> Tree1
%% @doc Insert a `Value' with associated `Priority' into `Tree'
-spec add(Priority::integer(),Value :: term(),
          Tree :: term()) -> term().
add(Priority, Value, Tree) ->
    NewVal = case gb_trees:lookup(Priority, Tree) of
               none -> [Value];
               {value, ValueList} -> [Value|ValueList]
             end,
  gb_trees:enter(Priority, NewVal, Tree).

%% @spec move(OldPriority, NewPriority, Value, Tree) -> Tree1
%% @doc Change the priority of `Value' from `OldPriority' to `NewPriority'
-spec move(OldPriority::integer(),NewPriority::integer(),
           Value::term(),Tree::term())-> term().
move(OldPriority, NewPriority, Value, Tree) ->
  add(NewPriority, Value, remove(OldPriority, Value, Tree)).
