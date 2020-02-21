-module(ai_pq).

-export([
         empty/0,
         add/3,
         move/4,
         remove/3,
         prune/2,
         collect/2
        ]).

-spec empty() -> {integer(),tuple()}.
empty() -> gb_trees:empty().
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


%% @spec prune(Tree, Priority) -> Tree1
%% @doc Remove nodes with priority less than or equal to `Priority'
-spec prune({integer(),tuple()},integer())-> {integer(),tuple()}.
prune({Size, TreeNode}, Priority) ->
  {Tree1, NumDeleted} = prune_nodes(TreeNode, Priority),
  {Size - NumDeleted, Tree1}.


%% @spec collect(Tree, Priority) -> List.
%% @doc Fold over values with priority greater than `Priority'
-spec collect({integer(),tuple()},integer())-> list().
collect({_Size, TreeNode}, Priority) ->
  collect_nodes(fun(V, Acc) -> [V|Acc] end,[], TreeNode, Priority).

%% 当前节点的值大于Priority
%% 收集右侧子树的所有的值，左侧子树也可能存在适合的值
collect_nodes(Function, Acc, {K, V, S, L}, Priority) when K > Priority ->
  Acc0 = collect_nodes(Function,Acc, L, Priority),
  Acc1 = lists:foldl(Function, Acc0, V),
  collect_nodes(Function, Acc1, S, Priority);

collect_nodes(Function, Acc, {K, _V, _S, L}, Priority) when K =< Priority ->
  collect_nodes(Function, Acc, L, Priority);
collect_nodes(_Function, Acc, nil, _Priority) -> Acc.


%% 当前节点的值比Priority大
%% 右侧子树不动，遍历左侧子树
prune_nodes({K, V, S, L},Priority) when K > Priority ->
  {Tree1, NumDeleted} = prune_nodes(S, Priority),
  {{K, V, Tree1, L}, NumDeleted};
%% 当前节点值比Priority小
%% 需要对左右子树都进行遍历
prune_nodes({K, _V, S, L}, Priority) when K =< Priority ->
  %% 遍历左子数只是为了计算有多少项目
  {_, NumDeleted_S} = prune_nodes(S, Priority),
  %% 右子树也会存在需要清除的项目
  {Tree1, NumDeleted_L} = prune_nodes(L, Priority),
  {Tree1, NumDeleted_S + NumDeleted_L + 1};
prune_nodes(nil, _Priority) -> {nil, 0}.
