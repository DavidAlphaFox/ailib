-module(ai_pooled_task).

-export([checkin/1]).

-export([async/1,
				 async/2,
				 async/3,
				 async/4]).
-export([await/1,
				 await/2,
				 safe_await/2,
         safe_await/3]).


async(Fun) when erlang:is_function(Fun) ->
		Me = erlang:self(),
		Pid = ai_pool:checkout(?MODULE,true,infinity),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Fun,[]}}),
   	{Pid, Ref}.
async(Node,Fun) when erlang:is_function(Fun)
										 andalso
										 Node =:= node() ->

		async(Fun);
async(Node,Fun) when erlang:is_function(Fun) ->
		Me = erlang:self(),
		Pid = ai_pool:checkout({?MODULE,Node},true,infinity),
		Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Fun,[]},group_leader()}),
   	{Pid, Ref}.

async(Mod,Fun,Args) ->
		Me = erlang:self(),
		Pid = ai_pool:checkout(?MODULE,true,infinity),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Mod,Fun,Args}}),
   	{Pid, Ref}.
async(Node,Mod,Fun,Args) when  Node =:= node() ->
		async(Mod,Fun,Args);
async(Node,Mod,Fun,Args) when erlang:is_function(Fun) ->
		Me = erlang:self(),
		Pid = ai_pool:checkout({?MODULE,Node},true,infinity),
		Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Mod,Fun,Args},group_leader()}),
   	{Pid, Ref}.

-spec await({pid(), reference()}) -> any() | no_return().
await({Pid, Ref}) ->
    await({Pid, Ref}, 5000).

-spec await({pid(), reference()},
            non_neg_integer()) -> any() | no_return().
await({Pid, Ref}, TimeOut) ->
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, noconnection} ->
            erlang:exit({nodedown, erlang:node(Pid),
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}});
        {'DOWN', Ref, _, _, Reason} ->
            erlang:exit({Reason,
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}})
    after TimeOut ->
            erlang:demonitor(Ref, [flush]),
            erlang:exit({timeout,
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}})
    end.

-spec safe_await({pid(), reference()}, term()) -> any().
safe_await(TaskRef, DefaultResult) ->
    safe_await(TaskRef, DefaultResult, 5000).

-spec safe_await({pid(), reference()},
                 term(), non_neg_integer()) -> any().
safe_await(TaskRef, DefaultResult, TimeOut) ->
    case catch await(TaskRef, TimeOut) of
        {'EXIT', _} -> DefaultResult;
        Any -> Any
    end.

checkin(Worker)->
		ai_pool:checkin(?MODULE,Worker).
