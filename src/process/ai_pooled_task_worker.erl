%%%-------------------------------------------------------------------
%%% @author David Gao <david@Davids-MacBook-Pro.local>
%%% @copyright (C) 2019, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2019 by David Gao <david@Davids-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(ai_pooled_task_worker).
-export([start_link/0]).
-export([loop/0]).

start_link()-> proc_lib:spawn_link(?MODULE,loop,[]).

loop()->
    receive
        {TaskOwner,Ref,MFA} ->
            erlang:send(TaskOwner,{Ref,do_apply(MFA)});
        {TaskOwner,Ref,MFA,GLeader} ->
            MyGL = group_leader(),
            group_leader(GLeader, self()),
            erlang:send(TaskOwner,{Ref,do_apply(MFA)}),
            group_leader(MyGL, self())
    end,
    %% 自动签入
    ai_pooled_task:checkin(self()),
    ?MODULE:loop().

-ifdef(OTP_RELEASE).
do_apply(MFA) ->
    try
				ai_function:run_mfa(MFA)
    catch
        error: Value: StackTrace ->
            task_exit({Value,StackTrace});
        throw: Value: StackTrace ->
            task_exit({{nocatch, Value}, StackTrace});
        exit: Value ->
            task_exit(Value)
    end.
-else.
do_apply(MFA) ->
    try
			ai_function:run_mfa(MFA)
    catch
        error: Value ->
            task_exit({Value, erlang:get_stacktrace()});
        throw: Value ->
            task_exit({{nocatch, Value}, erlang:get_stacktrace()});
        exit: Value ->
            task_exit(Value)
    end.
-endif.

task_exit(normal) ->erlang:exit(normal);
task_exit(shutdown) -> erlang:exit(shutdown);
task_exit(Reason) when erlang:tuple_size(Reason) =:=2
                             andalso
                             erlang:element(2, Reason) =:= shutdown ->
    erlang:exit(Reason);
task_exit(Reason) ->erlang:exit(Reason).

