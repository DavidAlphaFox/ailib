-module(ai_task).

-export([async/3,
         async/4,
         async/1,
         async/2]).

-export([await/1,
				 await/2,
				 safe_await/2,
         safe_await/3]).

-export([async_do/3]).

%% 启动进程挂掉了，任务进程就挂掉
-spec async(function()) -> {pid(), reference()}.
async(Fun) when erlang:is_function(Fun) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(?MODULE, async_do,
                             [Me, ai_process:global_process(Me), {Fun,[]}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec async(atom(), function()) -> {pid(), reference()}.
async(Node, Fun) when erlang:is_function(Fun) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(Node, ?MODULE, async_do,
	                            [Me, ai_process:global_process(Me), {Fun,[]}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec async(atom(), atom(), [term()]) -> {pid(), reference()}.
async(Mod, Fun, Args) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(?MODULE, async_do,
                              [Me, ai_process:global_process(Me), {Mod, Fun, Args}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec async(atom(), atom(), atom(), [term()]) -> {pid(), reference()}.
async(Node, Mod, Fun, Args) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(Node, ?MODULE, async_do,
                              [Me, ai_process:global_process(Me), {Mod, Fun, Args}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
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

-spec async_do(pid(), {node(), pid() | atom()},
               {atom(), atom(), [term()]}) -> term().
async_do(TaskOwner, TaskOwnerInfo, MFA) ->
		case MFA of
			{Mod,Fun,Args}->
						erlang:put('$initial_call',{Mod, Fun, erlang:length(Args),Args});
			{Fun,Args}->
						erlang:put('$initial_call',{Fun, erlang:length(Args),Args})
		end,
		%% 收到自己的Ref才开始运行
		%% 确保和自己的发起者保持一致
    Ref =
        receive
            {TaskOwner, Ref1} -> Ref1
        end,
    erlang:send(TaskOwner, {Ref, do_apply(TaskOwnerInfo, MFA)}).

-ifdef(OTP_RELEASE).
do_apply(TaskOwnerInfo,MFA) ->
    try
				ai_function:run_mfa(MFA)
    catch
        error: Value: StackTrace ->
            task_exit(TaskOwnerInfo,
                      {Value,StackTrace});
        throw: Value: StackTrace ->
            task_exit(TaskOwnerInfo,
                      {{nocatch, Value}, StackTrace});
        exit: Value ->
            task_exit(TaskOwnerInfo, Value)
    end.
-else.
do_apply(TaskOwnerInfo,MFA) ->
    try
			ai_function:run_mfa(MFA)
    catch
        error: Value ->
            task_exit(TaskOwnerInfo,
                      {Value, erlang:get_stacktrace()});
        throw: Value ->
            task_exit(TaskOwnerInfo,
                      {{nocatch, Value}, erlang:get_stacktrace()});
        exit: Value ->
            task_exit(TaskOwnerInfo, Value)
    end.
-endif.
task_exit(_, normal) ->erlang:exit(normal);
task_exit(_, shutdown) -> erlang:exit(shutdown);
task_exit(_, Reason) when erlang:tuple_size(Reason) =:=2
                             andalso
                             erlang:element(2, Reason) =:= shutdown ->
    erlang:exit(Reason);
task_exit(TaskOwnerInfo,Reason) ->
	InitialCall = erlang:get('$initial_call'),
	error_logger:format(
      "** Task ~p terminating~n" ++
          "** Started from ~p~n" ++
          "** When initial call == ~p~n" ++
          "** Reason for termination == ~n" ++
          "** ~p~n", [erlang:self(),
                      get_from(TaskOwnerInfo),
                      InitialCall, Reason]),
    erlang:exit(Reason).

get_from({PidOrName,Node}) when Node =:= erlang:node() ->PidOrName;
get_from(Other) -> Other.

