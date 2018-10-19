%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%	an idempotence pool looks like a fire and forget semaphore
%%% the `Key` is base on `Ctx`
%%% if some processes want to do a task which `Ctx` is same
%%% the process of pool will only schedule the task once 
%%% and notify the result to all waitting processes.
%%% @end
%%% Created : 16 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_idempotence_pool).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([named_pool/1,named_pool/3,named_pool/4]).
-export([unnamed_pool/0,unnamed_pool/2,unnamed_pool/3]).

-export([task_add/3,task_finish/3]).

-define(SERVER, ?MODULE).
-define(POOL_SIZE,10).
-record(state, {
                tasks :: maps:maps(),
                running :: list(),
                waitting :: queue:queue(),
                observers :: maps:maps(),
                notify_pool :: atom(),
                running_pool :: atom(),
                max_concurrent :: integer(),
                current_running :: integer(),
                monitors :: maps:maps()
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec unnamed_pool()-> {ok,pid()}.
unnamed_pool()->
    unnamed_pool(idempotence_task_notify_pool,idempotence_task_running_pool,?POOL_SIZE).
-spec unnamed_pool(NotifyPool :: atom(), RunningPool :: atom()) -> {ok,pid()}.
unnamed_pool(NotifyPool,RunningPool)->
    unnamed_pool(NotifyPool,RunningPool,?POOL_SIZE).
-spec unnamed_pool(NotifyPool :: atom(), RunningPool :: atom(),MaxConcurrent :: integer()) -> {ok,pid()}.
unnamed_pool(NotifyPool,RunningPool,MaxConcurrent)->
    Opts = [{notify_pool,NotifyPool},{running_pool,RunningPool},{max_concurrent,MaxConcurrent}],
    ai_idempotence_pool_sup:start_server(Opts).
-spec named_pool(Name :: atom())-> {ok,pid()}.
named_pool(Name)->
    named_pool(Name,idempotence_task_notify_pool,idempotence_task_running_pool,?POOL_SIZE).

-spec named_pool(Name :: atom(),NotifyPool :: atom(), RunningPool :: atom()) -> {ok,pid()}.
named_pool(Name,NotifyPool,RunningPool)->               
    named_pool(Name,NotifyPool,RunningPool,?POOL_SIZE).

-spec named_pool(Name :: atom(),NotifyPool :: atom(), RunningPool :: atom(),MaxConcurrent :: integer()) -> {ok,pid()}.
named_pool(Name,NotifyPool,RunningPool,MaxConcurrent)->
    Opts = [{name,server_name_new(Name)},{notify_pool,NotifyPool},{running_pool,RunningPool},{max_concurrent,MaxConcurrent}],
    ai_idempotence_pool_sup:start_server(Opts).


-spec task_add(Pool :: pid() | atom(),Key :: binary()|atom(),Ctx :: term()) -> {done,term()} | {error,term(),term()}.
task_add(Pool,Key,Ctx) when erlang:is_pid(Pool)->
    do_task_add(Pool,Key,Ctx);
task_add(Pool,Key,Ctx) ->
    do_task_add(server_name(Pool),Key,Ctx).    
-spec do_task_add(Pool :: pid() | atom(),Key :: binary()|atom(),Ctx :: term()) -> {done,term()} | {error,term(),term()}.
do_task_add(Pool,Key,Ctx)->
    Caller = self(),
    gen_server:call(Pool,{task_add,Key,Ctx,Caller},infinity).

-spec task_finish(Pool :: pid() | atom(), Key :: binary() | atom(),
                  Result :: {done,term()} | {error,term(),term()}) -> ok.
task_finish(Pool,Key,Result) when erlang:is_pid(Pool)->
    do_task_finish(Pool,Key,Result);
task_finish(Pool,Key,Result) ->
    do_task_finish(server_name(Pool),Key,Result).
-spec do_task_finish(Pool :: pid() | atom(), Key :: binary() | atom(),
                  Result :: {done,term()} | {error,term(),term()}) -> ok.
do_task_finish(Pool,Key,Result)->
    gen_server:cast(Pool,{task_finish,Key,Result}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

-spec start_link(Opts :: proplists:proplists()) -> {ok, Pid :: pid()} |
                                                   {error, Error :: {already_started, pid()}} |
                                                   {error, Error :: term()} |
                                                   ignore.
start_link(Opts) ->
    Name = proplists:get_value(name,Opts),
    case Name of
        undefined ->
            gen_server:start_link(?MODULE, Opts, []);
        _ ->
            start_link(Name,proplists:delete(name, Opts))
    end.

-spec start_link(Name :: atom(),Opts :: proplists:proplists()) -> {ok, Pid :: pid()} |
                                                                  {error, Error :: {already_started, pid()}} |
                                                                  {error, Error :: term()} |
                                                                  ignore.
start_link(Name,Opts) ->
    gen_server:start_link({local,Name}, ?MODULE, Opts, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init(Opts) ->
    NotifyPool = proplists:get_value(notify_pool,Opts),
    RunningPool = proplists:get_value(running_pool,Opts),
    MaxConcurrent = proplists:get_value(max_concurrent,Opts,10),
    {ok, #state{ tasks = maps:new(),
                 observers = maps:new(),running = [],waitting = queue:new(),
                 notify_pool = NotifyPool,running_pool = RunningPool,
                 max_concurrent = MaxConcurrent,current_running = 0,monitors = maps:new()
           }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({task_add,Key,Ctx,Caller},From,State)->
    NewState0 = task_add(Key,Ctx,Caller,From, State),
    NewState = try_schedule_task(Key,NewState0),
    {noreply,NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast({task_finish,Key,Result},State)->
    NewState0 = try_wakeup_observers(Key,Result,State),
    NewState = try_finish_task(Key,NewState0),
    {noreply,NewState};
handle_cast({reschedule_tasks,Tasks},State)->
    NewState = reschedule_tasks(Tasks,State),
    {noreply,NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'DOWN',MonitorReference, process, Pid,_Reason},#state{monitors = M} = State)->
    {MonitorReference,Tasks} = maps:get(Pid,M),
    demonitor_process(MonitorReference),
    gen_server:cast(self(),{reschedule_tasks,Tasks}),
    {noreply,State#state{monitors = maps:remove(Pid,M)}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
demonitor_process(undefined) -> true;
demonitor_process(MRef) -> erlang:demonitor(MRef).
running_pool(#state{running_pool = undefined})-> idempotence_task_running_pool;
running_pool(#state{running_pool = Name}) -> Name.
notify_pool(#state{notify_pool = undefined}) -> idempotence_task_notify_pool;
notify_pool(#state{notify_pool = Name}) ->  Name.

task_add(Key,Ctx,Caller,From,#state{tasks = T} =  State)->    
    case maps:is_key(Key,T) of
        true ->
            observer_add(Key,Caller,From,State);
        _ ->
            observer_add(Key,Caller,From,State#state{tasks = maps:put(Key,Ctx,T)})
    end.
observer_add(Key,Caller,From,#state{observers = O} = State)->
    Items = maps:get(Key,O,[]),
    State#state{
      observers = maps:put(Key,[{Caller,From}|Items],O)
     }.

try_schedule_task(Key,#state{waitting = W, max_concurrent = MaxRunning,current_running = MaxRunning } = State)->
	case queue:member(Key,W) of
				true -> State;
      	_ ->
					case queue:member(Key,W) of
						true -> State;
						_ -> State#state{ waitting = queue:in(Key,W)}
					end
  end;

try_schedule_task(Key,#state{tasks = T, running = R, current_running = CurrentRunning,monitors = M} = State) ->
    case lists:member(Key,R) of
        true -> State;
        _->
            Ctx = maps:get(Key,T),
            RunningPool = running_pool(State),
            RunningFun = fun(Worker) ->
                                 try
                                     ai_idempotence_task_worker:task_run(Worker,Key,Ctx),
                                     NM = case maps:get(Worker,M,undefined) of
                                              undefined ->
                                                  Ref = erlang:monitor(process,Worker),
                                                  maps:put(Worker,{Ref,[Key]},M);
                                              {Ref,Tasks}->
                                                  maps:put(Worker,{Ref,[Key|Tasks]},M)
                                          end,
                                     State#state{
                                       running = [Key|R],
                                       current_running = CurrentRunning + 1,
                                       monitors = NM
                                      }
                                 catch
                                     _Error:_Reason ->
                                         gen_server:cast(self(),{reschedule_tasks,[Key]}),
                                         State
                                 end
                         end,
            poolboy:transaction(RunningPool, RunningFun)
    end.
reschedule_tasks(Tasks,#state{running = R, current_running = CurrentRunning} = State)->
    ReSchedule = lists:foldl(fun(Task,Acc) ->
                                     case lists:member(Task,R) of
                                         true -> [Task|Acc];
                                         _ -> Acc
                                     end
                             end,[],Tasks),
    case erlang:length(ReSchedule) of
        0 -> State;
        N ->
            lists:foldl(fun(Task,Acc)->
                                try_schedule_task(Task,Acc)
                        end,
                        State#state{
                          running = queue:filter(fun(I) -> not lists:member(I) end,R),
                          current_running = CurrentRunning - N})
    end.
            

try_wakeup_observers(Key,Result,#state{observers = O} = State)->
    Items = maps:get(Key,O,[]),
    NotifyPool = notify_pool(State),
    poolboy:transaction(NotifyPool,fun(Worker)->
                                           ai_idempotence_notify_worker:notify(Worker,Items,Result)
                                   end),
    State#state{
      observers = maps:remove(Key,O)
     }.

try_finish_task(Key,#state{tasks = T,running = R,waitting = W, current_running = CurrentRunning} = State)->
    case queue:out(W) of
        {{value,NextTask},W2} ->
            NewState = State#state{
                         tasks = maps:remove(Key,T),
                         running = lists:filter(fun(I) -> I /= Key end,R),
                         waitting = W2,
                         current_running = CurrentRunning -1
                        },
            try_schedule_task(NextTask,NewState);
        _ ->
            State#state{
              tasks = maps:remove(Key,T),
              running = lists:filter(fun(I) -> I /= Key end,R),
              current_running = CurrentRunning -1
             }
    end.
    
server_name_new(Name)->
    Lname = erlang:atom_to_list(Name) ++ "_idempotence_pool",
    erlang:list_to_atom(Lname).
server_name(Name)->
    Lname = erlang:atom_to_list(Name) ++ "_idempotence_pool",
    erlang:list_to_existing_atom(Lname).

