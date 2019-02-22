-module(ai_pooled_task).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3, format_status/2]).

-export([checkin/1]).
-export([async/1,
				 async/2,
				 async/3,
				 async/4]).
-export([await/1,
				 await/2,
				 safe_await/2,
         safe_await/3]).

-record(state, {
								supervisor :: undefined | pid(),
								workers :: undefined | queue:queue(),
								waiting :: queue:queue(),
								monitors :: ets:tid(),
								size = 5 :: non_neg_integer(),
								overflow = 0 :: non_neg_integer(),
								max_overflow = 10 :: non_neg_integer(),
								strategy = lifo :: lifo | fifo
							 }).



async(Fun) when erlang:is_function(Fun) ->
		Me = erlang:self(),
		Pid = checkout(?MODULE,true,infinity),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Fun,[]}}),
   	{Pid, Ref}.
async(Node,Fun) when erlang:is_function(Fun)
										 andalso
										 Node =:= node() ->

		async(Fun);
async(Node,Fun) when erlang:is_function(Fun) ->
		Me = erlang:self(),
		Pid = checkout({?MODULE,Node},true,infinity),
		Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Fun,[]},group_leader()}),
   	{Pid, Ref}.

async(Mod,Fun,Args) ->
		Me = erlang:self(),
		Pid = checkout(?MODULE,true,infinity),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref,{Mod,Fun,Args}}),
   	{Pid, Ref}.
async(Node,Mod,Fun,Args) when  Node =:= node() ->
		async(Mod,Fun,Args);
async(Node,Mod,Fun,Args) when erlang:is_function(Fun) ->
		Me = erlang:self(),
		Pid = checkout({?MODULE,Node},true,infinity),
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


checkin(Worker) when is_pid(Worker) ->
    gen_server:cast(?MODULE, {checkin, Worker}).

-ifdef(OTP_RELEASE).
checkout(Server,Block, Timeout) ->
    CRef = make_ref(),
    try
        gen_server:call(Server, {checkout, CRef, Block}, Timeout)
    catch
				Class:Reason:Stacktrace ->
            gen_server:cast(Server, {cancel_waiting, CRef}),
            erlang:raise(Class, Reason,Stacktrace)
    end.
-else.
checkout(Server,Block, Timeout) ->
    CRef = make_ref(),
    try
        gen_server:call(Server, {checkout, CRef, Block}, Timeout)
    catch
				Class:Reason ->
            gen_server:cast(Server, {cancel_waiting, CRef}),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.
-endif.

start_link(PoolArgs)->
		gen_server:start_link({local,?MODULE}, ?MODULE,PoolArgs, []).

init(PoolArgs) ->
    process_flag(trap_exit, true),
    Waiting = queue:new(),
    Monitors = ets:new(monitors, [private]),
    {ok, Sup} = ai_pooled_task_worker_sup:start_link(),
    init(PoolArgs, #state{waiting = Waiting, monitors = Monitors,supervisor = Sup}).

init([{size, Size} | Rest], State) when is_integer(Size) ->
    init(Rest, State#state{size = Size});
init([{max_overflow, MaxOverflow} | Rest], State) when is_integer(MaxOverflow) ->
    init(Rest, State#state{max_overflow = MaxOverflow});
init([{strategy, lifo} | Rest], State) ->
    init(Rest, State#state{strategy = lifo});
init([{strategy, fifo} | Rest], State) ->
    init(Rest, State#state{strategy = fifo});
init([_ | Rest],State) ->
    init(Rest, State);
init([],#state{size = Size, supervisor = Sup} = State) ->
    Workers = prepopulate(Size, Sup),
    {ok, State#state{workers = Workers}}.

handle_cast({checkin, Pid}, State = #state{monitors = Monitors}) ->
    case ets:lookup(Monitors, Pid) of
        [{Pid, _, MRef}] ->
            true = erlang:demonitor(MRef),
            true = ets:delete(Monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            {noreply, State}
    end;

handle_cast({cancel_waiting, CRef}, State) ->
    case ets:match(State#state.monitors, {'$1', CRef, '$2'}) of
        [[Pid, MRef]] ->
            erlang:demonitor(MRef, [flush]),
            true = ets:delete(State#state.monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            Cancel = fun({_, Ref, MRef}) when Ref =:= CRef ->
                             demonitor(MRef, [flush]),
                             false;
                        (_) ->
                             true
                     end,
            Waiting = queue:filter(Cancel, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({checkout, CRef, Block}, {FromPid, _} = From, State) ->
    #state{supervisor = Sup,
           workers = Workers,
           monitors = Monitors,
           overflow = Overflow,
           max_overflow = MaxOverflow,
           strategy = Strategy} = State,
    case get_worker_with_strategy(Workers, Strategy) of
        {{value, Pid},  Left} ->
            MRef = erlang:monitor(process, FromPid),
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            {reply, Pid, State#state{workers = Left}};
        {empty, _Left} when MaxOverflow > 0, Overflow < MaxOverflow ->
            {Pid, MRef} = new_worker(Sup, FromPid),
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            {reply, Pid, State#state{overflow = Overflow + 1}};
        {empty, _Left} when Block =:= false ->
            {reply, full, State};
        {empty, _Left} ->
            MRef = erlang:monitor(process, FromPid),
            Waiting = queue:in({From, CRef, MRef}, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;

handle_call(status, _From, State) ->
    #state{workers = Workers,
           monitors = Monitors,
           overflow = Overflow} = State,
    StateName = state_name(State),
    {reply, {StateName, queue:len(Workers), Overflow, ets:info(Monitors, size)}, State};
handle_call(get_avail_workers, _From, State) ->
    Workers = State#state.workers,
    {reply, Workers, State};
handle_call(get_all_workers, _From, State) ->
    Sup = State#state.supervisor,
    WorkerList = supervisor:which_children(Sup),
    {reply, WorkerList, State};
handle_call(get_all_monitors, _From, State) ->
    Monitors = ets:select(State#state.monitors,
                          [{{'$1', '_', '$2'}, [], [{{'$1', '$2'}}]}]),
    {reply, Monitors, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, State}.

handle_info({'DOWN', MRef, _, _, _}, State) ->
    case ets:match(State#state.monitors, {'$1', '_', MRef}) of
        [[Pid]] ->
            true = ets:delete(State#state.monitors, Pid),
            NewState = handle_checkin(Pid, State),
            {noreply, NewState};
        [] ->
            Waiting = queue:filter(fun ({_, _, R}) -> R =/= MRef end, State#state.waiting),
            {noreply, State#state{waiting = Waiting}}
    end;
handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{supervisor = Sup,
           monitors = Monitors} = State,
    case ets:lookup(Monitors, Pid) of
        [{Pid, _, MRef}] ->
            true = erlang:demonitor(MRef),
            true = ets:delete(Monitors, Pid),
            NewState = handle_worker_exit(Pid, State),
            {noreply, NewState};
        [] ->
            case queue:member(Pid, State#state.workers) of
                true ->
                    W = filter_worker_by_pid(Pid, State#state.workers),
                    {noreply, State#state{workers = queue:in(new_worker(Sup), W)}};
                false ->
                    {noreply, State}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Workers = queue:to_list(State#state.workers),
    ok = lists:foreach(fun (W) -> unlink(W) end, Workers),
    true = exit(State#state.supervisor, shutdown),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec format_status(Opt :: normal | terminate,
										Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
		Status.


new_worker(Sup) ->
    {ok, Pid} = supervisor:start_child(Sup, []),
    true = link(Pid),
    Pid.

new_worker(Sup, FromPid) ->
    Pid = new_worker(Sup),
    Ref = erlang:monitor(process, FromPid),
    {Pid, Ref}.

get_worker_with_strategy(Workers, fifo) ->
    queue:out(Workers);
get_worker_with_strategy(Workers, lifo) ->
    queue:out_r(Workers).

dismiss_worker(Sup, Pid) ->
    true = unlink(Pid),
    supervisor:terminate_child(Sup, Pid).

filter_worker_by_pid(Pid, Workers) ->
    queue:filter(fun (WPid) -> WPid =/= Pid end, Workers).

prepopulate(N, _Sup) when N < 1 ->
    queue:new();
prepopulate(N, Sup) ->
    prepopulate(N, Sup, queue:new()).

prepopulate(0, _Sup, Workers) ->
    Workers;
prepopulate(N, Sup, Workers) ->
    prepopulate(N-1, Sup, queue:in(new_worker(Sup), Workers)).

handle_checkin(Pid, State) ->
    #state{supervisor = Sup,
           waiting = Waiting,
           monitors = Monitors,
           overflow = Overflow} = State,
    case queue:out(Waiting) of
        {{value, {From, CRef, MRef}}, Left} ->
            true = ets:insert(Monitors, {Pid, CRef, MRef}),
            gen_server:reply(From, Pid),
            State#state{waiting = Left};
        {empty, Empty} when Overflow > 0 ->
            ok = dismiss_worker(Sup, Pid),
            State#state{waiting = Empty, overflow = Overflow - 1};
        {empty, Empty} ->
            Workers = queue:in(Pid, State#state.workers),
            State#state{workers = Workers, waiting = Empty, overflow = 0}
    end.

handle_worker_exit(Pid, State) ->
    #state{supervisor = Sup,
           monitors = Monitors,
           overflow = Overflow} = State,
    case queue:out(State#state.waiting) of
        {{value, {From, CRef, MRef}}, LeftWaiting} ->
            NewWorker = new_worker(State#state.supervisor),
            true = ets:insert(Monitors, {NewWorker, CRef, MRef}),
            gen_server:reply(From, NewWorker),
            State#state{waiting = LeftWaiting};
        {empty, Empty} when Overflow > 0 ->
            State#state{overflow = Overflow - 1, waiting = Empty};
        {empty, Empty} ->
            W = filter_worker_by_pid(Pid, State#state.workers),
            Workers = queue:in(new_worker(Sup), W),
            State#state{workers = Workers, waiting = Empty}
    end.

state_name(State = #state{overflow = Overflow}) when Overflow < 1 ->
    #state{max_overflow = MaxOverflow, workers = Workers} = State,
    case queue:len(Workers) == 0 of
        true when MaxOverflow < 1 -> full;
        true -> overflow;
        false -> ready
    end;
state_name(#state{overflow = MaxOverflow, max_overflow = MaxOverflow}) ->
    full;
state_name(_State) ->
    overflow.
