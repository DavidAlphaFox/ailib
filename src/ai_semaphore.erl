%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%% a semaphore implementation
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_semaphore).

-behaviour(gen_server).

%% API
-export([start_link/1,start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([new/1,new/2,destroy/1]).
-export([wait/1,release/1]).

-define(SERVER, ?MODULE).
-define(SUFFIX, "_ai_semaphore").

-record(state, {
    total :: integer(),
    avalible :: integer(),
    waiters :: queue:queue(),
    waiters_map :: maps:maps(),
    lockers :: list(),
	monitors :: maps:maps()
}).

%%%===================================================================
%%% API 
%%%===================================================================

-spec new(Count :: integer())-> {ok,pid()}.
new(Count)->
	Opts = [{avalible,Count}],
	ai_semaphore_sup:new(Opts).
-spec new(Name :: atom() | list(),Count :: integer())-> {ok,pid()}.
new(Name,Count)->
    Opts = [{avalible,Count},{name,ai_string:atom_suffix(Name,?SUFFIX,false)}],
    ai_semaphore_sup:new(Opts).
-spec destroy(Semaphore :: atom()|pid()) -> ok.
destroy(Semaphore) when is_pid(Semaphore)->
    gen_server:cast(Semaphore,destroy);
destroy(Semaphore) ->
    gen_server:cast(server_name(Semaphore),destroy).
-spec wait(Semaphore :: pid()| atom()) -> ok.
wait(Semaphore) when erlang:is_pid(Semaphore)->
    do_wait(Semaphore);
wait(Semaphore) ->
    do_wait(server_name(Semaphore)).
-spec do_wait(Semaphore :: pid()| atom()) -> ok.
do_wait(Semaphore)->
    Caller = self(),
    gen_server:call(Semaphore,{wait,Caller},infinity).

-spec release(Semaphore :: atom() | pid()) -> ok.
release(Semaphore) when erlang:is_pid(Semaphore)->
    do_release(Semaphore);
release(Semaphore) ->
    do_release(server_name(Semaphore)).
-spec do_release(Semaphore :: atom() | pid()) -> ok.
do_release(Semaphore)->
	Caller = self(),
	gen_server:cast(Semaphore,{release,Caller}).
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
        undefined -> gen_server:start_link(?MODULE, Opts, []);
        _ -> start_link(Name,proplists:delete(name, Opts))
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
	Avalible = proplists:get_value(avalible,Opts),
	{ok, #state{
        total = Avalible,
        avalible = Avalible,
        waiters = queue:new(),
        waiters_map = maps:new(),
        lockers = [],
		monitors = maps:new()
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
handle_call({wait,Caller},From,#state{avalible = 0} = State)->
    NewState = wait_semaphore(Caller,From,State),
    {noreply,NewState};
handle_call({wait,Caller},_From,State)->
    {Result,NewState} = lock_semaphore(Caller,State),
    {reply,Result,NewState};
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
handle_cast({release,Caller},#state{lockers = L} = State)->
    NewState = 
        case lists:member(Caller,L) of
            false -> State;
            true -> release_semaphore(Caller,State)
        end,
    {noreply,NewState};
handle_cast(destroy,#state{waiters = W,lockers = L} =State)->
    NewState = destroy_semaphore(queue:to_list(W),L,State),
    {stop,destroy,NewState#state{waiters = queue:new(),lockers = []}};
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
handle_info({'DOWN', _MonitorReference, process, Pid,_Reason},State)->
    NewState = processor_down(Pid,State),
    {noreply,NewState};
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
server_name(Semaphore)-> ai_string:atom_suffix(Semaphore,?SUFFIX,true).
destroy_semaphore(Waiters,Lockers,State)->
    State1 = destroy_waiters(Waiters,State),
    destroy_lockers(Lockers,State1).
destroy_waiters([],State)-> State;
destroy_waiters([H|T],#state{waiters_map = WM, monitors = M } = State)->
    From = maps:get(H,WM),
    M2 = ai_process:demonitor_process(H,M),
    gen_server:reply(From,destroy),
    destroy_waiters(T,State#state{waiters_map = maps:remove(H,WM),monitors = M2}).

destroy_lockers([],State) -> State;
destroy_lockers([H|T],#state{monitors = M }  = State)->
    M2 = ai_process:demonitor_process(H,M),
    destroy_lockers(T,State#state{monitors = M2}).

wait_semaphore(Caller,From,#state{waiters = W,waiters_map = WM, monitors = M } = State)->
    M2 = ai_process:monitor_process(Caller,M),
    State#state{
        waiters = queue:in(Caller,W),
        waiters_map = maps:put(Caller,From,WM),
        monitors = M2
    }.
remove_waiter(Caller,#state{waiters = W,waiters_map = WM,monitors = M} = State)->
    M2 = ai_process:demonitor_process(Caller,M),
    State#state{
        waiters = queue:filter(fun(I)-> I /= Caller end,W),
        waiters_map = maps:remove(Caller,WM),
        monitors = M2
    }.

lock_semaphore(Caller,#state{avalible = Avalible,lockers = L, monitors = M} = State)->
    case lists:member(Caller,L) of
        false ->
            M2 = ai_process:monitor_process(Caller,M),
            {ok,State#state{
                avalible = Avalible - 1,
                lockers = [Caller | L],
                monitors = M2
            }};
        true -> {{error,already_lock},State}
    end.


processor_down(Pid,#state{waiters = W,lockers = L} = State)->
    case queue:member(Pid,W) of
        true -> remove_waiter(Pid,State);
        _ ->
            case lists:member(Pid,L) of 
                true -> release_semaphore(Pid,State);
                false -> State
            end
    end.


release_semaphore(Caller,#state{total = Total,avalible = Avalible,waiters = W,
                        lockers = L,monitors = M} = State)->
    M2 = ai_process:demonitor_process(Caller,M),
    L2 = lists:filter(fun(I)-> I /= Caller end,L),
    if 
        Total - Avalible > 0 -> notify_waiters(W,State#state{lockers = L2 , monitors = M2});
        true -> State
    end.

notify_waiters(Q,State) ->
    case queue:out(Q) of
        {{value, Waiter}, Q2}-> notify_waiter(Waiter,Q2,State);
        {empty,Q} -> State#state{avalible = State#state.avalible + 1}
    end.
notify_waiter(Caller,Q2,#state{waiters_map = WM,lockers = L,monitors = M } = State) ->
    %% 某个进程崩溃信息可能晚于semaphore释放的时间
    case erlang:is_process_alive(Caller) of
        true ->
            From = maps:get(Caller,WM),
            gen_server:reply(From,ok),
            State#state{
                waiters = Q2, 
                waiters_map = maps:remove(Caller,WM),
                lockers = [Caller|L]
            };
        _ ->
            M2 = ai_process:demonitor(Caller,M),
            notify_waiters(Q2,State#state{
                                waiters = Q2,
                                waiters_map = maps:remove(Caller,WM),
                                monitors = M2})
    end.
