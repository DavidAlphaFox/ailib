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

-export([create/1,create/2,destroy/1]).
-export([wait/1]).
-export([release/1]).
-define(SERVER, ?MODULE).


-record(state, {
  total :: integer(),
  avalible :: integer(),
	waiters :: queue:queue(),
	monitors :: maps:maps()
}).

%%%===================================================================
%%% API 
%%%===================================================================

-spec create(Count :: integer())-> {ok,pid()}.
create(Count)->
	Opts = [{avalible,Count}],
	ai_semaphore_sup:start_server(Opts).
-spec create(Name :: atom(),Count :: integer())-> {ok,pid()}.
create(Name,Count)->
    Opts = [{avalible,Count},{name,server_name_new(Name)}],
    ai_semaphore_sup:start_server(Opts).
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
	Avalible = proplists:get_value(avalible,Opts),
	{ok, #state{
    total = Avalible,
    avalible = Avalible,
		waiters = queue:new(),
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
handle_call({waiter,Caller},From,#state{avalible = 0} = State)->
    NewState = add_waiter(Caller,From,State),
    {noreply,NewState};
handle_call({wait,Caller},_From,State)->
		NewState = lock_semaphore(Caller,State),
    {reply,ok,NewState};
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
handle_cast({release,Caller},#state{monitors = M} = State)->
    case maps:get(Caller,M,undefined) of
        undefined ->
            {noreply,State};
        MRef ->
            demonitor_process(MRef),
            NewState = release_semaphore(State#state{monitors = maps:remove(Caller,M)}),
            {noreply,NewState}
    end;
handle_cast(destroy,#state{monitors = M} = State)->
    demonitor_loop(maps:iterator(M)),
    {stop,destroy,State#state{waiters = queue:new(),monitors = maps:new()}};
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
handle_info({'DOWN', _MonitorReference, process, Pid,_Reason},#state{monitors = M} = State)->
    case maps:get(Pid,M,undefined) of
        undefined ->
            {noreply,State};
        MRef ->
            NewState = processor_down(MRef,Pid,State),
            {noreply,NewState}
    end;
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

demonitor_loop({_Key, Value, NextIterator})->
    demonitor_process(Value),
    demonitor_loop(maps:next(NextIterator));
demonitor_loop(none) -> ok.

demonitor_process(undefined) -> true;
demonitor_process(MRef) -> erlang:demonitor(MRef).
 
add_waiter(Caller,From,#state{waiters = W,monitors = M } = State)->
    MRef = erlang:monitor(process,Caller),
    State#state{
      waiters = queue:in({Caller,From},W),
      monitors = maps:put(Caller,MRef,M)
     }.
remove_waiter(Caller,#state{waiters = W} = State)->
    State#state{
      waiters = queue:filter(fun(I)-> I /= Caller end,W)
     }.

lock_semaphore(Caller,#state{avalible = Avalible,monitors = M} = State)->
    MRef = erlang:monitor(process,Caller),
    State#state{
      avalible = Avalible - 1,
      monitors = maps:put(Caller,MRef,M)
     }.

processor_down(MRef,Pid,#state{waiters = W,monitors = M} = State)->
    demonitor_process(MRef),
    case queue:member(Pid,W) of
        true ->
            remove_waiter(Pid,State#state{monitors = maps:remove(Pid,M)});
        _ ->
            release_semaphore(State#state{monitors = maps:remove(Pid,M)})
    end.


release_semaphore(#state{total = Total,avalible = Avalible,waiters = W} = State)->
    if 
        Total - Avalible > 0 ->
            notify_waiters(W,State);
        true ->
            State
    end.

notify_waiters(Q,State) ->
    case queue:out(Q) of
        {{value, Waiter}, Q2}->
           notify_waiter(Waiter,Q2,State);
        {empty,Q} ->
            State#state{avalible = State#state.avalible + 1}
    end.
notify_waiter({Caller,From},Q2,#state{monitors = M } = State) ->
    case erlang:is_process_alive(Caller) of
        true ->
            gen_server:reply(From,ok),
            State#state{waiters = Q2};
        _ ->
            MRef = maps:get(Caller,M,undefined),
            demonitor_process(MRef),
            notify_waiters(Q2,State#state{waiters = Q2,monitors = maps:remove(Caller,M)})
    end.
server_name_new(Name)->
    Lname = erlang:atom_to_list(Name) ++ "_semaphore_server",
    erlang:list_to_atom(Lname).
server_name(Name)->
    Lname = erlang:atom_to_list(Name) ++ "_semaphore_server",
    erlang:list_to_existing_atom(Lname).
