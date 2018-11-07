%%%-------------------------------------------------------------------
%%% @author  <david@laptop-02.local>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2018 by  <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_cond_var).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3, format_status/2]).

-export([cond_var/0,cond_var/1,destroy/1]).

-define(SERVER, ?MODULE).
-define(SUFFIX, "_ai_cond_var").
-record(state, {
	waiters :: queue:queue(),
	waiters_map :: maps:maps(),
	monitors :: maps:maps()
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec cond_var()-> {ok,pid()}.
cond_var()->
	Opts = [],
	ai_cond_var_sup:mutex(Opts).
-spec cond_var(Name :: atom())-> {ok,pid()}.
cond_var(Name)->
    Opts = [{name,ai_strings:atom_suffix(Name,?SUFFIX,false)}],
    ai_cond_var_sup:mutex(Opts).

-spec destroy(CondVar :: atom()|pid()) -> ok.
destroy(CondVar) when is_pid(CondVar)->
    gen_server:cast(CondVar,destroy);
destroy(CondVar) ->
	gen_server:cast(server_name(CondVar),destroy).
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
-spec init(Opts :: term()) -> {ok, State :: term()} |
	{ok, State :: term(), Timeout :: timeout()} |
	{ok, State :: term(), hibernate} |
	{stop, Reason :: term()} |
	ignore.
init(_Opts) ->
	{ok, #state{
		waiters = queue:new(),
		waiters_map = maps:new(),
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
handle_call({wait,Caller,Mutex},From,State)->
	wait_cond_var(Caller,From,Mutex,State);
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
handle_cast(destroy,State)->
	NewState = destroy_cond_var(State),
	{stop,destroy,NewState};
handle_cast(signal,State)->
	NewState = signal_cond_var(State),
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
handle_info({'DOWN', _MonitorReference, process, Pid,_Reason},State)->
	NewState = remove_waiter(Pid,State),
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
server_name(CondVar) -> ai_strings:atom_suffix(CondVar,?SUFFIX,true).

wait_cond_var(Caller,From,Mutex,#state{waiters = W,waiters_map = WM, monitors = M} = State)->
	case ai_mutex:cond_release(Mutex,Caller) of 
		ok ->
			M2 = ai_process:monitor_process(Caller,M),
			{noreply,State#state{
				waiters = queue:in(Caller,W),
				waiters_map = maps:put(Caller,{From,Mutex},WM),
				monitors = M2
			}};
		Error ->
			{reply,Error,State}
	end.
destroy_cond_var(#state{waiters = W} = State)->
	NewState = loop_destroy(queue:to_list(W),State),
	NewState#state{waiters = queue:new()}.
loop_destroy([],State)->State;
loop_destroy([H|T],#state{waiters_map = WM,monitors = M} = State)->
	{From,_Mutex} = maps:get(H,WM),
	M2 = ai_process:demonitor_process(H,M),
	gen_server:reply(From,destroy),
	loop_destroy(T,State#state{
		waiters_map = maps:remove(H,WM),
		monitors = M2	
	}).
remove_waiter(Pid,#state{waiters = W,waiters_map = WM,monitors = M} = State)->
	M2 = ai_process:demonitor_process(Pid,M),
    State#state{
        waiters = queue:filter(fun(I)-> I /= Pid end,W),
        waiters_map = maps:remove(Pid,WM),
        monitors = M2
    }.
notify_waiters(Q,State) ->
	case queue:out(Q) of
		{{value, Waiter}, Q2}-> notify_waiter(Waiter,Q2,State);
		{empty,Q} -> State
	end.
notify_waiter(Caller,Q2,#state{waiters_map = WM,monitors = M } = State) ->
	%% 某个进程崩溃信息可能晚于mutex释放的时间
	case erlang:is_process_alive(Caller) of
		true ->
			M2 = ai_process:demonitor(Caller,M),
			{From,Mutex} = maps:get(Caller,WM),
			case ai_mutex:cond_lock(Mutex,Caller) of 
				ok -> gen_server:reply(From,ok);
				destroy -> gen_server:reply(From,destroy);
				Error -> gen_server:reply(From,Error)
			end,
			State#state{
				waiters = Q2,
				waiters_map = maps:remove(Caller,WM),
				monitors = M2
			};
		_ ->
			M2 = ai_process:demonitor(Caller,M),
			notify_waiters(Q2,State#state{
								waiters = Q2,
								waiters_map = maps:remove(Caller,WM),
								monitors = M2})
	end.
signal_cond_var(#state{waiters = W} = State)->
	notify_waiters(W,State).

