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

-export([create/2]).
-export([wait/1,wait/2]).
-export([release/1,release/2]).
-define(SERVER, ?MODULE).

-record(semaphore,{
	total :: integer(),
	avalible :: integer()
}).

-record(state, {
	semaphores :: maps:maps(),
	waiters :: maps:maps(),
	monitors :: maps:maps()
}).

%%%===================================================================
%%% API 
%%%===================================================================
-spec create(Semaphore :: atom(),Count :: integer())-> {ok,pid()}.
create(Semaphore,Count)->
	Opts = [
		{semaphores, maps:from_list([{Semaphore,#semaphore{total = Count,avalible = Count}}])}
	],
	ai_semaphore_sup:start_named_semaphore(Semaphore,Opts).

-spec wait(Semaphore :: pid()| atom()) -> ok.
wait(Semaphore) when erlang:is_pid(Semaphore)->
	wait(Semaphore,?SERVER);
wait(Semaphore) ->
		wait(Semaphore,Semaphore).
-spec wait(Semaphore :: atom() | pid(),Key::binary() | atom()) -> ok.
wait(Semaphore,Key)->
	Caller = self(),
	gen_server:call(Semaphore,{wait,Caller,Key},infinity).

-spec release(Semaphore :: atom() | pid()) -> ok.
release(Semaphore) when erlang:is_pid(Semaphore)->
	release(Semaphore,?SERVER);
release(Semaphore) ->
	release(Semaphore,Semaphore).
-spec release(Semaphore :: atom() | pid(),Key::binary() | atom()) -> ok.
release(Semaphore,Key)->
	Caller = self(),
	gen_server:cast(Semaphore,{release,Caller,Key}).
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
	gen_server:start_link(?MODULE, Opts, []).

-spec start_link(Name :: atom(),Opts :: proplists:proplists()) -> {ok, Pid :: pid()} |
											{error, Error :: {already_started, pid()}} |
											{error, Error :: term()} |
											ignore.
start_link(undefined,Opts)->
	gen_server:start_link({local,?SERVER}, ?MODULE, Opts, []);
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
	process_flag(trap_exit, true),
	Semaphores = proplists:get_value(semaphores,Opts),
	{ok, #state{
		semaphores = Semaphores,
		waiters = maps:new(),
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
handle_call({wait,Caller,Key},From,#state{semaphores = Semaphores} = State)->
		try_wait_semaphore(maps:get(Key,Semaphores,undefined),Caller,Key,From,State);
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
handle_cast({release,Caller,Key},State)->
		NState = release_semaphore(Caller,Key,State),
		{noreply,NState};
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
handle_info({'EXIT', _From, noproc},State)->
	{noreply,State};
handle_info({'EXIT', From, _Reason},{monitors = Monitors} = State)->
	Semaphores = maps:get(From,Monitors,[]),
	NState = release_semaphores(Semaphores,State),
	{noreply,NState#state{monitors = maps:remove(From,Monitors)}};

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
monitor_locker(Key,Caller,MS)->
	case maps:get(Caller,MS,undefined) of
		undefined ->
			erlang:link(Caller),
			maps:put(Caller,[Key],MS);
		L ->
			maps:update(Caller,L ++ [Key],MS)
	end.
try_wait_semaphore(undefined,_Caller,_Key,_From,State)->
	{reply,{error,not_init},State};
try_wait_semaphore(#semaphore{avalible = 0},Caller,Key,From,
	#state{waiters = WS} = State) ->
	NWS =
		case maps:get(Key,WS,undefined) of
			undefined->	
				maps:put(Key,[{Caller,From}],WS);
			W ->
				maps:update(Key,W ++ [{Caller,From}],WS)	
		end,
	{noreply,State#state{waiters = NWS}};
try_wait_semaphore(#semaphore{avalible = Avalible} = Sem,Caller,Key,_From,
	#state{semaphores = Sems, monitors = MS} = State) ->
		NSem = Sem#semaphore{avalible = Avalible -1},
		NMS = monitor_locker(Key,Caller,MS),
		{reply,ok,State#state{semaphores = maps:update(Key,NSem,Sems),monitors = NMS}}.

release_semaphore(Caller,Key,#state{monitors = MS} = State)->
	M = maps:get(Caller,MS,[]),
	NM = lists:filter(fun(I) -> I /= Key end,M),
	NState = if 
		NM == [] -> 
			erlang:unlink(Caller),
			State#state{monitors = maps:remove(Caller,MS)};
		true -> 
			State#state{monitors = maps:put(Caller,NM,MS)}
	end,
	release_semaphores([Key],NState).

release_semaphores([],State)->
	State;
release_semaphores(Semaphores,#state{semaphores = Sems,waiters = WS,monitors = MS} = State)->
		{NSems,NWS,NMS}	= lists:foldl(fun(I,{SemsAcc,WsAcc,MsAcc})->
												Sem = maps:get(I,SemsAcc),
												W = maps:get(I,WsAcc,[]),
												{NSem,NW,NMsAcc} = notify_semaphore_waiters(I,Sem,W,MsAcc),
												{maps:update(I,NSem,SemsAcc),maps:put(I,NW,WsAcc),NMsAcc}
												end,{Sems,WS,MS},Semaphores),
		State#state{semaphores = NSems,waiters = NWS,monitors = NMS}.

notify_semaphore_waiters(_Key,Sem,[],MS)->
	{Sem#semaphore{avalible = Sem#semaphore.avalible + 1},[],MS};
notify_semaphore_waiters(Key,Sem,[H|T],MS) ->
	{Caller,From} = H,
	case erlang:is_process_alive(Caller) of
		true ->
					NMS = monitor_locker(Key,Caller,MS),
					gen_server:reply(From,ok),
					{Sem,T,NMS};
		_ ->
				notify_semaphore_waiters(Key,Sem,T,MS)
	end.

