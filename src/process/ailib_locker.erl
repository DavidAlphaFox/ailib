%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%% a Locker implementation
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ailib_locker).

-behaviour(gen_server).

%% API
-export([start_link/1,start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/1]).

-export([new/1,new/2]).
-export([try_lock/1,lock/1,release/1,wakeup/1]).

-define(SERVER, ?MODULE).
-define(PREFIX, "ailib_locker_").

-record(state, {
                total :: integer(),
                avalible :: integer(),
                queue :: queue:queue(),
                waiters :: maps:maps(),
                lockers :: list(),
                monitors :: maps:maps()
               }).

%%%===================================================================
%%% API 
%%%===================================================================

-spec new(Count :: integer())-> {ok,pid()}.
new(Count)->
  Opts = [{avalible,Count}],
  ailib_locker_sup:new(Opts).
-spec new(Name :: atom() | list(),Count :: integer())-> {ok,pid()}.
new(Name,Count)->
  Opts = [{avalible,Count},{name,ailib_atom:prefix(Name,?PREFIX,false)}],
  ailib_locker_sup:new(Opts).
-spec wakeup(Locker :: atom()|pid()) -> ok.
wakeup(Locker) when is_pid(Locker)->
  gen_server:cast(Locker,wakeup);
wakeup(Locker) ->
  gen_server:cast(server_name(Locker),wakeup).
-spec lock(Locker :: pid()| atom()) -> ok.
lock(Locker) when erlang:is_pid(Locker)->
  do_lock(Locker);
lock(Locker) ->
  do_lock(server_name(Locker)).
-spec do_lock(Locker :: pid()| atom()) -> ok.
do_lock(Locker)->
  Caller = self(),
  gen_server:call(Locker,{lock,Caller},infinity).

-spec try_lock(Locker :: pid() | atom()) -> ok.
try_lock(Locker) when erlang:is_pid(Locker) ->
  do_try_lock(Locker);
try_lock(Locker) ->
  do_try_lock(server_name(Locker)).
-spec do_try_lock(Locker :: pid() | atom())-> ok | {error,locked}.
do_try_lock(Locker)->
  Caller = self(),
  gen_server:call(Locker,{try_lock,Caller}).
-spec release(Locker :: atom() | pid()) -> ok.
release(Locker) when erlang:is_pid(Locker)->
  do_release(Locker);
release(Locker) ->
  do_release(server_name(Locker)).
-spec do_release(Locker :: atom() | pid()) -> ok.
do_release(Locker)->
  Caller = self(),
  gen_server:cast(Locker,{release,Caller}).
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
  process_flag(trap_exit, true),
	Avalible = proplists:get_value(avalible,Opts),
	{ok, #state{
        total = Avalible,
        avalible = Avalible,
        queue = queue:new(),
        waiters = maps:new(),
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
handle_call({lock,Caller},From,#state{avalible = 0} = State)->
    NewState = wait(Caller,From,State),
    {noreply,NewState};
handle_call({lock,Caller},_From,State)->
    {Result,NewState} = lock(Caller,State),
    {reply,Result,NewState};
handle_call({try_lock,_Caller},_From,#state{avalible = 0 } = State)->
    {reply,{error,not_avalible},State};
handle_call({try_lock,Caller},_From,State)->
    {Result,NewState} = lock(Caller,State),
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
            true -> release(Caller,State)
        end,
    {noreply,NewState};
handle_cast(wakeup,#state{queue = Q} = State)->
    NewState = wakeup(Q,State),
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
    NewState = process_down(Pid,State),
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
terminate(_Reason,_State) ->
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
-spec format_status(Status :: list()) -> Status :: term().
format_status(Status) ->
	Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
server_name(Locker)-> ailib_atom:prefix(Locker,?PREFIX,true).

process_down(Pid,#state{queue = W,lockers = L} = State)->
  case queue:member(Pid,W) of
    true -> remove_waiter(Pid,State);
    _ ->
      case lists:member(Pid,L) of
        true -> release(Pid,State);
        false -> State
      end
  end.

release(Caller,#state{total = Total,avalible = Avalible,queue = W,
                      lockers = L,monitors = M} = State)->
  M2 = ailib_proc:demonitor(Caller,M),
  L2 = lists:filter(fun(I)-> I /= Caller end,L),
  if
    Total - Avalible > 0 -> notify_waiters(W,State#state{lockers = L2 , monitors = M2});
    true -> State
  end.
remove_waiter(Caller,#state{queue = W,waiters = WM,monitors = M} = State)->
  M2 = ailib_proc:demonitor(Caller,M),
  State#state{
    queue = queue:filter(fun(I)-> I /= Caller end,W),
    waiters = maps:remove(Caller,WM),
    monitors = M2
   }.

notify_waiters(Q,State) ->
  case queue:out(Q) of
    {{value, Waiter}, Q2}-> notify_waiter(Waiter,Q2,State);
    {empty,Q} -> State#state{avalible = State#state.avalible + 1}
  end.
notify_waiter(Caller,Q2,#state{waiters = WM,lockers = L,monitors = M } = State) ->
    %% 某个进程崩溃信息可能晚于Locker释放的时间
  case erlang:is_process_alive(Caller) of
    true ->
      From = maps:get(Caller,WM),
      gen_server:reply(From,lock),
      State#state{
        queue = Q2,
        waiters = maps:remove(Caller,WM),
        lockers = [Caller|L]
       };
    _ ->
      M2 = ailib_proc:demonitor(Caller,M),
      notify_waiters(Q2,State#state{
                          queue = Q2,
                          waiters = maps:remove(Caller,WM),
                          monitors = M2})
  end.

lock(Caller,#state{avalible = Avalible,lockers = L, monitors = M} = State)->
  case lists:member(Caller,L) of
    false ->
      M2 = ailib_proc:monitor(Caller,M),
      {lock,State#state{
              avalible = Avalible - 1,
              lockers = [Caller | L],
              monitors = M2
             }};
    true -> {{error,already_lock},State}
  end.
wait(Caller,From,#state{queue = W,waiters = WM, monitors = M } = State)->
  M2 = ailib_proc:monitor(Caller,M),
  State#state{
    queue = queue:in(Caller,W),
    waiters = maps:put(Caller,From,WM),
    monitors = M2
   }.
wakeup(Q,State) ->
  case queue:out(Q) of
    {{value, Waiter}, Q2}-> wakeup(Waiter,Q2,State);
    {empty,Q} -> State#state{queue = Q}
  end.
wakeup(Caller,Q2,#state{waiters = WM,monitors = M } = State) ->
  From = maps:get(Caller,WM),
  M2 = ailib_proc:demonitor(Caller,M),
  gen_server:reply(From,wakeup),
  wakeup(Q2,State#state{queue = Q2,waiters = maps:remove(Caller,WM),monitors = M2}).
