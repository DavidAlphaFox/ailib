%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ailib_locker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([new/1,supervisor_spec/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
new(Opts)->
  case erlang:whereis(?SERVER) of
    undefined ->
      {ok,_Pid} = ailib_locker_sup:start_link(),
      supervisor:start_child(?SERVER,[Opts]);
    _ -> supervisor:start_child(?SERVER,[Opts])
  end.
supervisor_spec()->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => transient,
      shutdown => 5000,
      type => supervisor,
      modules => [?MODULE]}.
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 1,
               period => 5},
  Locker = #{id => ailib_locker,
             start => {ailib_locker, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [ailib_locker]},
  {ok, {SupFlags, [Locker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
