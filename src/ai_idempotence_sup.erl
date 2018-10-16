%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_idempotence_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

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

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    IdempotenceWokersSup = #{id => ai_idempotence_workers_sup,
                             start => {ai_idempotence_workers_sup, start_link, []},
                             restart => transient,
                             shutdown => 5000,
                             type => supervisor,
                             modules => [ai_idempotence_workers_sup]},
    IdempotencePoolSup = #{id => ai_idempotence_pool_sup,
                           start => {ai_idempotence_pool_sup, start_link, []},
                           restart => transient,
                           shutdown => 5000,
                           type => supervisor,
                           modules => [ai_idempotence_pool_sup]},

    {ok, {SupFlags, [IdempotenceWokersSup,IdempotencePoolSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
