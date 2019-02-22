%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_pooled_task_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
-spec start_link(Args :: term()) -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

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
init(Args) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 5},
    Args1 = lists:filter(fun({Key,_V})->
                                 (Key /= worker_module) and (Key /= name)
                         end,Args),

    PooledTaskSpec = ai_pool:pool_spec(ai_pooled_task,
                          [{worker_module,ai_pooled_task_worker},
                           {name,{local,ai_pooled_task}}
                           |Args1],[]),
    {ok, {SupFlags, [PooledTaskSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
