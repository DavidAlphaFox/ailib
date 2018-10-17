%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_idempotence_workers_sup).

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
    PoolSpecs = worker_pool_specs(),
    {ok, {SupFlags, PoolSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
default_woker_pool()->
      [{idempotence_task_notify_pool,
        [{size,40},{worker_module,ai_idempotence_notify_worker},{strategy,fifo}],[]},
       {idempotence_task_running_pool,
        [{size,10},{worker_module,ai_idempotence_task_worker},{strategy,fifo}],[]}
      ].
worker_pool_specs()->
    {ok, PoolConf} = application:get_env(ailib, ai_idempotence_poolboy),
    MergedPools = lists:map(fun({Name,Args,WorkerArgs} = I)-> 
                                    case proplists:get_value(Name,PoolConf) of
                                        undefined -> I;
                                        Conf -> 
                                            M = maps:from_list(Args),
                                            M2 = lists:foldl(fun({Key,Value},Acc)->
                                                                     maps:puts(Key,Value,Acc)
                                                             end,M,Conf),
                                            NewArgs = maps:to_list(M2),
                                            {Name,NewArgs,WorkerArgs}
                                    end
                            end,default_woker_pool()),
    lists:map(fun({Name, Args, WorkerArgs}) ->
                      PoolArgs = [{name, {local, Name}}] ++ Args,
                      poolboy:child_spec(Name, PoolArgs, WorkerArgs)
              end, MergedPools).
