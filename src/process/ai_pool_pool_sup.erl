%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_pool_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

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
-spec start_link(Name :: atom(), Children :: [term()]) -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link(Name,Children) ->
    supervisor:start_link({local, Name}, ?MODULE, Children).

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
init(Children) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 5},
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
