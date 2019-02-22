%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_balance_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/3,start_worker/3]).

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
-spec start_link(atom(),atom(),term()) -> {ok, Pid :: pid()} |
                      {error, {already_started, Pid :: pid()}} |
                      {error, {shutdown, term()}} |
                      {error, term()} |
                      ignore.
start_link(Name, Mod,Args) -> supervisor:start_link(?MODULE, {Name,Mod, Args}).

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
init({Name,Mod,Args}) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 5},
    ChildSpec = #{ id => Mod,
				 start => {?MODULE, start_worker, [Name,Mod,Args]},
				 restart => temporary,
				 shutdown => 5000,
				 type => worker,
				 modules => [Mod]},
    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_worker(Name,Mod,Args) ->
    {ok, Pid} = Mod:start_link(Args),
    ai_balance_pool:join(Name,Pid),
    {ok, Pid}.
