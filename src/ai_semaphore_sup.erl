%%%-------------------------------------------------------------------
%%% @author David Gao <david@laptop-02.local>
%%% @copyright (C) 2018, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2018 by David Gao <david@laptop-02.local>
%%%-------------------------------------------------------------------
-module(ai_semaphore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_named_semphore/2,start_unnamed_semphore/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_named_semphore(Name :: atom(),Opts :: proplists:proplists())->{ok,pid()}.
start_named_semphore(Name,Opts)->
    supervisor:start_child(?SERVER,[Name,Opts]).
-spec start_unnamed_semphore(Opts :: proplists:proplists())->{ok,pid()}.
start_unnamed_semphore(Opts)->
    supervisor:start_child(?SERVER,[Opts]).
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

    Semphore = #{id => ai_semaphore,
               start => {ai_semaphore, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [ai_semaphore]},
    {ok, {SupFlags, [Semphore]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
