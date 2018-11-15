-module(ailib_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    SemaphoreSup = #{id => ai_semaphore_sup,
                 start => {ai_semaphore_sup, start_link, []},
                 restart => transient,
                 shutdown => 5000,
                 type => supervisor,
                 modules => [ai_semaphore_sup]},
    MutexSup = #{id => ai_mutex_sup,
                 start => {ai_mutex_sup, start_link, []},
                 restart => transient,
                 shutdown => 5000,
                 type => supervisor,
                 modules => [ai_mutex_sup]},    
	{ok, {SupFlags,[SemaphoreSup,MutexSup]}}.
