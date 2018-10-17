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
    IdempotenceSup = #{id => ai_idempotence_sup,
										 start => {ai_idempotence_sup, start_link, []},
										 restart => transient,
										 shutdown => 5000,
										 type => supervisor,
										 modules => [ai_idempotence_sup]},
	{ok, {SupFlags,[SemaphoreSup,IdempotenceSup]}}.
