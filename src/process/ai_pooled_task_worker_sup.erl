-module(ai_pooled_task_worker_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE,[]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 5},
    ChildSpec = #{ id => ai_pooled_task_worker,
                   start => {ai_pooled_task_worker, start_worker, []},
                   restart => temporary,
                   shutdown => 5000,
                   type => worker,
                   modules => [ai_pooled_task_worker]},
    {ok, {SupFlags, [ChildSpec]}}.
