
-module(ai_pool_worker_sup).
-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(Mod, Args) ->
    supervisor:start_link(?MODULE, {Mod, Args}).

init({Mod, Args}) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 5},
    ChildSpec = #{ id => Mod,
                   start => {Mod, start_link, [Args]},
                   restart => temporary,
                   shutdown => 5000,
                   type => worker,
                   modules => [Mod]},
    {ok, {SupFlags, [ChildSpec]}}.

