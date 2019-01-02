
-module(ai_mq_channel_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ai_mq_channel_sup}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 10},

    MQChannel = #{id => ai_mq_channel,
               start => {ai_mq_channel, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [ai_mq_channel]},
    {ok, {SupFlags, [MQChannel]}}.

%%%===========================================