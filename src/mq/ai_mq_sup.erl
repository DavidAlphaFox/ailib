-module(aimq_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    MQManagerSup = #{id => ai_mq_manager_sup,
                 start => {ai_mq_manager_sup, start_link, []},
                 restart => transient,
                 shutdown => 5000,
                 type => supervisor,
                 modules => [ai_mq_manager_sup]}, 
    MQChannelSup = #{id => ai_mq_channel_sup,
                 start => {ai_mq_channel_sup, start_link, []},
                 restart => transient,
                 shutdown => 5000,
                 type => supervisor,
                 modules => [ai_mq_channel_sup]}, 
	{ok, {SupFlags,[MQManagerSup,MQChannelSup]}}.
