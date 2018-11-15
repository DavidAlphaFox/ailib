-module(ai_mq_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([new/0,new/1,new/2,new/3]).
-export([subscribe/3,pull/3,push/3,now/2]).

-define(SUFFIX,"_ai_mq_manager").

-record(state, {dict, max_age,supervisor}).

-spec new()-> {ok,pid()}.
new()->
	Opts = [],
	start_link(Opts).

-spec new(Name :: atom() | list())-> {ok,pid()}.
new(Name)->
    Opts = [{name,ai_string:atom_suffix(Name,?SUFFIX,false)}],
    start_link(Opts).

new(Name,Sup)->
    Opts = [{name,ai_string:atom_suffix(Name,?SUFFIX,false)},{supervisor,Sup}],
    start_link(Opts).
new(Name,Sup,MaxAge)->
    Opts = [{name,ai_string:atom_suffix(Name,?SUFFIX,false)},
            {supervisor,Sup},{max_age,MaxAge}],
    start_link(Opts).


subscribe(Manager,Channel,Timestamp) when erlang:is_pid(Manager) ->
    do_subscribe(Manager,Channel,Timestamp);

subscribe(Manager,Channel,Timestamp) ->
    Name = server_name(Manager),
    do_subscribe(Name,Channel,Timestamp).

do_subscribe(Manager,Channel,Timestamp)->
    Subscriber = self(),
    gen_server:call(Manager,{subscribe,Channel,Timestamp,Subscriber}).

pull(Manager,Channel,Timestamp) when erlang:is_pid(Manager) ->
    do_pull(Manager,Channel,Timestamp);
pull(Manager,Channel, Timestamp) ->
    Name = server_name(Manager),
    do_pull(Name,Channel,Timestamp).

do_pull(Manager,Channel,Timestamp)->
    gen_server:call(Manager, {pull, Channel, Timestamp}).

push(Manager,Channel,Message)when erlang:is_pid(Manager) ->
    do_push(Manager,Channel,Message);
push(Manager,Channel,Message)->
    Name = server_name(Manager),
    do_push(Name,Channel,Message).
do_push(Manager,Channel, Message) ->
    gen_server:call(Manager, {push, Channel, Message}).

now(Manager,Channel) when erlang:is_pid(Manager)->
    do_now(Manager,Channel);
now(Manager,Channel) ->
    Name = server_name(Manager),
    do_now(Name,Channel).
do_now(Manager,Channel)->
    gen_server:call(Manager, {now, Channel}).

-spec start_link(Opts :: proplists:proplists()) -> {ok, Pid :: pid()} |
	{error, Error :: {already_started, pid()}} |
	{error, Error :: term()} |
	ignore.
start_link(Opts) ->
    Name = proplists:get_value(name,Opts),
    case Name of
        undefined -> gen_server:start_link(?MODULE, Opts, []);
        _ -> start_link(Name,proplists:delete(name, Opts))
    end.

-spec start_link(Name :: atom(),Opts :: proplists:proplists()) -> {ok, Pid :: pid()} |
											{error, Error :: {already_started, pid()}} |
											{error, Error :: term()} |
											ignore.
start_link(Name,Opts) ->
	gen_server:start_link({local,Name}, ?MODULE, Opts, []).


init(Args) ->
    MaxAgeSeconds = proplists:get_value(max_age,Args,60),
    Supervisor = proplists:get_value(supervisor,Args,ai_mq_channel_sup),
    {ok, #state{dict = dict:new(), max_age = MaxAgeSeconds,supervisor = Supervisor}}.

handle_call({subscribe, Channel, Timestamp, Subscriber}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, subscribe, Timestamp, Subscriber}),
    {noreply, NewState};

handle_call({pull, Channel, Timestamp}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, pull, Timestamp}),
    {noreply, NewState};

handle_call({push, Channel, Message}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, push, Message}),
    {noreply, NewState};

handle_call({now, Channel}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, now}),
    {noreply, NewState}.

handle_cast({expire, Channel}, State) ->
    NewState = State#state{
        dict = dict:erase(Channel, State#state.dict)},
    {noreply, NewState};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.


% internal
server_name(Manager) -> ai_string:atom_suffix(Manager,?SUFFIX,true).

find_or_create_channel(Channel, #state{dict = Chan2Pid, max_age = MaxAge,supervisor = Sup} = State) ->
    Self = self(),
    case dict:find(Channel, Chan2Pid) of
        {ok, Pid} ->
            {Pid, State};
        _ ->
            {ok, ChannelPid} = supervisor:start_child(Sup, [MaxAge,Self,Channel]),
            {ChannelPid, State#state{
                    dict = dict:store(Channel, ChannelPid, Chan2Pid)
                }}
    end.