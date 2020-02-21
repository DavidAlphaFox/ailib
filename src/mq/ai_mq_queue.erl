-module(ai_mq_queue).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([new/0,new/1,new/2,new/3]).
-export([subscribe/3,pull/3,push/3,now/2]).

-define(PREFIX,"ai_mq_queue_").

-record(state, {dict, max_age,supervisor}).

-spec new()-> {ok,pid()}.
new()->
  Opts = [],
  ai_mq_queue_sup:new(Opts).
-spec new(Name :: atom() | list())-> {ok,pid()}.
new(Name)->
  Opts = [{name,ai_string:atom_prefix(Name,?PREFIX,false)}],
  ai_mq_queue_sup:new(Opts).

new(Name,Sup)->
  Opts = [{name,ai_string:atom_prefix(Name,?PREFIX,false)},{supervisor,Sup}],
  ai_mq_queue_sup:new(Opts).

new(Name,Sup,MaxAge)->
  Opts = [{name,ai_string:atom_prefix(Name,?PREFIX,false)},
          {supervisor,Sup},{max_age,MaxAge}],
  start_link(Opts).

-spec subscribe(Queue::pid()|atom(),Channel::term(),Timestamp :: integer())-> ok.
subscribe(Queue,Channel,Timestamp) when erlang:is_pid(Queue) ->
  do_subscribe(Queue,Channel,Timestamp);
subscribe(Queue,Channel,Timestamp) ->
  Name = server_name(Queue),
  do_subscribe(Name,Channel,Timestamp).

do_subscribe(Queue,Channel,Timestamp)->
  Subscriber = self(),
  gen_server:call(Queue,{subscribe,Channel,Timestamp,Subscriber}).

pull(Queue,Channel,Timestamp) when erlang:is_pid(Queue) ->
  do_pull(Queue,Channel,Timestamp);
pull(Queue,Channel, Timestamp) ->
  Name = server_name(Queue),
  do_pull(Name,Channel,Timestamp).

do_pull(Queue,Channel,Timestamp)->
  gen_server:call(Queue, {pull, Channel, Timestamp}).

push(Queue,Channel,Message)when erlang:is_pid(Queue) ->
  do_push(Queue,Channel,Message);
push(Queue,Channel,Message)->
  Name = server_name(Queue),
  do_push(Name,Channel,Message).
do_push(Queue,Channel, Message) ->
  gen_server:call(Queue, {push, Channel, Message}).

now(Queue,Channel) when erlang:is_pid(Queue)->
  do_now(Queue,Channel);
now(Queue,Channel) ->
  Name = server_name(Queue),
  do_now(Name,Channel).
do_now(Queue,Channel)->
  gen_server:call(Queue, {now, Channel}).

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
server_name(Queue) -> ai_string:atom_prefix(Queue,?PREFIX,true).

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
