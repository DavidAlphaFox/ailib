-module(ai_mq_queue).

-behaviour(gen_server).
-compile({inline,[
                  do_subscribe/3,
                  do_pull/3,
                  do_push/3,
                  do_now/2
                 ]}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([new/0,new/1,new/2,
         subscribe/3,pull/3,push/3,now/2]).

-define(PREFIX,"ai_mq_queue_").
-record(state, {
                max_age,
                channels,
                rev_channels
               }).

-spec new()-> {ok,pid()}.
new()->
  Opts = [],
  ai_mq_sup:new(Opts).

-spec new(Name :: atom() | list() | binary())-> {ok,pid()}.
new(Name)->
  Opts = [{name,ai_string:atom_prefix(Name,?PREFIX,false)}],
  ai_mq_sup:new(Opts).

-spec new(Name :: atom() | list() | binary(),MaxAge::integer())-> {ok,pid()}.
new(Name,MaxAge)->
  Opts = [{name,ai_string:atom_prefix(Name,?PREFIX,false)},
          {max_age,MaxAge}],
  ai_mq_sup:new(Opts).

-spec subscribe(Queue::pid()|atom(),Channel::atom()|string()|binary(),
                Timestamp :: integer())-> {ok,pid(),integer()}.
subscribe(Queue,Channel,Timestamp) when erlang:is_pid(Queue) ->
  do_subscribe(Queue,Channel,Timestamp);
subscribe(Queue,Channel,Timestamp) ->
  Name = server_name(Queue),
  do_subscribe(Name,Channel,Timestamp).

do_subscribe(Queue,Channel,Timestamp)->
  Subscriber = self(),
  gen_server:call(Queue,{subscribe,Channel,Timestamp,Subscriber}).

-spec pull(Queue::pid()|atom(),Channel::atom()|string()|binary(),
           Timestamp :: integer())-> {ok,pid(),integer(),list()}.
pull(Queue,Channel,Timestamp) when erlang:is_pid(Queue) ->
  do_pull(Queue,Channel,Timestamp);
pull(Queue,Channel, Timestamp) ->
  Name = server_name(Queue),
  do_pull(Name,Channel,Timestamp).

do_pull(Queue,Channel,Timestamp)->
  gen_server:call(Queue, {pull, Channel, Timestamp}).

-spec push(Queue::pid()|atom(),Channel::atom()|string()|binary(),
           Message::term())-> {ok,pid(),integer()}.
push(Queue,Channel,Message)when erlang:is_pid(Queue) ->
  do_push(Queue,Channel,Message);
push(Queue,Channel,Message)->
  Name = server_name(Queue),
  do_push(Name,Channel,Message).
do_push(Queue,Channel, Message) ->
  gen_server:call(Queue, {push, Channel, Message}).

-spec now(Queue::pid()|atom(),Channel::atom()|string()|binary())->{ok,pid(),integer()}.
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
  process_flag(trap_exit,true),
  MaxAgeSeconds = proplists:get_value(max_age,Args,60),
  {ok, #state{
          max_age = MaxAgeSeconds,
          channels = maps:new(),
          rev_channels = maps:new()
         }}.

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

handle_cast({expire, Channel},
            #state{ channels = Chan2Pid,
                    rev_channels = Pid2Chan } = State) ->
  NewState =
    case maps:get(Channel,Chan2Pid,undefined) of
      undefined -> State;
      Pid ->
        Pid2Chan0 = maps:remove(Pid, Pid2Chan),
        Chan2Pid0 = maps:remove(Channel,Chan2Pid),
        State#state{
          channels = Chan2Pid0,
          rev_channels = Pid2Chan0
         }
    end,
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
find_or_create_channel(Channel, #state{channels = Chan2Pid, max_age = MaxAge,rev_channels = Pid2Chan} = State) ->
  Self = self(),
  case maps:get(Channel, Chan2Pid,undefined) of
    undefined ->
      {ok, ChannelPid} = ai_mq_channel:start_link(MaxAge, Self, Channel),
      {ChannelPid, State#state{
                     channels = maps:put(Channel,ChannelPid, Chan2Pid),
                     rev_channels = maps:put(ChannelPid,Channel,Pid2Chan)
                    }};
    Pid -> {Pid, State}
  end.
