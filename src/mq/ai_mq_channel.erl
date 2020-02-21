-module(ai_mq_channel).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channel,manager,messages = [], subscribers = [], max_age,
        last_pull, last_purge}).

start_link(MaxAge, Manager, Channel) ->
    gen_server:start_link(?MODULE, [MaxAge, Manager, Channel], []).

init([MaxAge, Manager, Channel]) ->
  Now = erlang:system_time(millisecond),
  {ok, #state{
          max_age = MaxAge,
          manager = Manager,
          channel = Channel,
          messages = gb_trees:empty(),
          last_pull = Now,
          last_purge = Now },
   timer:seconds(MaxAge)}.

handle_call(_From, _, State) -> {noreply, State}.

handle_cast({From, subscribe, now, Subscriber}, State) ->
  NewSubscribers = add_subscriber(Subscriber, State#state.subscribers),
  gen_server:reply(From, {ok,self(),erlang:system_time(millisecond)}),
  {noreply, purge_old_messages(State#state{ subscribers = NewSubscribers })};

handle_cast({From, subscribe, Timestamp, Subscriber}, State) ->
  ActualTimestamp =
    case Timestamp of
      last -> State#state.last_pull;
      undefined -> 0;
      _ -> Timestamp
    end,
  {NewSubscribers, LastPull} = pull_messages(ActualTimestamp, Subscriber, State),
  gen_server:reply(From, {ok, self(),LastPull}),
  {noreply, purge_old_messages(State#state{
                                 subscribers = NewSubscribers,last_pull = LastPull}),
   timer:seconds(State#state.max_age)};

handle_cast({From, pull, Timestamp}, State) ->
  ActualTimestamp =
    case Timestamp of
      undefined -> 0;
      last -> State#state.last_pull;
      _ -> Timestamp
    end,
  ReturnMessages = messages_newer_than_timestamp(ActualTimestamp, State#state.messages),
  Now = erlang:system_time(millisecond),
  gen_server:reply(From, {ok,self(), Now, ReturnMessages}),
  {noreply, purge_old_messages(State#state{ last_pull = Now }),
   timer:seconds(State#state.max_age)};


handle_cast({From, push, Message}, State) ->
  Now = erlang:system_time(millisecond),
  %% 遍历所有的订阅者，发送该消息
  lists:foreach(
    fun({Ref, Sub}) ->
        Sub ! {self(), Now, [Message]},
        erlang:demonitor(Ref)
    end, State#state.subscribers),
  gen_server:reply(From, {ok, self(), Now}),
  State2 = purge_old_messages(State),
  NewMessages = ai_pq:add(Now, Message, State2#state.messages),
  %% 清空所有的订阅者
  {noreply, State2#state{messages = NewMessages, subscribers = [], last_pull = Now},
   timer:seconds(State#state.max_age)};

handle_cast({From, now}, State) ->
    gen_server:reply(From, {ok,self(),erlang:system_time(millisecond)}),
  {noreply, purge_old_messages(State), timer:seconds(State#state.max_age)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, #state{manager = Manager,subscribers = [] } = State) ->
    gen_server:cast(Manager, {expire, State#state.channel}),
    {stop, normal, State};
handle_info(timeout, State) ->
  {noreply, State, timer:seconds(State#state.max_age)};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    handle_info(timeout, State#state{ subscribers = proplists:delete(Ref, State#state.subscribers) });
handle_info(_Info, State) ->
    {noreply, State}.


messages_newer_than_timestamp(Timestamp, Messages) ->
  ai_pq:collect(Messages, Timestamp).

purge_old_messages(State) ->
  Now = erlang:system_time(millisecond),
  LastPurge = State#state.last_purge,
  Duration = timer:seconds(1),
  if
    Now - LastPurge > Duration ->
      State#state{
        messages = ai_pq:prune(State#state.messages,
                         Now - timer:seconds(State#state.max_age)),
        last_purge = Now };
    true -> State
  end.

pull_messages(Timestamp, Subscriber, State) ->
  Now = erlang:system_time(millisecond),
  case messages_newer_than_timestamp(Timestamp, State#state.messages) of
    ReturnMessages when erlang:length(ReturnMessages) > 0 ->
      Subscriber ! {self(), Now, ReturnMessages},
      {State#state.subscribers, Now};
    _ ->
      {add_subscriber(Subscriber, State#state.subscribers), Now}
  end.

% CHecks if the new subscriber pid already has a monitor
add_subscriber(NewSubscriber, Subscribers) ->
  case lists:keymember(NewSubscriber, 2, Subscribers) of
		true -> Subscribers;
		false -> [{erlang:monitor(process, NewSubscriber), NewSubscriber} | Subscribers]
  end.



