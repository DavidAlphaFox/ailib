-module(ai_mq_channel).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channel,manager,messages = [], subscribers = [], max_age,
        last_pull, last_purge}).

start_link(MaxAge, Manager, Channel) ->
    gen_server:start_link(?MODULE, [MaxAge, Manager, Channel], []).

init([MaxAge, Manager, Channel]) ->
    {ok, #state{
            max_age = MaxAge,
            manager = Manager,
            channel = Channel,
            messages = gb_trees:empty(),
            last_pull = now_to_micro_seconds(os:timestamp()),
            last_purge = now_to_micro_seconds(os:timestamp()) },
     MaxAge * 1000}.

handle_call(_From, _, State) ->
    {noreply, State}.

handle_cast({From, subscribe, now, Subscriber}, State) ->
    NewSubscribers = add_subscriber(Subscriber, State#state.subscribers),
    gen_server:reply(From, {ok,self(),now_to_micro_seconds(os:timestamp())}),
    {noreply, purge_old_messages(State#state{ subscribers = NewSubscribers })};

handle_cast({From, subscribe, Timestamp, Subscriber}, State) ->
    ActualTimestamp = case Timestamp of
        last -> State#state.last_pull;
        undefined -> 0;
        _ -> Timestamp
    end,
    {NewSubscribers, LastPull} = pull_messages(ActualTimestamp, Subscriber, State),
    gen_server:reply(From, {ok, self(),LastPull}),
    {noreply, purge_old_messages(State#state{ subscribers = NewSubscribers,
                last_pull = LastPull}), State#state.max_age * 1000};

handle_cast({From, pull, Timestamp}, State) ->
    ActualTimestamp = case Timestamp of
        undefined -> 0;
        last -> State#state.last_pull;
        _ -> Timestamp
    end,
    ReturnMessages = messages_newer_than_timestamp(ActualTimestamp, State#state.messages),
    Now = now_to_micro_seconds(os:timestamp()),
    gen_server:reply(From, {ok,self(), Now, ReturnMessages}),
    {noreply, purge_old_messages(State#state{ last_pull = Now }), State#state.max_age * 1000};

handle_cast({From, push, Message}, State) ->
    Now = now_to_micro_seconds(os:timestamp()),
    LastPull = lists:foldr(fun({Ref, Sub}, _) ->
                Sub ! {self(), Now, [Message]},
                erlang:demonitor(Ref),
                Now
        end, State#state.last_pull, State#state.subscribers),
    gen_server:reply(From, {ok,self(), Now}),
    State2 = purge_old_messages(State),
    NewMessages = ai_pq:add(Now, Message, State2#state.messages),
    {noreply, State2#state{messages = NewMessages, subscribers = [], last_pull = LastPull}, State#state.max_age * 1000};

handle_cast({From, now}, State) ->
    gen_server:reply(From, {ok,self(),now_to_micro_seconds(os:timestamp())}),
    {noreply, purge_old_messages(State), State#state.max_age * 1000}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, #state{manager = Manager,subscribers = [] } = State) ->
    gen_server:cast(Manager, {expire, State#state.channel}),
    {stop, normal, State};
handle_info(timeout, State) ->
    {noreply, State, State#state.max_age * 1000};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    handle_info(timeout, State#state{ subscribers = proplists:delete(Ref, State#state.subscribers) });
handle_info(_Info, State) ->
    {noreply, State}.


seconds_to_micro_seconds(Seconds) ->
    Seconds * 1000 * 1000.

now_to_micro_seconds({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + MicroSecs.

messages_newer_than_timestamp(Timestamp, Messages) ->
    collect(fun(V, Acc) -> [V|Acc] end, [], Messages, Timestamp).

purge_old_messages(State) ->
    Now = now_to_micro_seconds(os:timestamp()),
    LastPurge = State#state.last_purge,
    Duration = seconds_to_micro_seconds(1),
    if
        Now - LastPurge > Duration ->
            State#state{
                messages = prune(State#state.messages,
                    Now - seconds_to_micro_seconds(State#state.max_age)),
                last_purge = Now };
        true ->
            State
    end.

pull_messages(Timestamp, Subscriber, State) ->
    Now = now_to_micro_seconds(os:timestamp()),
    case messages_newer_than_timestamp(Timestamp, State#state.messages) of
        ReturnMessages when erlang:length(ReturnMessages) > 0 ->
            Subscriber ! {self(), Now, ReturnMessages},
            {State#state.subscribers, Now};
        _ ->
            {add_subscriber(Subscriber, State#state.subscribers), Now}
    end.

% Checks if the new subscriber pid already has a monitor
add_subscriber(NewSubscriber, Subscribers) ->
        case lists:keymember(NewSubscriber, 2, Subscribers) of
		true -> Subscribers;
		false -> [{erlang:monitor(process, NewSubscriber), NewSubscriber} | Subscribers]
    end.
    


%% @spec collect(Function, Acc0, Tree, Priority) -> Acc1
%% @doc Fold over values with priority greater than `Priority'
collect(Function, Acc0, {_Size, TreeNode}, Priority) ->
    Acc1 = iterate_nonexpired_nodes(Function, Acc0, TreeNode, Priority),
    Acc1.

%% @spec prune(Tree, Priority) -> Tree1
%% @doc Remove nodes with priority less than or equal to `Priority'
prune({Size, TreeNode}, Priority) ->
    {Tree1, NumDeleted} = prune_expired_nodes(TreeNode, Priority),
    {Size - NumDeleted, Tree1}.


iterate_nonexpired_nodes(Function, State, {K, V, S, L}, Now) when K > Now ->
    Acc1 = iterate_nonexpired_nodes(Function, State, L, Now),
    Acc2 = lists:foldr(Function, Acc1, V),
    iterate_nonexpired_nodes(Function, Acc2, S, Now);
iterate_nonexpired_nodes(Function, State, {K, _V, _S, L}, Now) when K =< Now ->
    iterate_nonexpired_nodes(Function, State, L, Now);
iterate_nonexpired_nodes(_Function, State, nil, _Now) ->
    State.


prune_expired_nodes({K, V, S, L}, Now) when K > Now ->
    {Tree1, NumDeleted} = prune_expired_nodes(S, Now),
    {{K, V, Tree1, L}, NumDeleted};
prune_expired_nodes({K, _V, S, L}, Now) when K =< Now ->
    {_, NumDeleted_S} = prune_expired_nodes(S, Now),
    {Tree1, NumDeleted_L} = prune_expired_nodes(L, Now),
    {Tree1, NumDeleted_S + NumDeleted_L + 1};
prune_expired_nodes(nil, _Now) ->
    {nil, 0}.