-module(ai_process).
-export([demonitor_process/2,monitor_process/2]).
-export([least_busy/1]).

monitor_process(undefined,Monitors) -> Monitors;
monitor_process(Pid,Monitors) when erlang:is_map(Monitors)->
    case maps:get(Pid,Monitors,undefined) of 
        undefined ->
            MRef = erlang:monitor(process,Pid),
            M1 = maps:put(Pid,MRef,Monitors),
            maps:put(MRef,Pid,M1);
        _ -> Monitors
    end;
monitor_process(Pid,Monitors)->
    case ets:match_object(Monitors, {Pid,'_'}) of
        [] ->
            MRef = erlang:monitor(process, Pid),
            ets:insert(Monitors, {Pid, MRef});
        _ -> ok
    end,
    Monitors.

demonitor_process(undefined,Monitors)-> Monitors;
demonitor_process(Pid,Monitors) when erlang:is_map(Monitors) and erlang:is_pid(Pid) -> 
    case maps:get(Pid,Monitors,undefined) of 
        undefined -> Monitors;
        MRef -> 
            erlang:demonitor(MRef,[flush]),
            M1 = maps:remove(MRef,Monitors),
            maps:remove(Pid,M1)
    end;
demonitor_process(MRef,Monitors) when erlang:is_map(Monitors) and erlang:is_reference(MRef)->
    erlang:demonitor(MRef,[flush]),
    case maps:get(MRef,Monitors,undefined) of 
        undefined -> Monitors;
        Pid ->
            M1 = maps:remove(MRef,Monitors),
            maps:remove(Pid,M1)
    end;
demonitor_process(Pid,Monitors) when erlang:is_pid(Pid)->
    case ets:match_object(Monitors,{Pid,'_'}) of
        [{Pid,MRef}] ->
            erlang:demonitor(MRef,[flush]),
            ets:delete(Monitors,Pid);
         [] -> ok
   end,
   Monitors;
demonitor_process(MRef,Monitors) when erlang:is_reference(MRef)->
        case ets:match_object(Monitors,{'_',MRef}) of
        [{Pid,MRef}] ->
            erlang:demonitor(MRef,[flush]),
            ets:delete(Monitors,Pid);
         [] -> ok
   end,
   Monitors.

least_busy(Pids) ->
    Members = lists:map(fun(Pid) ->
                [
                    {message_queue_len, Messages},
                    {stack_size, StackSize}
                ] = erlang:process_info(Pid, [message_queue_len, stack_size]),
                {Pid, Messages, StackSize}
            end, Pids),
    SortedMembers = lists:keysort(2, lists:keysort(3, Members)),
    case SortedMembers of
        [{Pid, _Messages, _StackSize}] -> {ok,Pid};
        [{Pid, _Messages, _StackSize} | Tail] -> 
            case erlang:is_process_alive(Pid) of 
                true -> {ok,Pid};
                false ->
                    case lists:search(fun({Maybe,_M0,_S0})->
                            erlang:is_process_alive(Maybe)
                        end,Tail) of 
                            {value,{Found,_M0,_S0}} -> {ok,Found};
                            false -> {error, empty_process_group}
                    end
            end;
        _ -> {error, empty_process_group}
    end.
