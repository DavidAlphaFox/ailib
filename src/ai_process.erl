-module(ai_process).
-export([demonitor_process/2,monitor_process/2]).

monitor_process(undefined,Monitors) -> Monitors;
monitor_process(Pid,Monitors)->
    case maps:get(Pid,Monitors,undefined) of 
        undefined ->
            MRef = erlang:monitor(process,Pid),
            M1 = maps:put(Pid,MRef,Monitors),
            maps:put(MRef,Pid,M1);
        _ -> Monitors
    end.

demonitor_process(undefined,Monitors)-> Monitors;
demonitor_process(Pid,Monitors) when erlang:is_pid(Pid)-> 
    case maps:get(Pid,Monitors,undefined) of 
        undefined -> Monitors;
        MRef -> 
            erlang:demonitor(MRef),
            M1 = maps:remove(MRef,Monitors),
            maps:remove(Pid,M1)
    end;
demonitor_process(MRef,Monitors) when erlang:is_reference(MRef)->
    erlang:demonitor(MRef),
    case maps:get(MRef,Monitors,undefined) of 
        undefined -> Monitors;
        Pid ->
            M1 = maps:remove(MRef,Monitors),
            maps:remove(Pid,M1)
    end.