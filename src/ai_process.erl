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
demonitor_process(MRef,Monitors)->
    erlang:demonitor(MRef),
    case maps:get(MRef,Monitors,undefined) of 
        undefined -> Monitors;
        Pid ->
            M1 = maps:remove(MRef,Monitors),
            maps:remove(Pid,M1)
    end.