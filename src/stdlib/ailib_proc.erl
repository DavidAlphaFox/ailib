-module(ailib_proc).
-compile({no_auto_import,
          [get/1,monitor/2,demonitor/2]}).

-export([demonitor/2,monitor/2]).
-export([least_busy/1]).
-export([global/1]).
-export([cached_get/2]).

-spec monitor(Pid :: undefined | pid(),
                      Monitors :: map() | ets:tid()) -> map() | ets:tid().
monitor(undefined,Monitors) -> Monitors;
monitor(Pid,Monitors) when erlang:is_map(Monitors)->
  case maps:get(Pid,Monitors,undefined) of
    undefined ->
      MRef = erlang:monitor(process,Pid),
      M1 = maps:put(Pid,MRef,Monitors),
      maps:put(MRef,Pid,M1);
    _ -> Monitors
  end;
monitor(Pid,Monitors)->
  case ets:match_object(Monitors, {Pid,'_'}) of
    [] ->
      MRef = erlang:monitor(process, Pid),
      ets:insert(Monitors, {Pid, MRef});
    _ -> ok
  end,
  Monitors.
-spec demonitor(Pid :: undefined | identifier(),
                        Monitors :: map() | ets:tid()) -> map() | ets:tid().
demonitor(undefined,Monitors)-> Monitors;
demonitor(Pid,Monitors) when erlang:is_map(Monitors) and erlang:is_pid(Pid) ->
  case maps:get(Pid,Monitors,undefined) of
    undefined -> Monitors;
    MRef ->
      erlang:demonitor(MRef,[flush]),
      M1 = maps:remove(MRef,Monitors),
      maps:remove(Pid,M1)
  end;
demonitor(MRef,Monitors) when erlang:is_map(Monitors) and erlang:is_reference(MRef)->
  erlang:demonitor(MRef,[flush]),
  case maps:get(MRef,Monitors,undefined) of
    undefined -> Monitors;
    Pid ->
      M1 = maps:remove(MRef,Monitors),
      maps:remove(Pid,M1)
  end;
demonitor(Pid,Monitors) when erlang:is_pid(Pid)->
  case ets:match_object(Monitors,{Pid,'_'}) of
    [{Pid,MRef}] ->
      erlang:demonitor(MRef,[flush]),
      ets:delete(Monitors,Pid);
    [] -> ok
  end,
  Monitors;
demonitor(MRef,Monitors) when erlang:is_reference(MRef)->
  case ets:match_object(Monitors,{'_',MRef}) of
    [{Pid,MRef}] ->
      erlang:demonitor(MRef,[flush]),
      ets:delete(Monitors,Pid);
    [] -> ok
   end,
  Monitors.

-spec least_busy(Pids::[pid()]) -> {ok,pid()} | {error,empty_process_group}.
least_busy(Pids) ->
  Members =
    lists:map(
      fun(Pid) ->
          [
           {message_queue_len, Messages},
           {memory,MemorySize}
          ] = erlang:process_info(Pid, [message_queue_len,memory]),
          {Pid, Messages, MemorySize}
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

-spec global(Pid :: pid())-> {atom()|pid(),atom()}.
global(Pid)->
  Name =
    case erlang:process_info(Pid, [registered_name]) of
      [{registered_name, []}] -> Pid;
      [{registered_name, RegName}] -> RegName
    end,
  {Name,erlang:node(Pid)}.

-spec cached_get(term(),function())-> term().
cached_get(Key,Fun)->
  case erlang:get(Key) of
    undefined ->
      R = erlang:apply(Fun,[]),
      erlang:put(Key,R),
      R;
    Cached -> Cached
  end.
