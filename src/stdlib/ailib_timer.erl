-module(ailib_timer).
-export([new/0,new/1]).
-export([start/3,restart/1,cancel/1]).
-export([is_current/2,is_previous/2]).


-record(ailib_timer_state,{timeout,
                           msg,
                           prev_ref,
                           ref,
                           async,
                           info,
                           abs}).
-type state() :: #ailib_timer_state{}.

-export_type([state/0]).

-spec new() -> state().
new()->
  #ailib_timer_state{timeout = infinity,msg = undefined,prev_ref = undefined,
                     ref = undefined,async = false,info = false,abs = false}.
-spec new(list()) -> state().
new(Opts) ->
  Timer =  new(),
  lists:foldl(
    fun(I,Acc)->
        if  I == info -> Acc#ailib_timer_state{info = true};
            I == async -> Acc#ailib_timer_state{async = true};
            I == abs -> Acc#ailib_timer_state{abs = true};
            true -> Acc
        end
    end,Timer,Opts).

cancel_internal(#ailib_timer_state{async = Async,info = Info,ref = Ref} = Timer )->
    if Ref == undefined -> Timer;
       true ->
        erlang:cancel_timer(Ref,[{async,Async},{info,Info}]),
        Timer#ailib_timer_state{prev_ref = Ref,ref = undefined}
    end.

start_internal(Timeout,TimeoutMsg,#ailib_timer_state{ abs = ABS } = Timer)->
  Timer1 = cancel_internal(Timer),
  Ref = erlang:start_timer(Timeout, self(), TimeoutMsg,[{abs,ABS}]),
  Timer1#ailib_timer_state{timeout = Timeout,msg = TimeoutMsg,ref = Ref}.

check_rule(Timeout,TimeoutMsg)->
  if Timeout  == infinity -> {error,infinity};
     TimeoutMsg == undefined -> {error,unknown_message};
     true -> true
  end.

-spec start(integer(),term(),state())-> state()
          | {error,infinity}
          | {error,unknown_message}.
start(Timeout,TimeoutMsg,Timer)->
  case check_rule(Timeout,TimeoutMsg) of
    true -> start_internal(Timeout,TimeoutMsg,Timer);
    R -> R
  end.

-spec restart(state()) -> state()
          | {error,infinity}
          | {error,unknown_message}.
restart(#ailib_timer_state{timeout = Timeout, msg = TimeoutMsg}  = Timer)->
  case check_rule(Timeout,TimeoutMsg) of
    true -> start_internal(Timeout,TimeoutMsg,Timer);
    R -> R
  end.

-spec cancel(state()) -> state().
cancel(Timer)-> cancel_internal(Timer).

-spec is_current(term(),state()) -> boolean().
is_current(Ref,#ailib_timer_state{ref = TRef}) -> Ref == TRef.

-spec is_previous(term(),state()) -> boolean().
is_previous(Ref,#ailib_timer_state{prev_ref = TRef}) ->  Ref == TRef.
