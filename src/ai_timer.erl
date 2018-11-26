-module(ai_timer).
-export([new/0,new/1]).
-export([start/3,restart/1,cancel/1]).
-export([is_current/2,is_previous/2]).


-record(ai_timer,{timeout,msg,prev_ref,ref,async,info}).

new()-> #ai_timer{timeout = infinity,msg = undefined,prev_ref = undefined,
                    ref = undefined,async = false,info = false}.
new(Opts) ->
    Timer =  new(),
    lists:foldl(fun(I,Acc)->
        if  I == info -> Acc#ai_timer{info = true};
            I == async -> Acc#ai_timer{async = true};
            true -> Acc
        end
    end,Timer,Opts).

cancel_internal(#ai_timer{async = Async,info = Info,ref = Ref} = Timer )->
    if 
        Ref == undefined -> Timer;
        true -> 
            erlang:cancel(Ref,[{async,Async},{info,Info}]),
            Timer#ai_timer{prev_ref = Ref,ref = undefined}
    end.

start_internal(Timeout,TimeoutMsg,Timer)->
    Timer1 = cancel_internal(Timer),
    Ref = erlang:start_timer(Timeout, self(), TimeoutMsg),
    Timer1#ai_timer{timeout = Timeout,msg = TimeoutMsg,ref = Ref}.

check_rule(Timeout,TimeoutMsg)->
    if 
        Timeout  == infinity -> {error,infinity};
        TimeoutMsg == undefined -> {error,unknown_message};
        true -> true
    end.

start(Timeout,TimeoutMsg,Timer)->
    case check_rule(Timeout,TimeoutMsg) of 
        true -> start_internal(Timeout,TimeoutMsg,Timer);
        R -> R
    end.

restart(#ai_timer{timeout = Timeout, msg = TimeoutMsg}  = Timer)->
    case check_rule(Timeout,TimeoutMsg) of 
        true -> start_internal(Timeout,TimeoutMsg,Timer);
        R -> R
    end.

cancel( Timer)-> cancel_internal(Timer).

is_current(Ref,#ai_timer{ref = TRef}) -> Ref == TRef.

is_previous(Ref,#ai_timer{prev_ref = TRef}) ->  Ref == TRef.