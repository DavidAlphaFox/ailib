-module(ai_timer).
-export([new/0]).
-export([start/3,restart/1,cancel/1]).
-export([is_canceled/2]).


-record(ai_timer,{timeout,msg,prev,current}).

new()-> #ai_timer{timeout = infinity,msg = undefined,prev = undefined,current = undefined}.

start(Timeout,TimeoutMsg,#ai_timer{current = PrevRef})->
    if PrevRef == undefined -> ok;
       true -> erlang:cancel_timer(PrevRef)
    end,
    Ref = erlang:send_after(Timeout, self(), TimeoutMsg),
    #ai_timer{timeout = Timeout,msg = TimeoutMsg, prev = PrevRef,current = Ref}.
restart(#ai_timer{timeout = Timeout, msg = TimeoutMsg}  = Timer)->
    if 
        Timeout  == infinity -> {error,infinity};
        TimeoutMsg == undefined -> {error,unknown_message};
        true -> start(Timeout,TimeoutMsg,Timer)
    end.
cancel(#ai_timer{current = Ref} = Timer)->
	if Ref == undefined -> ok;
       true -> erlang:cancel_timer(Ref)
    end,
    Timer#ai_timer{prev = Ref,current = undefined}.
is_canceled(Ref,#ai_timer{current = Ref}) -> false;
is_canceled(_Ref,_Timer) -> true.