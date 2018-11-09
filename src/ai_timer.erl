-module(ai_timer).
-export([new/0]).
-export([start/3,restart/1,cancel/1]).


-record(ai_timer,{timeout,msg,ref}).

new()-> #ai_timer{timeout = infinity,msg = undefined,ref = undefined}.

start(Timeout,TimeoutMsg,#ai_timer{ref = PrevRef})->
    if PrevRef == undefined -> ok;
       true -> erlang:cancel_timer(PrevRef)
    end,
    Ref = erlang:send_after(Timeout, self(), TimeoutMsg),
    #ai_timer{timeout = Timeout,msg = TimeoutMsg,ref = Ref}.
restart(#ai_timer{timeout = Timeout, msg = TimeoutMsg}  = Timer)->
    if 
        Timeout  == infinity -> {error,infinity};
        TimeoutMsg == undefined -> {error,unknown_message};
        true -> start(Timeout,TimeoutMsg,Timer)
    end.
cancel(#ai_timer{ref = Ref} = Timer)->
	if Ref == undefined -> ok;
       true -> erlang:cancel_timer(Ref)
    end,
    Timer#ai_timer{ref = undefined}.