-module(ai_timer).
-export([new/0]).
-export([start/3,restart/1,cancel/1]).
-export([async_restart/1,async_cancel/1]).

-record(ai_timer,{timeout,msg,ref}).

new()-> #ai_timer{timeout = infinity,msg = undefined,ref = undefined}.

start_timer(Timeout,TimeoutMsg,Async,#ai_timer{ref = PrevRef})->
    if PrevRef == undefined -> ok;
        true -> erlang:cancel_timer(PrevRef,[{async,Async}])
    end,
    Ref = erlang:send_after(Timeout, self(), TimeoutMsg),
    #ai_timer{timeout = Timeout,msg = TimeoutMsg,ref = Ref}.

check_rule(Timeout,TimeoutMsg)->
    if 
        Timeout  == infinity -> {error,infinity};
        TimeoutMsg == undefined -> {error,unknown_message};
        true -> true
    end.

start(Timeout,TimeoutMsg,Timer)->
    case check_rule(Timeout,TimeoutMsg) of 
        true -> start_timer(Timeout,TimeoutMsg,false,Timer);
        R -> R
    end.

restart(#ai_timer{timeout = Timeout, msg = TimeoutMsg}  = Timer)->
    case check_rule(Timeout,TimeoutMsg) of 
        true -> start_timer(Timeout,TimeoutMsg,false,Timer);
        R -> R
    end.

async_restart(#ai_timer{timeout = Timeout, msg = TimeoutMsg}  = Timer)
    case check_rule(Timeout,TimeoutMsg) of 
        true -> start_timer(Timeout,TimeoutMsg,true,Timer);
        R -> R
    end.
async_cancel(#ai_timer{ref = Ref} = Timer)->
    if 
        Ref == undefined -> ok;
        true -> erlang:cancel_timer(Ref,[{async,true}])
    end,
    Timer#ai_timer{ref = undefined}.
cancel(#ai_timer{ref = Ref} = Timer)->
	if Ref == undefined -> ok;
       true -> erlang:cancel_timer(Ref)
    end,
    Timer#ai_timer{ref = undefined}.