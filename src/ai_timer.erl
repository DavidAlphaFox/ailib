-module(ai_timer).
-export([new/0]).
-export([timer/3,cancel_timer/1]).
-export([is_canceled/2]).


-record(ai_timer,{prev,current}).

new()-> #ai_timer{prev = undefined,current = undefined}.

timer(Timeout,TimeoutMsg,#ai_timer{current = PrevRef})->
    if PrevRef == undefined -> ok;
       true -> erlang:cancel_timer(PrevRef)
    end,
    Ref = erlang:send_after(Timeout, self(), TimeoutMsg),
    #ai_timer{prev = PrevRef,current = Ref}.
cancel_timer(#ai_timer{current = Ref})->
	if Ref == undefined -> ok;
       true -> erlang:cancel_timer(Ref)
    end,
    #ai_timer{prev = Ref,current = undefined}.
is_canceled(Ref,#ai_timer{current = Ref}) -> false;
is_canceled(_Ref,_Timer) -> true.