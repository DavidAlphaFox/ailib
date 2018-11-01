-module(ai_lists).
-export([search/2,break_on_error/1,run_pipe/2,run_pipe/1]).
-spec search(Pred, List) -> {value, Value} | false when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      Value :: T.

search(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> {value, Hd};
        false -> search(Pred, Tail)
    end;
search(Pred, []) when is_function(Pred, 1) -> false.

-spec break_on_error(List :: [{term(),term()}]) -> {error,term()} | ok.
break_on_error([H|T])->
    {Fun,Pred} = H,
    R = ai_function:run_mfa(Fun),
    case ai_function:run_mfa(Pred,[R]) of 
        false -> R;
        true -> break_on_error(T)
    end;
break_on_error([]) -> ok.

-spec run_pipe(List :: [term()]) -> term().
run_pipe(L)-> run_pipe(L,[]).

-spec run_pipe(List :: [term()],Args :: [term()]) -> term().
run_pipe([H|T],Args)->
    R = ai_function:run_mfa(H,Args),
    run_pipe(T,[R]);
run_pipe([],[R]) -> R.
