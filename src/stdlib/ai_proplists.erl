-module(ai_proplists).
-export([merge/2]).

merge(P1,[]) ->P1;
merge(P1,[{K,_} = H| T]) ->
    case proplists:is_defined(K,P1) of
        true -> merge(P1,T);
        false -> merge([H|P1],T)
    end.
