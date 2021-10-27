-module(ailib_lists).
-export([search/2,
         merge_left/2,
         merge_right/2,
         foldl/3,
         foldl/4,
         foldr/3,
         foldr/4]).

-spec search(fun((T) -> boolean()), [T]) -> {value, T} | false.
search(Pred, [Hd|Tail]) ->
  case Pred(Hd) of
    true -> {value, Hd};
    false -> search(Pred, Tail)
  end;
search(_Pred, []) -> false.

-spec merge_left(proplists:proplist(),
                 proplists:proplist())-> proplists:proplist().
merge_left(P1,[]) ->P1;
merge_left(P1,[{K,_} = H| T]) ->
  %% if key is defined in left
  %% use the value in the left
  case proplists:is_defined(K,P1) of
    true -> merge_left(P1,T);
    false -> merge_left([H|P1],T)
  end.

-spec merge_right(proplists:proplist(),
                  proplists:proplist())-> proplists:proplist().
merge_right(P1,P2) -> merge_left(P2,P1).


-spec foldl(fun((I,Acc)-> Acc)
           |{atom(),atom()},Acc,[I])-> Acc.
foldl(_Fun,Acc,[])-> Acc;
foldl(Fun,Acc,L)
  when erlang:is_function(Fun)->
  lists:foldl(Fun,Acc,L);
foldl({M,F},Acc,L)-> foldl(M,F,Acc,L).

-spec foldl(atom(),atom(),Acc,[term()])-> Acc.
foldl(_M,_F,Acc,[])-> Acc;
foldl(M,F,Acc,[H|T])->
  Acc0 = M:F(H,Acc),
  foldl(M,F,Acc0,T).

-spec foldr(fun((I,Acc)-> Acc)
           |{atom(),atom()},Acc,[I])-> Acc.
foldr(_Fun,Acc,[])->Acc;
foldr(Fun,Acc,L)
  when erlang:is_function(Fun)->
  lists:foldr(Fun, Acc, L);
foldr({M,F},Acc,L)-> foldr(M,F,Acc,L).

-spec foldr(atom(),atom(),Acc,[term()])-> Acc.
foldr(M,F,Acc,L)->
  L0 = lists:reverse(L),
  foldl(M,F,Acc,L0).
