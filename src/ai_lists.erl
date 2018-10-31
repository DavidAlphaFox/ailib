-module(ai_lists).
-export([search/2]).
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