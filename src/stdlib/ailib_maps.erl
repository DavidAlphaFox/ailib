-module(ailib_maps).

-export([is_key/2,
         put/3,
         get/2,
         get/3,
         update/3,
         remove/2,
         keys/2,
         append/3]).

-spec is_key([term()],map())-> boolean().
is_key([Key], Map) ->maps:is_key(Key, Map);
is_key([Key|PathRest], Map) ->
  case Map of
    #{Key := SubMap} -> is_key(PathRest, SubMap);
    _ -> false
  end.

-spec get([term()],map()) -> term()|undefined.
get([Key|PathRest], Map) -> get(PathRest, maps:get(Key, Map));
get([], Value) -> Value.

-spec get([term()],map(),term()) -> term().
get([Key|PathRest], Map, Default) ->
  case maps:get(Key, Map, {?MODULE, Default}) of
    {?MODULE, Default} -> Default;
    NestedMap -> get(PathRest, NestedMap, Default)
  end;
get([], Value, _) -> Value.

-spec update([term()],
             term()|fun((I) -> I),
             map()) -> map().
update(Path, ValueOrFun, Map) ->
  try updatef_internal(Path, ValueOrFun, Map)
  catch
    error:{error, {no_map, PathRest, Element}} ->
      PathLength  = length(Path) - length(PathRest),
      PathToThrow = lists:sublist(Path, PathLength),
      erlang:error({no_map, PathToThrow, Element})
  end.

updatef_internal([Key|PathRest], ValueOrFun, Map) when is_map(Map) ->
    maps:update(Key,
                updatef_internal(PathRest, ValueOrFun, maps:get(Key, Map)),
                Map);
updatef_internal([], Fun, OldValue)
  when is_function(Fun) ->
  Fun(OldValue);
updatef_internal([], Value, _) -> Value;
updatef_internal(Path, _, Element) -> error({error, {no_map, Path, Element}}).

-spec put([term()],term(),map())->map().
put([Key|PathRest], Value, Map) ->
  SubMap =
    case maps:is_key(Key, Map) andalso is_map(maps:get(Key, Map)) of
      true ->  maps:get(Key, Map);
      false -> #{}
    end,
  maps:put(Key, put(PathRest, Value, SubMap), Map);
put([], Value, _) -> Value.

-spec remove([term()],map())->map().
remove([], _) -> throw({bad_path, []});
remove([LastKey], Map) -> maps:remove(LastKey, Map);
remove([Key|PathRest], Map) ->
  case maps:is_key(Key, Map) of
    true -> maps:put(Key, remove(PathRest, maps:get(Key, Map)), Map);
    false -> Map
  end.

-spec keys([term()],map())->[term()].
keys([Key|PathRest], Map) -> keys(PathRest, maps:get(Key, Map));
keys([], Map) -> maps:keys(Map).

-spec append([term()],term(),map())->map().
append(Path, Value, Map) ->
  AppendFun =
    fun (List) when is_list(List) -> List ++ [Value];
        (_) -> error(no_list)
    end,
  update(Path, AppendFun, Map).
