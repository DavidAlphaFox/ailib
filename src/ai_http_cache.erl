-module(ai_http_cache).
-include("ailib.hrl").
%% only support http 1.1
-export([validate_hit/1,hit/1,cache/3,cache/2]).

-define(CACHE_CONTROL,<<"cache-control">>).
-define(NO_STORE,<<"no-store">>).
-define(NO_CACHE,<<"no-cache">>).
-define(ETAG,<<"etag">>).
-define(LAST_MODIFIED,<<"last-modified">>).
-define(DATE,<<"date">>).

find(Table,Key) ->
    F = fun() ->
            mnesia:read(Table,Key)
        end,
    ai_mnesia_operation:error_ignore_transaction(F).

write(Rec) ->
    F = fun() ->
        mnesia:write(Rec)
    end,
    case mnesia:transaction(F) of
        {atomic,ok} -> ok;
        Res -> Res
    end.
            
-spec hit(Key:: binary()) -> {ok,tuple()} | not_found.
hit(Key)->
    case find(ai_http_cache,Key) of
        [] -> not_found;
        [C] -> {ok,C}
    end.
-spec validate_hit(Key:: binary()) -> {ok,tuple()} | not_found.
validate_hit(Key)->
    case hit(Key) of
        not_found -> not_found;
        {ok,C} -> cache_validate(C)
    end.

can_cache(Headers) ->
    CacheControl = proplists:get_value(?CACHE_CONTROL,Headers),
    can_cache(CacheControl,Headers).
can_cache(undefined,_Headers) -> false;
can_cache(?NO_STORE,_Headers) -> false;
can_cache(?NO_CACHE,Headers) -> no_cache(Headers);
can_cache(<<"private,",_Rest/binary>>,_Headers) -> false;
can_cache(<<"public, ",MaxAge/binary>>, Headers) ->
    case binary:split(MaxAge,<<"=">>) of 
        [_H,Age] -> {true,age, erlang:binary_to_integer(Age)};
        _ -> no_cache(Headers)
    end;
can_cache(<<"max-age=",Age/binary>>,_Headers)-> {true,age,erlang:binary_to_integer(Age)};
can_cache(_Any,Headers) -> no_cache(Headers).

no_cache(Headers)->
    case proplists:get_value(?ETAG,Headers) of 
        undefined -> false;
        ETag -> {true,no_cache,ETag}
    end.

age_validate(no_cache,_Date,_MaxAge)-> expired;
age_validate(age,undefined,_MaxAge) -> expired;
age_validate(age,_Date,undefined) -> expired;
age_validate(aga,Date,MaxAge)->
    CachedData = ai_rfc822_date:universal_time(ai_rfc822_date:parse(Date)),
    Now = calendar:universal_time(),
    Age = calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(CachedData),
    if
        Age - MaxAge > 0 -> expired;
        true -> hit
    end.


cache_validate(#ai_http_cache{date = Date,strategy =  Strategy,age = MaxAge} = C)->
    case age_validate(Strategy,Date,MaxAge) of
        expired -> {expired,C#ai_http_cache.etag,C#ai_http_cache.last_modified};
        hit -> {hit,C#ai_http_cache.cache_key,C#ai_http_cache.headers}
    end.
    
fields_need({?DATE,V},Acc)->Acc#ai_http_cache{date = V};
fields_need({?ETAG,V},Acc) ->Acc#ai_http_cache{etag = V};
fields_need({?LAST_MODIFIED,V},Acc) -> Acc#ai_http_cache{last_modified = V};
fields_need(_,Acc) -> Acc.
fileds(Headers,Item)->    
    lists:foldl(fun fields_need/2,Item,Headers).

cache(Key,CacheKey,Headers)->
    case can_cache(Headers) of 
        false -> ok;
        {true,age,MaxAge} ->
            Item = #ai_http_cache{key = Key,cache_key = CacheKey,headers = Headers,strategy = age,age = MaxAge},
            CacheItem = fileds(Headers,Item),
            write(CacheItem);
        {true,no_cache,ETag}->
            Item = #ai_http_cache{key = Key,cache_key = CacheKey,headers = Headers,strategy = no_cache,etag = ETag},
            CacheItem = fileds(Headers,Item),
            write(CacheItem)
    end.
cache(Key,Headers)->
    case hit(Key) of 
        {ok,Item} ->
            cache(Key,Item#ai_http_cache.cache_key,Headers);
        not_found -> ok
    end.