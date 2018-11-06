-module(ai_http).
-export([content_length/1,etag/1,last_modified/1]).
-export([accept_ranges/1,content_range/1,range/1]).

-define(CONTENT_LENGTH,<<"content-length">>).
-define(ETAG,<<"etag">>).
-define(LAST_MODIFIED,<<"last-modified">>).
-define(ACCEPT_RANGES,<<"Accept-Ranges">>).
-define(CONTENT_RANGE,<<"Content-Range">>).
-define(BYTES,<<"bytes">>).
-define(NONE,<<"none">>).
-define(RANGE,<<"range">>).

headers(H) when erlang:is_map(H) -> maps:to_list();
headers(H) -> H.

content_length(Headers)->
    H = headers(Headers),
    case proplists:get_value(?CONTENT_LENGTH,H) of
        undefined -> undefined;
        Length -> erlang:binary_to_integer(Length)
    end.
etag(Headers)->
    H = headers(Headers),
    proplists:get_value(?ETAG,H).
last_modified(Headers)->
    H = headers(Headers),
    proplists:get_value(?LAST_MODIFIED,H).
accept_ranges(Headers)->
    H = headers(Headers),
    case proplists:get_value(?ACCEPT_RANGES,H) of 
        undefined -> false;
        Type -> 
            case Type of 
                ?BYTES -> true;
                ?NONE -> false
            end
    end.
content_range(Headers)->
    H = headers(Headers),
    case proplists:get_value(?CONTENT_RANGE,H) of 
        undefined -> undefined;
        ContentRange -> 
            cow_http_hd:parse_content_range(ContentRange)
    end.               
range(Headers)->
    H = headers(Headers),
    case proplists:get_value(?RANGE,H) of 
        undefined -> undefined;
        Range ->
            cow_http_hd:parse_range(Range)
    end.