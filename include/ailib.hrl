-record(rfc822_date,{
                     year :: binary(),
                     month :: binary(),
                     date :: integer(),
                     day :: binary(),                    
                     hour :: integer(),
                     minute :: integer(),
                     second :: integer(),
                     zone :: binary(),
                     utc_diff :: {integer(),integer()}
                    }).

-record(ai_http_cache,{ 
                    key :: binary(), 
                    cache_key :: term(),
                    date :: binary(),
                    etag :: binary(),
                    strategy :: atom(), %% no_cache | age
                    age :: integer(),
                    last_modified :: binary(),
                    headers :: term()
                }).
