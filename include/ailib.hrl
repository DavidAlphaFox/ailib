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
