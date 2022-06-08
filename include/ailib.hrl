-define(TYPE, '__type__').
-define(M, ?MODULE).

-define(IS_TYPE(X), (is_map(X) andalso maps:is_key(?TYPE, X))).
-define(MATCH_TYPE(X), #{?TYPE := X}).