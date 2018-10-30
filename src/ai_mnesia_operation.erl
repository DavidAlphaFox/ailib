-module(ai_mnesia_operation).
-export([error_ignore_transaction/1,error_ignore_transaction/2]).
-export([one_or_none/1]).

error_ignore_transaction(F)->
    error_ignore_transaction(F,[]).
error_ignore_transaction(F,Default)->
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, _Reason} -> Default
    end.	

one_or_none({atomic,[One]})-> One;
one_or_none({atomic,[]}) -> not_found.