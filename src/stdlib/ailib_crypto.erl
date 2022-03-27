-module(ailib_crypto).
-export([mac/3,mac/4]).
-export([hmac/3,hmac/4]).

-if(?OTP_RELEASE > 22).
mac(Type,SubType,Key,Data)->crypto:mac(Type,SubType,Key,Data).
mac(Type,Key,Data)->crypto:mac(Type, Key, Data).

hmac(Type,Key,Data)-> crypto:mac(hmac,Type,Key,Data).
hmac(Type,Key,Data,Length) ->
  R = crypto:mac(hmac,Type,Key,Data),
  <<Hash:Length/binary,_/binary>> = R,
  Hash.

-else.

hmac(Type,Key,Data)-> crypto:hmac(Type,Key,Data).
hmac(Type,Key,Data,Length) -> crypto:hmac(Type,Key,Data,Length).

mac(Type,SubType,Key,Data)-> do_mac({Type,SubType},Key,Data).
mac(Type,Key,Data)-> do_mac(Type,Key,Data).

do_mac(poly1305,Key,Data)->
  crypto:poly1305(Key,Data);
do_mac({poly1305,undefined},Key,Data)->
  crypto:poly1305(Key,Data);
do_mac({hmac,SubType},Key,Data) ->
  crypto:hmac(SubType,Key,Data);
do_mac({cmac,SubType},Key,Data) ->
  crypto:cmac(SubType,Key,Data).

-endif.
