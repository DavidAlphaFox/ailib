-module(ailib_hash).
-export([md5/1,
         md5/2,
         sha/1,
         sha/2,
         sha256/1,
         sha256/2,
         sha512/1,
         sha512/2]).

-spec md5(binary()|iolist())->binary().
md5(Data)-> md5(Data,uppper).

-spec sha(binary()|iolist())->binary().
sha(Data)-> sha(Data,uppper).

-spec sha256(binary()|iolist())->binary().
sha256(Data)-> sha256(Data,uppper).

-spec sha512(binary()|iolist())->binary().
sha512(Data)-> sha512(Data,uppper).

-spec md5(binary()|iolist(),upper|lower)->binary().
md5(Data,Case)->
  Hash = crypto:hash(md5, Data),
  ailib_string:to_hex(Hash,128,Case).

-spec sha(binary()|iolist(),upper|lower)->binary().
sha(Data,Case)->
  Hash = crypto:hash(sha,Data),
  ailib_string:to_hex(Hash,160,Case).

-spec sha256(binary()|iolist(),upper|lower)->binary().
sha256(Data,Case) ->
  Hash = crypto:hash(sha256,Data),
  ailib_string:to_hex(Hash,256,Case).

-spec sha512(binary()|iolist(),upper|lower)->binary().
sha512(Data,Case)->
  Hash = crypto:hash(sha512,Data),
  ailib_string:to_hex(Hash,512,Case).

