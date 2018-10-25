-module(ai_strings).
-export([to_string/1,to_string/2]).
-export([hash_to_string/3,md5_string/2,sha_string/2,sha256_string/2,sha512_string/2]).

to_string(Val) when is_integer(Val) -> erlang:integer_to_binary(Val);
to_string(Val) when is_float(Val) -> erlang:list_to_binary(io_lib:format("~.2f", [Val]));
to_string(Val) when is_atom(Val) -> erlang:atom_to_binary(Val);
to_string(Val) when is_list(Val)  ->erlang:list_to_binary(Val);
to_string(Val)-> Val.

to_string(Format,Val) ->
		to_string(io_lib:format(Format,[Val])).

md5_string(Data,Case)->
	Hash = crypto:hash(md5, Data),
	hash_to_string(Hash,128,Case).
sha_string(Data,Case)->
	Hash = crypto:hash(sha,Data),
	hash_to_string(Hash,160,Case).
sha256_string(Data,Case) ->
	Hash = crypto:hash(sha256,Data),
	hash_to_string(Hash,256,Case).
sha512_string(Data,Case)->
	Hash = crypto:hash(sha512,Data),
	hash_to_string(Hash,512,Case).

hash_to_string(Hash,Len,Case) ->
	<<S:Len/big-unsigned-integer>> = Hash,
	FormatStr = hash_format(Len,Case),
	erlang:list_to_binary(lists:flatten(io_lib:format(FormatStr,[S]))).


%%%
%%% private
%%%
hash_format(Len,Case)-> 
	Size = Len div 8 * 2,
	Base = "~X.16.0",
	F = 
		case Case of
			upper -> Base ++ "B";
			lower -> Base ++ "b"
		end,
	lists:flatten(io_lib:format(F,[Size,"~"])).

		
