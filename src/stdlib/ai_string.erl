-module(ai_string).

-export([to_string/1,to_string/2]).
-export([to_integer/1,to_boolean/1]).
-export([md5_string/2,sha_string/2,sha256_string/2,sha512_string/2]).
-export([string_to_hex/2,hash_to_string/3]).
-export([atom_suffix/3,join/2]).
-export([dynamic_module/2]).
-export([html_escape/1,html_unescape/1]).
-export([to_iolist/1]).

-define(HTML_ESCAPE,[
		{"\\&","\\&amp"},{"<","\\&lt;"},{">","\\&gt;"},
		{"\"","\\&quot;"},{"'","\\&#39;"},{"/","\\&#x2F;"},
		{"=","\\&#x3D;"},
		{"`","\\&#x60;"} %% delimiter in IE
	]).


html_escape(Str)->
	BinStr = to_string(Str),
	lists:foldl(fun({El,Replace},Acc)->
			re:replace(Acc,El,Replace,[global,{return,binary}])
		end,BinStr,?HTML_ESCAPE).
html_unescape(Str)->
	BinStr = to_string(Str),
	lists:foldr(fun({Replace,El},Acc)->
			re:replace(Acc,El,Replace,[global,{return,binary}])
		end,BinStr,?HTML_ESCAPE).
-spec dynamic_module(Name :: list(),Content :: list())->  {module,  atom()} | {error, term()}.
dynamic_module(Name,Content)->
    {Mod, Code} = ai_dynamic_compile:from_string(Content),
    code:load_binary(Mod, Name, Code).


to_boolean(<<"true">>) -> true;
to_boolean(<<"false">>) -> false;
to_boolean(<<"1">>) -> true;
to_boolean(<<"0">>) -> false;
to_boolean(<<"T">>) -> true;
to_boolean(<<"F">>) -> false;
to_boolean(<<"t">>) -> true;
to_boolean(<<"f">>) -> false;
to_boolean(1) -> true;
to_boolean(0) -> false;
to_boolean(true)-> true;
to_boolean(false)-> false.

to_integer(Val) when erlang:is_binary(Val) -> erlang:binary_to_integer(Val);
to_integer(Val) when erlang:is_list(Val) -> erlang:list_to_integer(Val);
to_integer(Val) when erlang:is_integer(Val) -> Val.

to_string(Val) when erlang:is_integer(Val) -> erlang:integer_to_binary(Val);
to_string(Val) when erlang:is_float(Val) -> erlang:list_to_binary(io_lib:format("~f", [Val]));
to_string(Val) when erlang:is_boolean(Val) -> erlang:atom_to_binary(Val,latin1);
to_string(Val) when erlang:is_atom(Val) -> erlang:atom_to_binary(Val,latin1);
%% iolist场景
to_string(Val) when erlang:is_list(Val)  ->
	try 
		erlang:iolist_to_binary(Val)
	catch
		_Reason:_Error -> unicode:characters_to_binary(Val)
	end;
to_string(Val) when erlang:is_binary(Val)-> Val.

to_string(Format,Val) ->
	to_string(io_lib:format(Format,[Val])).


to_iolist(L) when erlang:is_list(L)->
	B = 
		try 
			erlang:iolist_to_binary(L)
		catch
			_Reason:_Error -> unicode:characters_to_binary(L)
		end,
	to_iolist(B);
to_iolist(B)-> erlang:binary_to_list(B).

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

string_to_hex(L,Case) when erlang:is_list(L)->
	B = erlang:list_to_binary(L),
	string_to_hex(B,Case);
string_to_hex(B,Case)->
	Size = erlang:byte_size(B) * 2,
	Bits = erlang:byte_size(B) * 8,
	<<S:Bits/unsigned-integer>> = B,
	FormatStr = hex_format(Size,Case),
	erlang:list_to_binary(lists:flatten(io_lib:format(FormatStr,[S]))).

hash_to_string(Hash,Len,Case) ->
	<<S:Len/big-unsigned-integer>> = Hash,
	Size = Len div 8 * 2,
	FormatStr = hex_format(Size,Case),
	erlang:list_to_binary(lists:flatten(io_lib:format(FormatStr,[S]))).

-spec atom_suffix(Name :: atom(),Suffix :: string(),Exist :: boolean()) -> atom().
atom_suffix(Name,Suffix,Exist) when erlang:is_binary(Suffix)->
	atom_suffix(Name, erlang:binary_to_list(Suffix),Exist);
atom_suffix(Name,Suffix,Exist) when erlang:is_atom(Name)->
	atom_suffix(erlang:atom_to_list(Name),Suffix,Exist);
atom_suffix(Name,Suffix,Exist) when erlang:is_binary(Name)->
	atom_suffix(erlang:binary_to_list(Name),Suffix,Exist);
atom_suffix(Name,Suffix,Exist)->
    StrName  = Name ++ Suffix,
    atom_suffix(StrName,Exist).

atom_suffix(Name,false)-> erlang:list_to_atom(Name);
atom_suffix(Name,true)->erlang:list_to_existing_atom(Name).


join([], _) -> <<>>;
join([H|T],Sep)->
	SepBin = to_string(Sep),
	Fun = fun(B)-> Str = to_string(B),<<SepBin/binary,Str/binary>> end,
	L = [ Fun(B) || B <- T ],
    lists:foldl(fun(I,Acc)-> <<Acc/binary,I/binary>> end,to_string(H),L).
hex_format(Size,Case)-> 
	Base = "~X.16.0",
	F = 
		case Case of
			upper -> Base ++ "B";
			lower -> Base ++ "b"
		end,
	lists:flatten(io_lib:format(F,[Size,"~"])).

		
