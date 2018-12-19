-module(ai_string).

-export([to_string/1,to_string/2]).
-export([to_integer/1,to_boolean/1]).
-export([hash_to_string/3,md5_string/2,sha_string/2,sha256_string/2,sha512_string/2]).
-export([prefix/2,find/3,slice/2,slice/3,join/2]).
-export([atom_suffix/3]).
-export([dynamic_module/2]).
-export([html_escape/1,html_unescape/1]).

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
to_string(Val) when erlang:is_float(Val) -> erlang:list_to_binary(io_lib:format("~p", [Val]));
to_string(Val) when erlang:is_boolean(Val) -> erlang:atom_to_binary(Val,latin1);
to_string(Val) when erlang:is_atom(Val) -> erlang:atom_to_binary(Val,latin1);
to_string(Val) when erlang:is_list(Val)  ->unicode:characters_to_binary(Val);
to_string(Val) when erlang:is_binary(Val)-> Val.

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


-spec prefix(String::unicode:chardata(), Prefix::unicode:chardata()) ->
                    'nomatch' | unicode:chardata().
-ifdef(OTP_RELEASE).
prefix(Str, Prefix0) -> string:prefix(Str,Prefix0).
-else.
prefix(Str, Prefix0) -> ai_string_compat:prefix(Str,Prefix0).
-endif.

-spec find(String, SearchPattern, Dir) -> unicode:chardata() | 'nomatch' when
      String::unicode:chardata(),
      SearchPattern::unicode:chardata(),
      Dir::atom().
-ifdef(OTP_RELEASE).
find(String,SearchPattern,Dir)-> string:find(String,SearchPattern,Dir).
-else.
find(String,SearchPattern,Dir)-> ai_string_compat:find(String,SearchPattern,Dir).
-endif.

-spec slice(String, Start) -> Slice when
      String::unicode:chardata(),
      Start :: non_neg_integer(),
      Slice :: unicode:chardata().
-ifdef(OTP_RELEASE).
slice(String,Start)-> string:slice(String,Start).
-else.
slice(String,Start) -> ai_string_compat:slice(String,Start).
-endif.

-spec slice(String, Start, Length) -> Slice when
      String::unicode:chardata(),
      Start :: non_neg_integer(),
      Length :: 'infinity' | non_neg_integer(),
      Slice :: unicode:chardata().
-ifdef(OTP_RELEASE).
slice(String,Start,Length)-> string:slice(String,Start,Length).
-else.
slice(String,Start,Length) -> ai_string_compat:slice(String,Start,Length).
-endif.

join([], _) -> <<>>;
join(List,Sep)->
	SepBin = to_string(Sep),
	[Hd | Tl] = [ [SepBin, to_string(B)] || B <- List ],
    erlang:iolist_to_binary([erlang:tl(Hd) | Tl]).

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

		
