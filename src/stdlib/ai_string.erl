-module(ai_string).

-export([to_string/1,to_string/2]).
-export([to_integer/1,to_boolean/1]).
-export([md5_string/2,sha_string/2,sha256_string/2,sha512_string/2]).
-export([string_to_hex/2,hash_to_string/3]).
-export([prefix/2,find/3,slice/2,slice/3,join/2]).
-export([atom_suffix/3]).
-export([dynamic_module/2]).
-export([html_escape/1,html_unescape/1]).
-export([to_iolist/1]).
-export([to_upper/1,to_lower/1]).

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
join([H|T],Sep)->
	SepBin = to_string(Sep),
	Fun = fun(B)-> Str = to_string(B),<<SepBin/binary,Str/binary>> end,
	L = [ Fun(B) || B <- T ],
    lists:foldl(fun(I,Acc)-> <<Acc/binary,I/binary>> end,to_string(H),L).

	-spec to_lower(binary()|atom()|list()) -> binary().




to_lower(L) when is_binary(L) ->
	OTPRelease = erlang:list_to_integer(erlang:system_info(otp_release)),
	if 
		OTPRelease >= 20 -> string:lowercase(L);
		true -> << << (char_to_lower(C)) >> || << C >> <= L >>
	end;
to_lower(L) ->
  to_lower(to_string(L)).

-spec to_upper(binary()|atom()|list()) -> binary().
to_upper(U) when is_binary(U)->
	OTPRelease = erlang:list_to_integer(erlang:system_info(otp_release)),
	if 
		OTPRelease >= 20 -> string:uppercase(U);
		true ->   << << (char_to_upper(C)) >> || << C >> <= U >>
	end;
to_upper(L) ->
  to_upper(to_string(L)).

%%%
%%% private
%%%

%% @doc Convert [A-Z] characters to lowercase.
%% @end
%% We gain noticeable speed by matching each value directly.
-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

%% @doc Convert [a-z] characters to uppercase.
-spec char_to_upper(char()) -> char().
char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(Ch) -> Ch.

hex_format(Size,Case)-> 
	Base = "~X.16.0",
	F = 
		case Case of
			upper -> Base ++ "B";
			lower -> Base ++ "b"
		end,
	lists:flatten(io_lib:format(F,[Size,"~"])).

		
