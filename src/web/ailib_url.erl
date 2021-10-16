-module(ailib_url).

-export([parse/1,build/1]).
-export([parse_query/1,build_query/1]).
-export([urlencode/1,urldecode/1]).

-record(ailib_url_state,{schema = undefined,
                   authority = undefined,
                   host = undefined,
                   port = undefined,
                   path = undefined,
                   qs = undefined,
                   fragment = undefined}).

-type state() :: #ailib_url_state{}.

-export_type([state/0]).


%% This is from chapter 3, Syntax Components, of RFC 3986:
%%
%% The generic URI syntax consists of a hierarchical sequence of
%% components referred to as the scheme, authority, path, query, and
%% fragment.
%%
%%    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%%
%%    hier-part   = "//" authority path-abempty
%%                   / path-absolute
%%                   / path-rootless
%%                   / path-empty
%%
%%    The scheme and path components are required, though the path may be
%%    empty (no characters).  When authority is present, the path must
%%    either be empty or begin with a slash ("/") character.  When
%%    authority is not present, the path cannot begin with two slash
%%    characters ("//").  These restrictions result in five different ABNF
%%    rules for a path (Section 3.3), only one of which will match any
%%    given URI reference.
%%
%%    The following are two example URIs and their component parts:
%%
%%          foo://example.com:8042/over/there?name=ferret#nose
%%          \_/   \______________/\_________/ \_________/ \__/
%%           |           |            |            |        |X2
%%        scheme     authority       path        query   fragment
%%           |   _____________________|__
%%          / \ /                        \
%%          urn:example:animal:ferret:nose
%%
%%    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
%%    authority   = [ userinfo "@" ] host [ ":" port ]
%%    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

-spec new() -> state().
new()-> #ailib_url_state{}.

parse(U)->
    UBinary = ai_string:to_string(U),
    parse(schema,UBinary,#ailib_url_state{}).
parse(schema,Bin,Acc)->
    case binary:match(Bin, [<<":">>]) of
        nomatch ->
            parse(authority,Bin,Acc);
        {S,L}->
            %% maybe this example.com:3333/over/there?name=ferret#nose
            MaybeSchema = binary:part(Bin, 0, S),
            Pos = S + L, %%binary的切开点，Pos是未匹配字串的第一个字符
            DoubleSlash = binary:at(Bin,Pos) =:= $/ andalso binary:at(Bin,Pos + 1) =:= $/,
            if
                DoubleSlash == true ->
                    Rest  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
                    parse(authority,Rest,Acc#ailib_url_state{schema = MaybeSchema});
                true ->
                    parse(authority,Bin,Acc)
            end
    end;
parse(authority,<<"//",Bin/bits>>,Acc)->
    case binary:match(Bin, [<<":">>]) of
        nomatch ->
            %% example.com/a
            %% example.com/
            %% example.com
            case binary:match(Bin,[<<"/">>]) of
                nomatch -> Acc#ailib_url_state{authority = Bin,host = Bin,path = <<"/">>}; %example.com
                {S1,_L1}->
                    Authority = binary:part(Bin,0,S1),
                    Rest = binary:part(Bin,S1,byte_size(Bin) - S1),
                    parse(path,Rest,Acc#ailib_url_state{authority = Authority,host = Authority})
            end;
        {S,L}->
            case binary:match(Bin,[<<"/">>]) of
                nomatch ->
                    Pos = S + L,
                    Port = binary:part(Bin,Pos,byte_size(Bin) - Pos),
                    Host = binary:part(Bin,0,S),
                    Acc#ailib_url_state{authority = Bin,host = Host,port = Port,path = <<"/">>}; %example.com
                {S2,_L2}->
                    Pos = S + L,
                    Port = binary:part(Bin,Pos,S2 - Pos),
                    Host = binary:part(Bin,0,S),
                    Rest = binary:part(Bin,S2,byte_size(Bin) - S2),
                    parse(path,Rest,Acc#ailib_url_state{authority = <<Host/binary,":",Port/binary>>,host = Host,port = Port})

            end
    end;
%% no schema here, but can have authority
parse(authority,Bin,Acc)->
   case binary:match(Bin,[<<".">>]) of
        nomatch -> parse(path,Bin,Acc);
        _ -> parse(authority,<<"//",Bin/binary>>,Acc)
    end;

parse(path,Bin,Acc)->
    case binary:match(Bin,[<<"?">>]) of
        nomatch ->
            %% example.com/a/b/c
            %% example.com/a/b/c#c=x&d=n
            case binary:match(Bin,[<<"#">>]) of
                nomatch -> %% example.com/a/b/c
                    Acc#ailib_url_state{path = Bin};
                {S,L}->
                    Path = binary:part(Bin,0,S),
                    Pos = S + L,
                    Rest  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
                    parse(fragment,Rest,Acc#ailib_url_state{path = Path})
            end;
        {S1,L1}->
            %% example.com/a/b/c?1=&2=
            %% example.com/a/b/c?1=&2=#c=x&d=n
            Path = binary:part(Bin,0,S1),
            Pos = S1 + L1,
            Rest = binary:part(Bin,Pos,byte_size(Bin) - Pos),
            parse(qs,Rest,Acc#ailib_url_state{path = Path})
    end;
parse(qs,Bin,Acc)->
    case binary:match(Bin,[<<"#">>]) of
        nomatch ->
            QS = parse_query(Bin),
            QS0 = lists:map(fun({K,V})->
                                    {urldecode(K),urldecode(V)}
                            end,QS),
            Acc#ailib_url_state{qs = QS0};
        {S,L} ->
            Query = binary:part(Bin,0,S),
            QS = parse_query(Query),
            QS0 = lists:map(fun({K,V})->
                                    {urldecode(K),urldecode(V)}
                            end,QS),
            Pos = S + L,
            Rest = binary:part(Bin,Pos,byte_size(Bin) - Pos),
            parse(fragment,Rest,Acc#ailib_url_state{qs = QS0})
end;
parse(fragment,Bin,Acc)->
    QS = parse_query(Bin),
    QS0 = lists:map(fun({K,V})->
            {urldecode(K),urldecode(V)}
        end,QS),
    Acc#ailib_url_state{fragment = QS0}.


build(Record)->
    build(schema,Record,<<>>).
build(schema,Record,Acc)->
    case Record#ailib_url_state.schema of
        undefined ->
            build(authority,Record,Acc);
        Schema ->
            SchemaBin = ai_string:to_string(Schema),
            build(authority,Record,<<Acc/binary,SchemaBin/binary,"://">>)
    end;
build(authority,Record,Acc)->
    case {Record#ailib_url_state.authority,Record#ailib_url_state.host} of
        {undefined,undefined} ->  build(path,Record,Acc);
        {undefined,Host}->
            case Record#ailib_url_state.port of
                undefined ->
                    HostBin = ai_string:to_string(Host),
                    build(path,Record,<<Acc/binary,HostBin/binary>>);
                Port ->
                    HostBin = ai_string:to_string(Host),
                    PortBin = ai_string:to_string(Port),
                    build(path,Record,<<Acc/binary,HostBin/binary,":",PortBin/binary>>)
            end;
        {Authority,_Host}->
            AuthorityBin = ai_string:to_string(Authority),
            build(path,Record,<<Acc/binary,AuthorityBin/binary>>)
    end;
build(path,Record,Acc)->
    case Record#ailib_url_state.path of
        undefined -> build(qs,Record,<<Acc/binary,"/">>);
        Path ->
            PathBin = ai_string:to_string(Path),
            build(qs,Record,<<Acc/binary,PathBin/binary>>)
    end;
build(qs,Record,Acc)->
    case Record#ailib_url_state.qs of
        undefined -> build(fragment,Record,Acc);
        QS ->
            Q = lists:map(fun({Key,Value})->
                                  EKey = urlencode(ai_string:to_string(Key)),
                                  EValue =urlencode(ai_string:to_string(Value)),
                                  <<EKey/binary,"=",EValue/binary>>
                          end,QS),
            S = lists:foldl(
                    fun
                        (I,QAcc) when erlang:is_atom(QAcc)-> <<"?",I/binary>> ;
                        (I,QAcc) ->  <<QAcc/binary,"&",I/binary>>
                    end, undefined,Q),
            if
                S == undefined ->
                    build(fragment,Record,Acc);
                true ->
                    build(fragment,Record,<<Acc/binary,S/binary>>)
            end
    end;
build(fragment,Record,Acc)->
    case Record#ailib_url_state.fragment of
        undefined -> Acc;
        QS ->
            Q = lists:map(fun({Key,Value})->
                                  EKey = urlencode(ai_string:to_string(Key)),
                                  EValue = urlencode(ai_string:to_string(Value)),
                                  <<EKey/binary,"=",EValue/binary>>
                          end,QS),
            S = lists:foldl(
                    fun
                        (I,QAcc) when erlang:is_atom(QAcc)-> <<"#",I/binary>> ;
                        (I,QAcc) ->  <<QAcc/binary,"&",I/binary>>
                    end, undefined,Q),
            <<Acc/binary,S/binary>>
    end.

urlencode(B) -> urlencode(B, <<>>).
urlencode(<< $\s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $-, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $- >>);
urlencode(<< $., Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $. >>);
urlencode(<< $0, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $0 >>);
urlencode(<< $1, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $1 >>);
urlencode(<< $2, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $2 >>);
urlencode(<< $3, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $3 >>);
urlencode(<< $4, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $4 >>);
urlencode(<< $5, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $5 >>);
urlencode(<< $6, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $6 >>);
urlencode(<< $7, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $7 >>);
urlencode(<< $8, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $8 >>);
urlencode(<< $9, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $9 >>);
urlencode(<< $A, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $A >>);
urlencode(<< $B, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $B >>);
urlencode(<< $C, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $C >>);
urlencode(<< $D, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $D >>);
urlencode(<< $E, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $E >>);
urlencode(<< $F, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $F >>);
urlencode(<< $G, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $G >>);
urlencode(<< $H, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $H >>);
urlencode(<< $I, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $I >>);
urlencode(<< $J, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $J >>);
urlencode(<< $K, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $K >>);
urlencode(<< $L, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $L >>);
urlencode(<< $M, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $M >>);
urlencode(<< $N, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $N >>);
urlencode(<< $O, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $O >>);
urlencode(<< $P, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $P >>);
urlencode(<< $Q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Q >>);
urlencode(<< $R, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $R >>);
urlencode(<< $S, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $S >>);
urlencode(<< $T, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $T >>);
urlencode(<< $U, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $U >>);
urlencode(<< $V, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $V >>);
urlencode(<< $W, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $W >>);
urlencode(<< $X, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $X >>);
urlencode(<< $Y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Y >>);
urlencode(<< $Z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Z >>);
urlencode(<< $_, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $_ >>);
urlencode(<< $a, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $a >>);
urlencode(<< $b, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $b >>);
urlencode(<< $c, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $c >>);
urlencode(<< $d, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $d >>);
urlencode(<< $e, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $e >>);
urlencode(<< $f, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $f >>);
urlencode(<< $g, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $g >>);
urlencode(<< $h, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $h >>);
urlencode(<< $i, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $i >>);
urlencode(<< $j, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $j >>);
urlencode(<< $k, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $k >>);
urlencode(<< $l, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $l >>);
urlencode(<< $m, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $m >>);
urlencode(<< $n, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $n >>);
urlencode(<< $o, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $o >>);
urlencode(<< $p, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $p >>);
urlencode(<< $q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $q >>);
urlencode(<< $r, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $r >>);
urlencode(<< $s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $s >>);
urlencode(<< $t, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $t >>);
urlencode(<< $u, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $u >>);
urlencode(<< $v, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $v >>);
urlencode(<< $w, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $w >>);
urlencode(<< $x, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $x >>);
urlencode(<< $y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $y >>);
urlencode(<< $z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $z >>);
urlencode(<< C, Rest/bits >>, Acc) ->
	H = hex(C bsr 4),
	L = hex(C band 16#0f),
	urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) -> Acc.

urldecode(B) -> urldecode(B, <<>>).

urldecode(<< $%, H, L, Rest/bits >>, Acc) ->
    C = (unhex(H) bsl 4 bor unhex(L)),
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<< $+, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, " " >>);
urldecode(<< C, Rest/bits >>, Acc) when C =/= $% -> urldecode(Rest, << Acc/bits, C >>);
urldecode(<<>>, Acc) -> Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

build_query([])-> <<>>;
build_query(L)-> build_query(L, <<>>).

build_query([], Acc) ->
	<< $&, Query/bits >> = Acc,
	Query;
build_query([{Name, true}|Tail], Acc) ->
	Acc2 = urlencode(Name, << Acc/bits, $& >>),
	build_query(Tail, Acc2);
build_query([{Name, Value}|Tail], Acc) ->
	Acc2 = urlencode(Name, << Acc/bits, $& >>),
	Acc3 = urlencode(Value, << Acc2/bits, $= >>),
	build_query(Tail, Acc3).

parse_query(B) -> parse_query_name(B, [], <<>>).

parse_query_name(<< $%, H, L, Rest/bits >>, Acc, Name) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	parse_query_name(Rest, Acc, << Name/bits, C >>);
parse_query_name(<< $+, Rest/bits >>, Acc, Name) ->
    parse_query_name(Rest, Acc, << Name/bits, " " >>);
parse_query_name(<< $=, Rest/bits >>, Acc, Name) when Name =/= <<>> ->
    parse_query_value(Rest, Acc, Name, <<>>);
parse_query_name(<< $&, Rest/bits >>, Acc, Name) ->
	case Name of
		<<>> -> parse_query_name(Rest, Acc, <<>>);
		_ -> parse_query_name(Rest, [{Name, true}|Acc], <<>>)
	end;
parse_query_name(<< C, Rest/bits >>, Acc, Name) when C =/= $%, C =/= $= ->
    parse_query_name(Rest, Acc, << Name/bits, C >>);
parse_query_name(<<>>, Acc, Name) ->
	case Name of
		<<>> -> lists:reverse(Acc);
		_ -> lists:reverse([{Name, true}|Acc])
	end.

parse_query_value(<< $%, H, L, Rest/bits >>, Acc, Name, Value) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	parse_query_value(Rest, Acc, Name, << Value/bits, C >>);
parse_query_value(<< $+, Rest/bits >>, Acc, Name, Value) ->
	parse_query_value(Rest, Acc, Name, << Value/bits, " " >>);
parse_query_value(<< $&, Rest/bits >>, Acc, Name, Value) ->
	parse_query_name(Rest, [{Name, Value}|Acc], <<>>);
parse_query_value(<< C, Rest/bits >>, Acc, Name, Value) when C =/= $% ->
	parse_query_value(Rest, Acc, Name, << Value/bits, C >>);
parse_query_value(<<>>, Acc, Name, Value) ->
	lists:reverse([{Name, Value}|Acc]).
