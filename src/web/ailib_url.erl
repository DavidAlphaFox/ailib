-module(ailib_url).

-export([new/0,
         new/1,
         urlencode/1,
         urldecode/1,
         parse/1,
         build/1,
         parse_query/1,
         build_query/1,
         scheme/1,
         scheme/2,
         authority/1,
         authority/2,
         host/1,
         host/2,
         port/1,
         port/2]).

-record(url,{scheme = undefined :: binary(),
             authority = undefined ::binary(),
             userinfo = undefined ::binary(),
             host = undefined ::binary(),
             port = undefined ::binary(),
             path = undefined ::binary(),
             qs = undefined ::  list(),
             fragment = undefined :: list()}).

-type url() :: #url{}.
-record(ailib_url_state,{array_fun = undefined :: fun(),
                         url = #url{} :: url()}).

-type state() :: #ailib_url_state{}.

-export_type([state/0]).

-define(BRACKET,<<"[]">>).

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

-spec new(fun()) -> state().
new(ArrayFun)-> #ailib_url_state{array_fun = ArrayFun}.

-spec scheme(state()) -> binary().
scheme(#ailib_url_state{url = URL}) -> URL#url.scheme.

-spec scheme(binary(),state()) -> state().
scheme(Scheme,#ailib_url_state{url = URL} = S) ->
  S#ailib_url_state{url = URL#url{scheme = Scheme}}.

-spec authority(state()) -> binary().
authority(#ailib_url_state{url = URL}) -> URL#url.authority.

-spec authority(binary(),state()) -> state().
authority(Authority,#ailib_url_state{url = URL} = S) ->
  case binary:match(Authority, [<<":">>]) of
    nomatch ->
      S#ailib_url_state{
        url = URL#url{scheme = Authority,port = udefined, host = Authority}};
    {S,L} ->
      Pos = S + L,
      Port = ailib_string:to_integer(
               binary:part(Authority,Pos,byte_size(Authority) - Pos)),
      Host = binary:part(Authority,0,S),
      S#ailib_url_state{
        url = URL#url{authority = Authority,host = Host,port = Port}}
  end.

-spec host(state())->binary().
host(#ailib_url_state{url = URL}) -> URL#url.host.

-spec host(binary(),state())-> state().
host(Host,#ailib_url_state{url = URL} = S)->
  Authority = build_authority(Host,URL#url.port),
  S#ailib_url_state{url = URL#url{authority = Authority,host = Host}}.

-spec port(state())->binary().
port(#ailib_url_state{url = URL}) -> URL#url.port.

-spec port(binary(),state())-> state().
port(Port,#ailib_url_state{url = URL} = S)->
  Authority = build_authority(URL#url.host,Port),
  S#ailib_url_state{url = URL#url{authority = Authority,port = Port}}.

build_authority(undefined,undefined) -> undefined;
build_authority(Host,undefined) -> Host;
build_authority(undefined,Port)->
  BPort = ailib_string:to_binary(Port),
  <<$:,BPort/binary>>;
build_authority(Host,Port) ->
  BPort = ailib_string:to_binary(Port),
  <<Host/binary,$:,BPort/binary>>.

-spec parse(binary()|list()) -> state().
parse(U)-> parse(U,#ailib_url_state{}).

-spec parse(binary()|list(),state()) -> state().
parse(U,S)->
  UBinary = ailib_string:to_binary(U),
  URL = parse(scheme,UBinary,S#ailib_url_state.url),
  S#ailib_url_state{url = URL}.

-spec parse(atom(),binary(),state()) -> state().
parse(scheme,Bin,Acc)->
  case binary:match(Bin, [<<":">>]) of
    nomatch -> parse(authority,Bin,Acc);
    {S,L}->
      %% maybe this example.com:3333/over/there?name=ferret#nose
      MaybeScheme = binary:part(Bin, 0, S),
      Pos = S + L, %%binary的切开点，Pos是未匹配字串的第一个字符
      DoubleSlash = binary:at(Bin,Pos) =:= $/ 
        andalso binary:at(Bin,Pos + 1) =:= $/,
      if
        DoubleSlash == true ->
          Rest  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
          parse(authority,Rest,Acc#url{scheme = MaybeScheme});
        true -> parse(authority,Bin,Acc)
      end
    end;

parse(authority,<<"//",Bin/bits>>,Acc)-> parse_authority(Bin,<<>>,Acc);
parse(authority,Bin,Acc)-> parse(path,Bin,Acc);


parse(path,Bin,Acc)->
  case binary:match(Bin,[<<"?">>]) of
    nomatch ->
      %% example.com/a/b/c
      %% example.com/a/b/c#c=x&d=n
      case binary:match(Bin,[<<"#">>]) of
        nomatch -> %% example.com/a/b/c
          Acc#url{path = Bin};
        {S,L}->
          Path = binary:part(Bin,0,S),
          Pos = S + L,
          Rest  = binary:part(Bin, Pos, byte_size(Bin) - Pos),
          parse(fragment,Rest,Acc#url{path = Path})
      end;
    {S1,L1}->
      %% example.com/a/b/c?1=&2=
      %% example.com/a/b/c?1=&2=#c=x&d=n
      Path = binary:part(Bin,0,S1),
      Pos = S1 + L1,
      Rest = binary:part(Bin,Pos,byte_size(Bin) - Pos),
      parse(qs,Rest,Acc#url{path = Path})
  end;
parse(qs,Bin,Acc)->
  case binary:match(Bin,[<<"#">>]) of
    nomatch ->
      QS = parse_query(Bin),
      Acc#url{qs = QS};
    {S,L} ->
      Query = binary:part(Bin,0,S),
      QS = parse_query(Query),
      Pos = S + L,
      Rest = binary:part(Bin,Pos,byte_size(Bin) - Pos),
      parse(fragment,Rest,Acc#url{qs = QS})
  end;
parse(fragment,Bin,Acc)->
  QS = parse_query(Bin),
  Acc#url{fragment = QS}.

parse_authority(Bin,Acc,URL)->
  parse_authority(false,Bin,Acc,URL).
parse_authority(true,<<>>,Acc,URL)->
  URL0 = split_userinfo(Acc,URL),
  URL0#url{path = <<"/">>};
parse_authority(true,<<$/,_/binary>> = Bin,Acc,URL) ->
  URL0 = split_userinfo(Acc,URL),
  parse(path,Bin,URL0);
parse_authority(true,<<$?,Rest/binary>>,Acc,URL)->
  URL0 = split_userinfo(Acc,URL),
  parse(qs,Rest,URL0#url{path = <<"/">>});
parse_authority(true,<<$#,Rest/binary>>,Acc,URL) ->
  URL0 = split_userinfo(Acc,URL),
  parse(fragment,Rest,URL0#url{path = <<"/">>});
parse_authority(_,<<$.,Rest/binary>>,Acc,URL) ->
  parse_authority(true,Rest,<<Acc/binary,$.>>,URL);
parse_authority(SeenDot,<<C,Rest/binary>>,Acc,URL) ->
  parse_authority(SeenDot,Rest,<<Acc/binary,C>>,URL).

split_userinfo(Bin,URL)->
  case binary:match(Bin,[<<"@">>]) of
    nomatch -> split_host_port(Bin,URL);
    {S,L} ->
      Pos = S + L,
      UserInfo = binary:part(Bin,0,S),
      Rest =   binary:part(Bin,Pos,byte_size(Bin) - Pos),
      split_host_port(Rest,URL#url{userinfo = UserInfo})
  end.

split_host_port(Bin,URL)->
  case binary:match(Bin,[<<":">>]) of
    nomatch -> URL#url{authority = Bin,host = Bin};
    {S1,L1} ->
      Pos1 = S1 + L1,
      Port = ailib_string:to_integer(
               binary:part(Bin,Pos1,byte_size(Bin) - Pos1)),
      Host = binary:part(Bin,0,S1),
      URL#url{authority = Bin,host = Host,port = Port}
  end.

-spec build(state())->binary().
build(Record)-> build(scheme,Record,<<>>).
build(scheme,#ailib_url_state{url = URL} = Record,Acc)->
  case URL#url.scheme of
    undefined -> build(authority,Record,Acc);
    Scheme ->
      SchemeBin = ailib_string:to_binary(Scheme),
      build(authority,Record,<<Acc/binary,SchemeBin/binary,"://">>)
  end;
build(authority,#ailib_url_state{url = URL} = Record,Acc)->
  case { URL#url.authority,URL#url.host} of
    {undefined,undefined} ->  build(path,Record,Acc);
    {undefined,Host}->
      case URL#url.port of
        undefined ->
          HostBin = ai_string:to_binary(Host),
          build(path,Record,<<Acc/binary,HostBin/binary>>);
        Port ->
          HostBin = ai_string:to_binary(Host),
          PortBin = ai_string:to_binary(Port),
          build(path,Record,<<Acc/binary,HostBin/binary,":",PortBin/binary>>)
      end;
    {Authority,_Host}->
      AuthorityBin = ai_string:to_binaryg(Authority),
      build(path,Record,<<Acc/binary,AuthorityBin/binary>>)
  end;
build(path,#ailib_url_state{url = URL} = Record,Acc)->
  case URL#url.path of
    undefined -> build(qs,Record,<<Acc/binary,"/">>);
    Path ->
      PathBin = ai_string:to_binary(Path),
      build(qs,Record,<<Acc/binary,PathBin/binary>>)
  end;
build(qs,#ailib_url_state{url = URL,array_fun = ArrayFun} = Record,Acc)->
  case URL#url.qs of
    undefined -> build(fragment,Record,Acc);
    QS ->
      Q = build_query(ArrayFun,QS),
      build(fragment,Record,<<Acc/binary,$?,Q/binary>>)
  end;
build(fragment,#ailib_url_state{url = URL,array_fun = ArrayFun},Acc)->
  case URL#url.fragment of
    undefined -> Acc;
    QS ->
      Q = build_query(ArrayFun,QS),
      <<Acc/binary,$?,Q/binary>>
  end.

-spec build_query(list()) -> binary().
build_query([])-> <<>>;
build_query(L)-> build_query(undefined,L, <<>>).

-spec build_query(fun(),list()) -> binary().
build_query(_,[])-> <<>>;
build_query(ArrayFun,L)-> build_query(ArrayFun,L, <<>>).

build_query(_,[], Acc) -> Acc;
build_query(ArrayFun,[{Name, true}|Tail], Acc) ->
	Acc2 = urlencode(Name, << Acc/bits, $& >>),
	build_query(ArrayFun,Tail, Acc2);
build_query(ArrayFun,[{Name, Value}|Tail], Acc) ->
  QS =
    if erlang:is_list(Value) -> build_array_param(ArrayFun,Name,Value);
       true ->
        Name0 = urlencode(Name),
        Value0 = urlencode(Value),
        <<Name0/binary,$=,Value0/binary>>
    end,
	build_query(Tail, <<Acc/binary, $&,QS/binary>>).

build_array_param(undefined,Name,Value)->
  KV = lists:maps(
        fun(Item)->
            BName = ailib_string:to_binary(Name),
            BItem = ailib_string:to_binary(Item),
            Name0 = urlencode(<<BName/binary,?BRACKET/binary>>),
            Item0 = urlencode(BItem),
            <<Name0/binary,$=,Item0/binary>>
        end,Value),
  ailib_string:join(KV,<<"&">>);
build_array_param(ArrayFun,Name,Value)->
  erlang:apply(ArrayFun,[Name,Value]).

parse_query(B) -> parse_query_name(B, [], <<>>).

parse_query_name(<< $=, Rest/bits >>, Acc, Name) when Name =/= <<>> ->
  parse_query_value(Rest, Acc, urldecode(Name), <<>>);
parse_query_name(<< $&, Rest/bits >>, Acc, Name) ->
	case Name of
		<<>> -> parse_query_name(Rest, Acc, <<>>);
		_ -> parse_query_name(Rest, [{urldecode(Name), true}|Acc], <<>>)
	end;
parse_query_name(<< C, Rest/bits >>, Acc, Name) when C =/= $%, C =/= $= ->
    parse_query_name(Rest, Acc, << Name/bits, C >>);
parse_query_name(<<>>, Acc, Name) ->
	case Name of
		<<>> -> lists:reverse(Acc);
		_ -> lists:reverse([{urldecode(Name), true}|Acc])
	end.

parse_query_value(<< $&, Rest/bits >>, Acc, Name, Value) ->
	parse_query_name(Rest,
                   add_query_param(Name, urldecode(Value),Acc),
                   <<>>);
parse_query_value(<< C, Rest/bits >>, Acc, Name, Value) when C =/= $% ->
	parse_query_value(Rest, Acc, Name, << Value/bits, C >>);
parse_query_value(<<>>, Acc, Name, Value) ->
	lists:reverse(add_query_param(Name, urldecode(Value),Acc)).

add_query_param(Name,Value,[{Name0,VS}|Acc0] = Acc)
  when erlang:byte_size(Name) > 2 ->
  NameSize = erlang:byte_size(Name) - 2,
  <<Name1:NameSize/binary,Rest/binary>> = Name,
  case Rest of
    ?BRACKET ->
      if Name1 == Name0 -> [{Name0,VS ++ [Value]}| Acc0];
         true -> [{Name1, [Value]}| Acc]
      end;
    _ ->
      if Name == Name0 ->
          if erlang:is_list(VS) -> [{Name,VS ++ [Value]}| Acc0];
             true -> [{Name,[VS,Value]}| Acc0]
          end;
         true -> [{Name,Value}|Acc]
      end
  end;

add_query_param(Name,Value,[])
  when erlang:byte_size(Name) > 2 ->
  NameSize = erlang:byte_size(Name) - 2,
  <<Name1:NameSize/binary,Rest/binary>> = Name,
  case Rest of
    ?BRACKET -> [{Name1,[Value]}];
    _ -> [{Name,Value}]
  end;

add_query_param(Name,Value,[{Name0,VS}|Acc0] = Acc)->
  if Name == Name0 ->
      if erlang:is_list(VS) -> [{Name,VS ++ [Value]}| Acc0];
         true -> [{Name,[VS,Value]}| Acc0]
      end;
     true -> [{Name,Value}|Acc]
  end;
add_query_param(Name,Value,[]) -> [{Name,Value}].


%% @doc Percent encode a string. (RFC3986 2.1)
-spec urlencode(B) -> B when B::binary().
urlencode(B) -> urlencode(B, <<>>).
urlencode(<< $!, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $! >>);
urlencode(<< $$, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $$ >>);
urlencode(<< $&, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $& >>);
urlencode(<< $', Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $' >>);
urlencode(<< $(, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $( >>);
urlencode(<< $), Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $) >>);
urlencode(<< $*, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $* >>);
urlencode(<< $+, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $,, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $, >>);
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
urlencode(<< $:, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $: >>);
urlencode(<< $;, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $; >>);
urlencode(<< $=, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $= >>);
urlencode(<< $@, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $@ >>);
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
urlencode(<< $~, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $~ >>);
urlencode(<< C, Rest/bits >>, Acc) ->
	H = hex(C bsr 4),
	L = hex(C band 16#0f),
	urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) ->
	Acc.

%% @doc Decode a percent encoded string. (RFC3986 2.1)
-spec urldecode(B) -> B when B::binary().
urldecode(B) -> urldecode(B, <<>>).
urldecode(<< $%, H, L, Rest/bits >>, Acc) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<< $!, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $! >>);
urldecode(<< $$, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $$ >>);
urldecode(<< $&, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $& >>);
urldecode(<< $', Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $' >>);
urldecode(<< $(, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $( >>);
urldecode(<< $), Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $) >>);
urldecode(<< $*, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $* >>);
urldecode(<< $+, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $+ >>);
urldecode(<< $,, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $, >>);
urldecode(<< $-, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $- >>);
urldecode(<< $., Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $. >>);
urldecode(<< $0, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $0 >>);
urldecode(<< $1, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $1 >>);
urldecode(<< $2, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $2 >>);
urldecode(<< $3, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $3 >>);
urldecode(<< $4, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $4 >>);
urldecode(<< $5, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $5 >>);
urldecode(<< $6, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $6 >>);
urldecode(<< $7, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $7 >>);
urldecode(<< $8, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $8 >>);
urldecode(<< $9, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $9 >>);
urldecode(<< $:, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $: >>);
urldecode(<< $;, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $; >>);
urldecode(<< $=, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $= >>);
urldecode(<< $@, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $@ >>);
urldecode(<< $A, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $A >>);
urldecode(<< $B, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $B >>);
urldecode(<< $C, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $C >>);
urldecode(<< $D, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $D >>);
urldecode(<< $E, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $E >>);
urldecode(<< $F, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $F >>);
urldecode(<< $G, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $G >>);
urldecode(<< $H, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $H >>);
urldecode(<< $I, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $I >>);
urldecode(<< $J, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $J >>);
urldecode(<< $K, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $K >>);
urldecode(<< $L, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $L >>);
urldecode(<< $M, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $M >>);
urldecode(<< $N, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $N >>);
urldecode(<< $O, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $O >>);
urldecode(<< $P, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $P >>);
urldecode(<< $Q, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $Q >>);
urldecode(<< $R, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $R >>);
urldecode(<< $S, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $S >>);
urldecode(<< $T, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $T >>);
urldecode(<< $U, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $U >>);
urldecode(<< $V, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $V >>);
urldecode(<< $W, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $W >>);
urldecode(<< $X, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $X >>);
urldecode(<< $Y, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $Y >>);
urldecode(<< $Z, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $Z >>);
urldecode(<< $_, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $_ >>);
urldecode(<< $a, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $a >>);
urldecode(<< $b, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $b >>);
urldecode(<< $c, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $c >>);
urldecode(<< $d, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $d >>);
urldecode(<< $e, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $e >>);
urldecode(<< $f, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $f >>);
urldecode(<< $g, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $g >>);
urldecode(<< $h, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $h >>);
urldecode(<< $i, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $i >>);
urldecode(<< $j, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $j >>);
urldecode(<< $k, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $k >>);
urldecode(<< $l, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $l >>);
urldecode(<< $m, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $m >>);
urldecode(<< $n, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $n >>);
urldecode(<< $o, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $o >>);
urldecode(<< $p, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $p >>);
urldecode(<< $q, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $q >>);
urldecode(<< $r, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $r >>);
urldecode(<< $s, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $s >>);
urldecode(<< $t, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $t >>);
urldecode(<< $u, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $u >>);
urldecode(<< $v, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $v >>);
urldecode(<< $w, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $w >>);
urldecode(<< $x, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $x >>);
urldecode(<< $y, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $y >>);
urldecode(<< $z, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $z >>);
urldecode(<< $~, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $~ >>);
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
