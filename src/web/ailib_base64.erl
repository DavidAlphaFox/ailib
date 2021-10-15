-module(ailib_base64).
-export([decode/1,
         decode/2,
         encode/1,
         encode/2]).

-export_type([base64_codec_opts/0]).

-type base64_codec_opts() :: #{padding => true | false,
                               url => true | false}.

-spec decode(binary()) -> binary().
decode(Enc) -> decode(Enc, #{}).
-spec decode(binary(),base64_codec_opts()) -> binary().
decode(Enc0, #{url := true} = Opts) ->
	Enc1 = << << case C of
                 $- -> $+;
                 $_ -> $/;
                 _ -> C
               end >> || << C >> <= Enc0 >>,
	Enc = add_padding(Enc1,Opts),
	base64:decode(Enc);
decode(Enc0,Opts) ->
    Enc = add_padding(Enc0,Opts),
    base64:decode(Enc).

-spec encode(binary()) -> binary().
encode(Dec) -> encode(Dec, #{}).
-spec encode(binary(),base64_codec_opts()) -> binary().
encode(Dec, Opts) -> encode(base64:encode(Dec), Opts, <<>>).

encode(<<$+, R/bits>>, #{url := true } = Opts, Acc) -> encode(R, Opts, <<Acc/binary, $->>);
encode(<<$/, R/bits>>, #{url := true } = Opts, Acc) -> encode(R, Opts, <<Acc/binary, $_>>);
encode(<<$=, _/bits>>, #{padding := false}, Acc) -> Acc;
encode(<<C, R/bits>>, Opts, Acc) -> encode(R, Opts, <<Acc/binary, C>>);
encode(<<>>, _, Acc) -> Acc.

add_padding(Enc,#{padding := false}) ->
    case erlang:byte_size(Enc) rem 4 of
        0 -> Enc;
        2 -> << Enc/binary, "==" >>;
        3 -> << Enc/binary, "=" >>
    end;
add_padding(Enc,_) -> Enc.
