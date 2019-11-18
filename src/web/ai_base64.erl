-module(ai_base64).
-export([decode/1,decode/2,encode/1,encode/2]).

decode(Enc) -> decode(Enc, #{}).
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

encode(Dec) -> encode(Dec, #{}).
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
