-module(ailib_hashid).

%% hash混淆ID的方案
%% Fisher–Yates_shuffle 混淆算法

-export([new/0, new/1,
         encode/2, decode/2,
         encode_hex/2, decode_hex/2,
         salt/1, alphabet/1, min_hash_length/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([consistent_shuffle/2, hash/2, unhash/2]).
-endif.

% Type declaration
-record (ailib_hashid, {
    salt       = [] :: list(),
    min_length = 0  :: non_neg_integer(),
    alphabet   = [] :: list(),
    seperators = [] :: list(),
    guards     = [] :: list()
}).

-type ailib_hashid() :: #ailib_hashid{}.

-export_type([ailib_hashid/0]).

% Constants
-define(VERSION,              "1.0.3").

-define(MIN_ALPHABET_LEN,     16).
-define(SEP_DIV,              3.5).
-define(GUARD_DIV,            12).

-define(DEFAULT_ALPHABET,     "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").
-define(DEFAULT_SEPS,         "cfhistuCFHISTU").

%% @doc make a new hashids context (convenient function)
-spec new() -> ailib_hashid().
new() ->
  new([]).

%% @doc make a new hashids context
-spec new([] | [{salt | default_alphabet | min_hash_length, any()}]) -> ailib_hashid().
new(Opts) ->
    Salt            = get_value(salt, Opts, []),
    NotUniqAlphabet = get_value(default_alphabet, Opts, ?DEFAULT_ALPHABET),
    MinHashLength   = erlang:max(get_value(min_hash_length, Opts, 0), 0),

    % validate options
    Alphabet = ailib_lists:unique(NotUniqAlphabet),

    valid = validate_alphabet(Alphabet),
    ok    = validate_salt(Salt),

    {Seps, ShuffledAlphabet}           = setup_sep(Alphabet, Salt),
    {Guards, FinalSeps, FinalAlphabet} = setup_guard(Seps, ShuffledAlphabet),

    % New HashID Context
    #ailib_hashid{
        salt       = Salt,
        min_length = MinHashLength,
        alphabet   = FinalAlphabet,
        seperators = FinalSeps,
        guards     = Guards
    }.

%% @doc encode numbers
%% @spec encode(ailib_hashid(), integer() | [integer(), ...]) -> string()
-spec encode(ailib_hashid(), integer() | [integer(), ...]) -> string().
encode(_, N) when is_integer(N), N < 0  ->
    "";
encode(Context, N) when is_integer(N), N >= 0 ->
    encode(Context, [N]);
encode(Context, N) when is_list(N) ->
    case lists:any(fun(E) -> is_integer(E) == false orelse E < 0 end, N) of
        true -> "";
        _    -> internal_encode(Context, N)
    end.


%% @doc encode hex string
%% @spec encode_hex(ailib_hashid(), string()) -> string()
-spec encode_hex(ailib_hashid(), string()) -> string().
encode_hex(Context, Str) when is_list(Str) ->
    encode(Context, [list_to_integer([$1 | S], 16) || S <- parts(Str, 12)]).


%% @doc decode hash string
%% @spec decode(ailib_hashid(), string()) -> [integer(), ...]
-spec decode(ailib_hashid(), string()) -> [integer(), ...].
decode(_, []) ->
    "";
decode(Context, HashStr) when is_list(HashStr) ->
    internal_decode(Context, HashStr).


%% @doc decode hash string to decoded hex string
%% @spec decode_hex(ailib_hashid(), string()) -> string()
-spec decode_hex(ailib_hashid(), string()) -> string().
decode_hex(Context, HashStr) when is_list(HashStr) ->
    lists:concat([begin [_ | T] = integer_to_list(I, 16), T end || I <- decode(Context, HashStr)]).


%% @doc returns salt from context
%% @spec salt(ailib_hashid()) -> string()
-spec salt(ailib_hashid()) -> string().
salt(Context) when is_record(Context, ailib_hashid) ->
    Context#ailib_hashid.salt.


%% @doc returns adjusted custom alphabet from context
%% @spec alphabet(ailib_hashid()) -> string()
-spec alphabet(ailib_hashid()) -> string().
alphabet(Context) when is_record(Context, ailib_hashid) ->
    Context#ailib_hashid.alphabet.


%% @doc returns minimum hash length from context
%% @spec min_hash_length(ailib_hashid()) -> non_neg_integer()
-spec min_hash_length(ailib_hashid()) -> non_neg_integer().
min_hash_length(Context) when is_record(Context, ailib_hashid) ->
    Context#ailib_hashid.min_length.


%% ===================================================================
%% Private
%% ===================================================================
internal_encode(_, []) ->
    "";
internal_encode(#ailib_hashid { salt       = Salt,
                                   min_length = MinHashLength,
                                   alphabet   = Alphabet,
                                   seperators = Seps,
                                   guards     = Guards}, N) ->

    HashInt            = hash_numbers(N),
    {FinalAlphabet, R} = pre_encode(N, HashInt, Salt, Alphabet, Seps),
    ExtendedR          = try_extend_encoded(1, R, HashInt, MinHashLength, Guards),

    post_encode(ExtendedR, FinalAlphabet, MinHashLength).


try_extend_encoded(1, R, HashInt, MinHashLength, Guards) when length(R) < MinHashLength ->
    try_extend_encoded(2, [pick_char_from_guards(1, R, HashInt, Guards)] ++ R, HashInt, MinHashLength, Guards);
try_extend_encoded(2, R, HashInt, MinHashLength, Guards) when length(R) < MinHashLength ->
    try_extend_encoded(3, R ++ [pick_char_from_guards(3, R, HashInt, Guards)], HashInt, MinHashLength, Guards);
try_extend_encoded(_, R, _, _, _) ->
    R.

pick_char_from_guards(Index, R, HashInt, Guards) ->
    HT = (HashInt + lists:nth(Index, R)) rem length(Guards),
    lists:nth(HT + 1, Guards).


pre_encode(N, HashInt, Salt, Alphabet, Seps) ->
    Lottery    = lists:nth(HashInt rem length(Alphabet) + 1, Alphabet),
    PreBuf     = [Lottery] ++ Salt,
    SepsLength = length(Seps),

    {FinalAlphabet, R, _} = 
        lists:foldl(fun(E, {Alpha, R0, I}) ->
            Buf     = PreBuf ++ Alpha,
            Alpha1  = consistent_shuffle(Alpha, lists:sublist(Buf, 1, length(Alpha))),
            Last    = hash(E, Alpha1),
            R1      = R0 ++ Last,
            R2 = 
                if  
                    (I + 1) < length(N) ->
                        E1 = E rem (lists:nth(1, Last) + I),
                        R1 ++ [lists:nth((E1 rem SepsLength) + 1, Seps)];
                    true -> R1
                end,
            {Alpha1, R2, I + 1}

        end, {Alphabet, [Lottery], 0}, N),

    {FinalAlphabet, R}.

post_encode(R, Alphabet, MinHashLength) when length(R) < MinHashLength ->
    HalfLen           = length(Alphabet) div 2,
    ShuffledAlphabet  = consistent_shuffle(Alphabet, Alphabet),

    R2 = lists:sublist(ShuffledAlphabet, HalfLen + 1, length(ShuffledAlphabet) - HalfLen) ++
         R ++
         lists:sublist(ShuffledAlphabet, 1, HalfLen),

    Excess = length(R2) - MinHashLength,
    if  Excess > 0 -> lists:sublist(R2, Excess div 2 + 1, MinHashLength);
        true       -> R2
    end;
post_encode(R, _, _) ->
    R.


internal_decode(#ailib_hashid { salt       = Salt,
                                   alphabet   = Alphabet,
                                   seperators = Seps,
                                   guards     = Guards} = Context, HashStr) ->

    HashBreakdown = replace_chars_with_whitespace_in_list(Guards, HashStr),
    HashArray     = string:tokens(HashBreakdown, " "),

    Breakdown = lists:nth(breakdown_index(length(HashArray)), HashArray),
    Result    = decode_breakdown_hash(Breakdown, Salt, Seps, Alphabet),

    case encode(Context, Result) of
        HashStr -> Result;
        _       -> []
    end.


replace_chars_with_whitespace_in_list(Check, Replace) when is_list(Check), is_list(Replace) ->
    [replace_whitespace_if_member(V, Check) || V <- Replace].

replace_whitespace_if_member(E, Check) ->
    case lists:member(E, Check) of
        true -> $\s;
        _    -> E
    end.


breakdown_index(3) -> 2;
breakdown_index(2) -> 2;
breakdown_index(_) -> 1.


decode_breakdown_hash(Breakdown, _, _, _) when length(Breakdown) == 0 ->
    [];
decode_breakdown_hash(Breakdown, Salt, Seps, Alphabet) when is_list(Breakdown), length(Breakdown) > 0 ->
    [Lottery | T] = Breakdown,
    PreBuf        = [Lottery] ++ Salt,

    Replaced = replace_chars_with_whitespace_in_list(Seps, T),
    {_, R}   = lists:foldl( fun(E, {Alpha, Acc}) ->
                                Buf    = PreBuf ++ Alpha,
                                Alpha1 = consistent_shuffle(Alpha, lists:sublist(Buf, 1, length(Alpha))),

                                {Alpha1, [unhash(E, Alpha1) | Acc]}

                            end, {Alphabet, []}, string:tokens(Replaced, " ")),

    lists:reverse(R).


validate_alphabet(Alphabet) when is_list(Alphabet) ->
    ok = check_alphabet_len(Alphabet),
    ok = contains_space_in_alphabet(Alphabet),

    valid.

check_alphabet_len(Alphabet) when length(Alphabet) < ?MIN_ALPHABET_LEN -> {error, too_short_alphabet};
check_alphabet_len(_)                                                  -> ok.

contains_space_in_alphabet(Alphabet) ->
    case lists:member($\s, Alphabet) of
        true  -> {error, alphabet_error};
        false -> ok
    end.


validate_salt(Salt) when is_list(Salt) -> ok;
validate_salt(_)                       -> {error, invalid_salt}.


setup_sep(Alphabet, Salt) ->
  % seps should contain only characters present in alphabet; alphabet should not contains seps
  {NotIn, In}        = lists:partition(fun(S) -> lists:member(S, Alphabet) end, ?DEFAULT_SEPS),
  UnshuffledAlphabet = Alphabet -- NotIn,
  UnshuffledSeps     = ?DEFAULT_SEPS -- In,
  %% 对分割符号进行洗牌
  Seps                               = consistent_shuffle(UnshuffledSeps, Salt),
  {AdjustedSeps, AdjustedAlphabet}   = calculate_seps(Seps, UnshuffledAlphabet),
  ShuffledAlphabet                   = consistent_shuffle(AdjustedAlphabet, Salt),
  {AdjustedSeps, ShuffledAlphabet}.


calculate_seps(Seps, Alphabet) when length(Alphabet) == 0;
                                    length(Alphabet) div length(Seps) > (?SEP_DIV) ->

  SepLength = ceiling(length(Alphabet) / ?SEP_DIV),
  Length =
    case SepLength of
      1 -> 2;
      _ -> SepLength
    end,

  Diff = Length - length(Seps),
  if  Diff > 0 ->
      {Seps ++ lists:sublist(Alphabet, Diff), lists:sublist(Alphabet, Diff + 1, length(Alphabet) - Diff) };
      true ->
      {lists:sublist(Seps, 1, Length), Alphabet}
  end;
calculate_seps(Seps, Alphabet) ->
  {Seps, Alphabet}.


setup_guard(Seps, Alphabet) ->
    GC = ceiling(length(Alphabet) / ?GUARD_DIV),
    calculate_guard(GC, Seps, Alphabet).

calculate_guard(GC, Seps, Alphabet) when length(Alphabet) < 3 ->
    {lists:sublist(Seps, 1, GC), lists:sublist(Seps, GC + 1, length(Seps) - GC), Alphabet};
calculate_guard(GC, Seps, Alphabet) ->
    {lists:sublist(Alphabet, 1, GC), Seps, lists:sublist(Alphabet, GC + 1, length(Alphabet) - GC)}.


hash_numbers(Numbers) when is_list(Numbers) ->
    {HashInt, _} = lists:foldl(fun(Ele, {H, I}) -> {H + (Ele rem (I + 100)), I + 1} end, {0, 0}, Numbers), HashInt.


hash(0, Alphabet)     -> [lists:nth(1, Alphabet)];
hash(Input, Alphabet) -> hash_loop(Input, Alphabet, []).


hash_loop(0, _, Acc) ->
    Acc;
hash_loop(N, Alphabet, Acc) ->
    Len = length(Alphabet),
    hash_loop(N div Len, Alphabet, [lists:nth((N rem Len) + 1, Alphabet) | Acc]).


unhash(Input, Alphabet) ->
    lists:foldl(
      fun(Item, Carry) ->
        Carry * length(Alphabet) + Item
      end,
      0,
      lists:map(
        fun(Item) ->
          {ok, Y} =
          case string:chr(Alphabet, Item) of
            0 -> {error, cannot_unhash};
            X -> {ok , X - 1}
          end, Y
        end,
        Input
      )
    ).


%% ===================================================================
%% Helper Functions
%% ===================================================================
get_value(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    _ -> Default
  end.


%% 当没有salt的时候，直接使用字母表
consistent_shuffle(Alphabet, []) -> Alphabet;
consistent_shuffle(Alphabet, Salt) ->
  %% 获取salt的长度
  SaltLength = length(Salt),
  {Shuffled, _, _, _}  =  lists:foldr(
                            fun(_, {_, _, _, 0} = Acc) ->
                                Acc;
                               (_, {Al, V, P, L}) ->
                                %% V会在0 至 SaltLength -1之间循环
                                %% P是根据Salt字母+V的位移动值
                                V1 = V rem SaltLength,
                                N  = lists:nth(V1 + 1, Salt), %% 从盐中取出字母N
                                P1 = P + N, %% 字母N的位置移动P
                                J   = (N + V1 + P1) rem L, %% (N + V1 + P + N) rem L
                                Al1 = swap(Al, J, L), %% 交换位置
                                {Al1, V1 + 1, P + N, L - 1}
                                end, {Alphabet, 0, 0, length(Alphabet) - 1}, Alphabet),
  Shuffled.

%% 将S1位置的字母和S2位置的字母进行交换
swap(List, S1, S2) ->
  {List2, [F | List3]} = lists:split(S1, List), %% 在S1处将字母表分开，将位置S1的字母取出
  LT = List2 ++ [lists:nth(S2 + 1, List) | List3],  %% 将S2处的字母放到S1处
  {List4, [_ | List5]} = lists:split(S2, LT),  %% 删除S2处的字母
  List4 ++ [F | List5]. %% 将S1处的字母放到S2处


ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.


parts(List, Max) ->
     RevList = split_list(List, Max),
     lists:foldl(fun(E, Acc) ->
         [lists:reverse(E)|Acc]
     end, [], RevList).


split_list(List, Max) ->
    element(1, lists:foldl(fun
        (E, {[Buff|Acc], C}) when C < Max ->
            {[[E|Buff]|Acc], C+1};
        (E, {[Buff|Acc], _}) ->
            {[[E],Buff|Acc], 1};
        (E, {[], _}) ->
            {[[E]], 1}
    end, {[], 0}, List)).
