-module(ai_tag).
-export([replace/2,replace/4]).
%% 该模块只用于简单的tag替换
%% 虽然aihtml项目中提供了该功能，但aihtml的功能过于复杂
%% 可以将该模块视为aihtml的降级版本

-define(START_TAG, <<"{{">>).
-define(STOP_TAG,  <<"}}">>).

-define(IIF(Cond, TValue, FValue),
        case Cond of true -> TValue; false -> FValue end).
-define(ADD(X, Y), ?IIF(X =:= <<>>, Y, [X | Y])).

-record(state,{
        start    = ?START_TAG :: binary(),
        stop     = ?STOP_TAG  :: binary()
    }).

replace(Line,Context)->
    {_State,Tags} = parse(#state{},Line,[]),
    Parsed = lists:reverse(Tags),
    render(Parsed,Context).

replace(Line,StartTag,StopTag,Context)->
     {_State,Tags} = parse(#state{
        start = ai_string:to_string(StartTag),
        stop = ai_string:to_string(StopTag)
    },Line,[]),
    Parsed = lists:reverse(Tags),
    render(Parsed,Context).

parse(#state{start = StartTag} = State,Bin, Result) ->
    case binary:match(Bin, [StartTag]) of %% 找StartTag，或者\n
        nomatch -> {State, ?ADD(Bin, Result)}; %% 整个就是个binary
        {_S, _L}  -> %% 找到StartTag或者\n了
            Split = split_tag(State, Bin),
            parse1(State,Split, Result) %% 找到标签了，整个文本向前找标签
    end.

parse1(State, [B1, B2, B3], Result) ->
    Tag0 = remove_space_from_head(B2),
    Tag  = remove_space_from_tail(Tag0),
    parse(State, B3, [{tag,Tag} | ?ADD(B1, Result)]);
 parse1(_, _, _) -> error({error, unclosed_tag}).

remove_space_from_head(<<X:8, Rest/binary>>) 
 when X =:= $\t; X =:= $  -> 
         remove_space_from_head(Rest);
remove_space_from_head(Bin) -> Bin.

-spec remove_space_from_tail(binary()) -> binary().
remove_space_from_tail(<<>>) -> <<>>;
remove_space_from_tail(Bin) ->
    PosList = binary:matches(Bin, <<" ">>),
    LastPos = remove_space_from_tail_impl(lists:reverse(PosList), byte_size(Bin)),
    binary:part(Bin, 0, LastPos).

-spec remove_space_from_tail_impl([{non_neg_integer(), pos_integer()}], non_neg_integer()) -> non_neg_integer().
remove_space_from_tail_impl([{X, Y} | T], Size) when Size =:= X + Y -> remove_space_from_tail_impl(T, X);
remove_space_from_tail_impl(_, Size) ->Size.


%% @doc Split by the tag, it returns a list of the split binary.
%%
%% e.g.
%% ```
%% 1> split_tag(State, <<"...{{hoge}}...">>).
%% [<<"...">>, <<"hoge">>, <<"...">>]
%%
%% 2> split_tag(State, <<"...{{hoge ...">>).
%% [<<"...">>, <<"hoge ...">>]
%%
%% 3> split_tag(State, <<"...">>)
%% [<<"...">>]
%% '''
split_tag(#state{start = StartTag, stop = StopTag}, Bin) ->
    case binary:match(Bin, StartTag) of
        nomatch -> [Bin]; %% 未找到开始标签
        {StartPos, StartTagLen} ->
            PosLimit = byte_size(Bin) - StartTagLen, 
            ShiftNum = ai_function:while({true, StartPos + 1}, %% 在下一个StartTag之前一直向前推进
                             fun(Pos) ->  %% {{{ ,startPos 0, StartDelimiterLen = 2 ShitNum = 1
                                     ?IIF(Pos =< PosLimit
                                          andalso binary:part(Bin, Pos, StartTagLen) =:= StartTag,
                                          {true, Pos + 1}, {false, Pos})
                             end) - StartPos - 1,
            %% PreTag是StartTag之前的文本，X是包含StarTag的文本
            {PreTag, X} = erlang:split_binary(Bin, StartPos + ShiftNum), 
            Tag0  =  binary:part(X, StartTagLen, byte_size(X) - StartTagLen), %%  去掉StartTag
            case binary:split(Tag0, StopTag) of
                [_] -> [PreTag, Tag0]; % 这段文本里面没有StopTag
                [TagName, Rest]  -> [PreTag, TagName, Rest]
            end
    end.

render(Parsed,Context)->
    render(Parsed,Context,<<>>).
render([],_Context,Acc)->Acc;
render([{tag,Tag}|T],Context,Acc)->
    Acc0 =
        case maps:get(Tag,Context,undefined) of 
            undefined -> Acc;
            Value -> 
                BValue = ai_string:to_string(Value),
                <<Acc/binary,BValue/binary>>
        end,
    render(T,Context,Acc0);
render([Bin|T],Context,Acc)->
    Acc0 = <<Acc/binary,Bin/binary>>,
    render(T,Context,Acc0).