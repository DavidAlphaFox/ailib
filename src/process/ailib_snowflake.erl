% The id server will generate a 64-bit integer base on utc time.
% The id is k-order shortable
%
%
% Bits   Description
% 1      Signeddness flag,always 0.Because thrift only supports signed 64-bit integer And I don't want a negtive integer.
% 41     Unix timestamp,down to the millisecond
% 5      Top 5 bits of node number
% 5      then 5 bits of partition number
% 12     Per-partition static increasing counter
% +------------------+------------------+--------------+
% |0|1     ...     41|42      ...     51|52   ...    63|
% +------------------+------------------+--------------+
% |0| Unix Timestamp | Partition Number |    Counter   |
%


-module(ailib_snowflake).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([next/1]).

%-define (EPOCH,1356998400974295).%2013-1-1 0:0:0 {1356,998400,974295} microseconds

-define (EPOCH,1356998400974). %2013-1-1 0:0:0 {1356,998400,974295} milliseconds

-record(state,{
               partition,
               sequence = 0,
               last_timestamp
              }).


start_link(Args)->
  Partition = proplists:get_value(partition,Args,0),
  Name = server_name(Partition),
  gen_server:start_link({local,Name},?MODULE,Partition,[]).

%%%
%%% API
%%%
-spec next(integer() | pid()) -> {error, backwards_clock} | {ok, integer()}.
next(ParOrPid)->
  ServerName =
    if  erlang:is_pid(ParOrPid) -> ParOrPid;
        true -> server_name(ParOrPid)
    end,
  gen_server:call(ServerName,next_id).

%%%
%%% gen_server callback
%%%

init(Partition)->
  timer:sleep(1),
  TS = timestamp(),
  PartitionBinary = <<Partition:10/integer-unsigned>>,
  {ok,#state{
         partition = PartitionBinary,
         sequence = 0,
         last_timestamp = TS}}.


handle_call(next_id,From, #state{last_timestamp = TS, sequence = Seq, partition = Partition} = State) ->
  case try_next_seq(TS, Seq) of
    backwards_clock ->
      {reply, {error, backwards_clock}, State};
    exhausted ->
      timer:sleep(1),
      handle_call(next_id, From, State);
    {ok, Time, NewSeq} ->
      {reply, next_id(Time, Partition, NewSeq), State#state{last_timestamp = Time, sequence = NewSeq}}
  end;

handle_call(_Msg,_From,State)->
    {reply,ok,State}.
handle_cast(_Msg,State)->
    {noreply,State}.
handle_info(_Msg,State)->
    {noreply,State}.

terminate(_Reason, _State) ->
	ok.
code_change(_Old, State, _Extra) ->
    {ok, State}.

%%%
%%% inner functions
%%%
timestamp()-> erlang:system_time(millisecond) - ?EPOCH.
next_seq(Now,Seq)->
  case (Seq + 1) rem 4096 of
    0 -> exhausted;
    NewSeq -> {ok,Now,NewSeq}
  end.
try_next_seq(Time, Seq) ->
  Now = timestamp(),
  if
    Now =:= Time -> next_seq(Now,Seq);% Time is essentially equal at the millisecond
    Now < Time ->backwards_clock; % Woops, clock was moved backwards by NTP
    true ->{ok, Now, 0}  % New millisecond
  end.

next_id(Millis, Partition, Seq) ->
    <<Integer:64/big-unsigned-integer>> = <<0:1, Millis:41/integer-unsigned,
                                            Partition:10/bits, Seq:12/integer-unsigned>>,
    {ok,Integer}.

server_name(Partition)->
  Par = ailib_string:to_binary(Partition),
  ailib_atom:suffix(?MODULE, <<"_",Par/binary>>).
