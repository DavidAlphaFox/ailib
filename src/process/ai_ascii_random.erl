% @doc
%% @author Grégoire Lejeune <gregoire.lejeune@botsunit.com>
%% @author Mathias Franck <mathias.franck@botsunit.com>
%% @copyright 2016 BotsUnit
%%
%% Erlang server for generating random strings, usable as file names, tokens,...
%%
%% The implementation avoids problems with "rand-seeding"
%% the Erlang random generator between processes : Using bucrandom:randstr/1
%% guarantees a good random distribution, and low probability of getting
%% the same value at first call, from wherever the function is called.
%% However, it may not be appropriate for cryptographic or 'sensible' purposes.
%% @end
-module(ai_ascii_random).
-behaviour(gen_server).

-export([start_link/0,start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([rand/1, rand/2]).
-export([randp/2,randp/3]).

-define(SERVER, ?MODULE).
-define(RAND, 
        begin
          case code:ensure_loaded(rand) of
            {module, rand} -> rand;
            _ -> random
          end
        end).
-define(SEED_STATE, 
        begin
          case code:ensure_loaded(rand) of
            {module, rand} -> exs1024;
            _ -> erlang:system_time(microseconds)
          end
        end).
-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").
-record(state,{chars}).

% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Args)->
  Type = proplists:get_value(register,Args),
  case Type of
    undefined -> gen_server:start_link(?MODULE,Args,[]);
    _ -> gen_server:start_link({Type, ?SERVER}, ?MODULE, [], [])
  end.

randp(Pid,Length)->
  gen_server:call(Pid,{rand,Length}).
randp(Pid,Length,Allowed)->
  gen_server:call(Pid,{rand,Length,Allowed}).

-spec rand(Length::integer()) -> string().
rand(Length) -> randp(?SERVER,Length).

-spec rand(Length::integer(), Allowed::list()) -> string().
rand(Length, Allowed) -> randp(?SERVER,Length,Allowed).

% @hidden
init(Args) ->
  Chars = proplists:get_value(chars,Args,?CHARS),
  _ = erlang:apply(?RAND, seed, [?SEED_STATE]),
  {ok, #state{chars = Chars}}.

% @hidden
handle_call({rand,Length},_From,#state{chars = Allowed} = State)->
  {reply, private_rand(Length, Allowed), State};
handle_call({rand, Length, Allowed}, _From, State) ->
  {reply, private_rand(Length, Allowed), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% @hidden
handle_cast(_Msg, State) -> {noreply, State}.

% @hidden
handle_info(_Info, State) -> {noreply, State}.

% @hidden
terminate(_Reason, _State) -> ok.

% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% Service implementation

private_rand(Size, Allowed) ->
  AllowedLength = length(Allowed),
  lists:flatten([lists:sublist(Allowed, erlang:apply(?RAND, uniform, [AllowedLength]), 1)
                 || _ <- lists:seq(1, Size)]).
