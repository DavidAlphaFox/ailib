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
-define(SERVER, ?MODULE).
-define(RAND, begin
                case code:ensure_loaded(rand) of
                  {module, rand} -> rand;
                  _ -> random
                end
              end).
-define(SEED_STATE, begin
                      case code:ensure_loaded(rand) of
                        {module, rand} -> exs1024;
                        _ -> erlang:system_time(micro_seconds)
                      end
                    end).
-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([rand/1, rand/2]).
start()->
		gen_server:start({local, ?SERVER}, ?MODULE, [], []).
% @hidden
start_link() ->
		gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec rand(Length::integer()) -> string().
rand(Length) ->
  rand(Length, ?CHARS).

-spec rand(Length::integer(), Allowed::list()) -> string().
rand(Length, Allowed) ->
  _ = ensure_started(),
  gen_server:call(?SERVER, {rand, Length, Allowed}).

% @hidden
init(Args) ->
  _ = erlang:apply(?RAND, seed, [?SEED_STATE]),
  {ok, Args}.

% @hidden
handle_call({rand, Length, Allowed}, _From, State) ->
  {reply, private_rand(Length, Allowed), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Service implementation

private_rand(Size, Allowed) ->
  lists:flatten([lists:sublist(Allowed, erlang:apply(?RAND, uniform, [length(Allowed)]), 1) || _ <- lists:seq(1, Size)]).

oneshot()->
	case ai_ascii_random:start() of
				{error,{already_started,Pid}} -> {ok,Pid};
				R -> R
	end.
ensure_started() ->
    case erlang:whereis(?SERVER) of
        undefined -> oneshot();		
        Pid -> {ok,Pid}
    end.

