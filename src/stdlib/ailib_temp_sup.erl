-module(ailib_temp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: map()) -> {ok, Pid :: pid()} |
        {error, {already_started, Pid :: pid()}} |
        {error, {shutdown, term()}} |
        {error, term()} |
        ignore.

start_link(Args) ->
  Children = maps:get(children,Args,[]),
  SupFlags = maps:filter(
               fun(K,_V)-> (K /= name) and (K /= children) end,
               Args),
	case maps:get(name,Args,undefined) of
    undefined -> supervisor:start_link(?MODULE,{SupFlags,Children});
    Name when erlang:is_tuple(Name) -> supervisor:start_link(Name,?MODULE,{SupFlags,Children});
    Name -> supervisor:start_link({local,Name},?MODULE,{SupFlags,Children})
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: {term(),term()}) ->
        {ok, {SupFlags :: supervisor:sup_flags(),
              [ChildSpec :: supervisor:child_spec()]}} |
        ignore.
init({SupFlags,Children}) ->
  {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
