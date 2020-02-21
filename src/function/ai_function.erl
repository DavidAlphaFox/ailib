-module(ai_function).
-export([run_mfa/1,run_mfa/2,run_catch_mfa/1, run_catch_mfa/2]).
-export([lookup_fun/3,apply_fun/4]).
-export([while/2]).

-spec run_mfa({module(),atom(),[term()]}|{function(),[term()]})->term().
run_mfa({M,F,A})-> erlang:apply(M,F,A);
run_mfa({F,A}) -> erlang:apply(F,A).

-spec run_mfa({module(),atom(),[term()]}|{function(),[term()]},[term()])->term().
run_mfa({M,F,A},Others) -> erlang:apply(M,F,A ++ Others);
run_mfa({F,A},Others) -> erlang:apply(F,A ++ Others).

-spec run_catch_mfa({module(),atom(),[term()]}|{function(),[term()]}) ->term().
run_catch_mfa(MFA)->
  try
    run_mfa(MFA)
  catch
    Error:Reason -> {exception,Error,Reason}
  end.

-spec run_catch_mfa({module(),atom(),[term()]}|{function(),[term()]},[term()]) ->term().
run_catch_mfa(MFA,Others)->
  try
    run_mfa(MFA,Others)
  catch
    Error:Reason -> {exception,Error,Reason}
  end.

-spec lookup_fun(atom(),list()|binary(),integer())-> boolean().
lookup_fun(Mod,Fun,ArgCount) when erlang:is_list(Fun)->
  FunBin = erlang:list_to_binary(Fun),
  lookup_fun(Mod,FunBin,ArgCount);
lookup_fun(Mod,Fun,ArgCount)->
  Exports = Mod:module_info(exports),
  lists:filter(fun(FunInMod)->
                   {FunName,AC} = FunInMod,
                   FunNameBin = erlang:atom_to_binary(FunName,latin1),
                   FunNameBin =:= Fun andalso AC == ArgCount
               end,Exports).
-spec apply_fun(module(),atom(),arity(),[term()])->term().
apply_fun(Mod,Fun,ArgCount,Args)->
  Res = lookup_fun(Mod,Fun,ArgCount),
  [{Fun,_AC}] = Res,
  erlang:apply(Mod,Fun,Args).

-spec while({boolean(), term()}, fun((term()) -> {boolean(), term()})) -> term().
while({true, Value}, Fun) -> while(Fun(Value), Fun);
while({false, Value}, _Fun) -> Value.
