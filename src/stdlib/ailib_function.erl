-module(ailib_function).
-export([run/1,
         run/2,
         run_catch/1,
         run_catch/2,
         has/3,
         while/2,
         pipe/1,
         pipe/2,
         foreach/1,
         foreach/2]).

-spec run({module(),atom(),[term()]}
           |{function(),[term()]})->term().
run({M,F,A})-> erlang:apply(M,F,A);
run({F,A}) -> erlang:apply(F,A).

-spec run({module(),atom(),[term()]}
           |{function(),[term()]},
            [term()])->term().
run({M,F,A},Others) -> erlang:apply(M,F,A ++ Others);
run({F,A},Others) -> erlang:apply(F,A ++ Others).

-spec run_catch({module(),atom(),[term()]}
               |{function(),[term()]}) -> term()|{run_catch,term(),term()}.
run_catch(MFA)->
  try
    run(MFA)
  catch
    Error:Reason -> {run_catch,Error,Reason}
  end.

-spec run_catch({module(),atom(),[term()]}
               |{function(),[term()]},[term()]) -> term()|{run_catch,term(),term()}.
run_catch(MFA,Others)->
  try
    run(MFA,Others)
  catch
    Error:Reason -> {run_catch,Error,Reason}
  end.

-spec has(atom(),list()|binary(),integer())-> boolean().
has(Mod,Fun,ArgCount) when erlang:is_list(Fun)->
  FunBin = erlang:list_to_binary(Fun),
  has(Mod,FunBin,ArgCount);
has(Mod,Fun,ArgCount)->
  Exports = Mod:module_info(exports),
  Found = lists:filter(fun({FunName,AC})->
                           FunNameBin = erlang:atom_to_binary(FunName,latin1),
                           FunNameBin =:= Fun andalso AC == ArgCount
                       end,Exports),
  erlang:length(Found) > 0.



-spec while({boolean(), term()},
            fun((term()) -> {boolean(), term()})) -> term().
while({true,Value}, Fun) -> while(Fun(Value), Fun);
while({false,Value}, _Fun) -> Value.


-spec pipe(List :: [term()]) -> term().
pipe(L)-> pipe(L,[]).

-spec pipe(List :: [term()],
           Acc :: [term()]) -> term().
pipe([H|T],Acc)->
  R = run(H,Acc),
  pipe(T,[R]);
pipe([],[R]) -> R;
pipe([],[]) -> undefined.


-spec foreach([{term(),term()}| {term(),term(),term()}]) -> foreach_done | term().
foreach([Fun|T])->
  case run(Fun) of
    continue -> foreach(T);
    V -> V
  end;
foreach([]) -> foreach_done.

-spec foreach([{term(),term()}|{term(),term(),term()}],
              [term()]) -> foreach_done | term().
foreach([Fun|T],Others)->
  case run(Fun,Others) of
    continue -> foreach(T,Others);
    V -> V
  end;
foreach([],_) -> foreach_done.
