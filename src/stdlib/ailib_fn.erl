-module(ailib_fn).
-compile({no_auto_import,[apply/2,apply/3]}).
-export([apply/1,
         apply/2,
         catch_apply/1,
         catch_apply/2,
         has/3,
         while/2,
         pipe/1,
         pipe/2,
         foreach/1,
         foreach/2]).

-spec apply({module(),atom(),[term()]}
           |{function(),[term()]})->term().
apply({M,F,A})-> erlang:apply(M,F,A);
apply({F,A}) -> erlang:apply(F,A).

-spec apply({module(),atom(),[term()]}
           |{function(),[term()]},
            [term()])->term().
apply({M,F,A},Others) -> erlang:apply(M,F,A ++ Others);
apply({F,A},Others) -> erlang:apply(F,A ++ Others).

-spec catch_apply({module(),atom(),[term()]}
               |{function(),[term()]}) -> term()|{catch_apply,term(),term()}.
catch_apply(MFA)->
  try
    apply(MFA)
  catch
    Error:Reason -> {catch_apply,Error,Reason}
  end.

-spec catch_apply({module(),atom(),[term()]}
               |{function(),[term()]},[term()]) -> term()|{catch_apply,term(),term()}.
catch_apply(MFA,Others)->
  try
    apply(MFA,Others)
  catch
    Error:Reason -> {catch_apply,Error,Reason}
  end.

-spec has(atom(),list()|binary(),integer())-> boolean().
has(Mod,FunName,ArgCount)->
  Exports = Mod:module_info(exports),
  try
    FunName0 = fun_name(FunName),
    Found = lists:filter(fun({FName,AC})->
                             FunName0 =:= FName andalso AC == ArgCount
                         end,Exports),
    erlang:length(Found) > 0
  catch
    _Error:_Reason ->
      false
  end.


-spec fun_name(atom()|list()|binary()) -> atom().
fun_name(FunName) when erlang:is_atom(FunName) ->
  FunName;
fun_name(FunName) ->
  FunName0 = ailib_string:to_binary(FunName),
  ailib_string:to_atom(FunName0, true).


-spec while(term(),
            fun((term()) -> {boolean(), term()})) -> term().
while(Value,Fun)-> while_internal({true,Value},Fun).

-spec while_internal({boolean(), term()},
                     fun((term()) -> {boolean(), term()})) -> term().
while_internal({true,Value}, Fun) -> while_internal(Fun(Value), Fun);
while_internal({false,Value}, _Fun) -> Value.


-spec pipe(List :: [term()]) -> term().
pipe(L)-> pipe(L,[]).

-spec pipe(List :: [term()],
           Acc :: [term()]) -> term().
pipe([H|T],Acc)->
  R = apply(H,Acc),
  pipe(T,[R]);
pipe([],[R]) -> R;
pipe([],[]) -> undefined.


-spec foreach([{term(),term()}| {term(),term(),term()}]) -> {foreach, done} | term().
foreach([Fun|T])->
  case apply(Fun) of
    continue -> foreach(T);
    V -> V
  end;
foreach([]) -> {foreach,done}.

-spec foreach([{term(),term()}|{term(),term(),term()}],
              [term()]) -> {foreach,done} | term().
foreach([Fun|T],Others)->
  case apply(Fun,Others) of
    continue -> foreach(T,Others);
    V -> V
  end;
foreach([],_) -> {foreach,done}.
