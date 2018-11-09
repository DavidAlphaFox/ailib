-module(ai_function).
-export([run_mfa/1,run_mfa/2,run_catch_mfa/1, run_catch_mfa/2]).
-export([lookup_func/3,apply_func/4]).

run_mfa({M,F,A})-> erlang:apply(M,F,A);
run_mfa({F,A}) -> erlang:apply(F,A).

run_mfa({M,F,A},Others) -> erlang:apply(M,F,A ++ Others);
run_mfa({F,A},Others) -> erlang:apply(F,A ++ Others).

run_catch_mfa(MFA)->
		try 
				run_mfa(MFA)
		catch
				Error:Reason -> {exception,Error,Reason}
		end.

run_catch_mfa(MFA,Others)->
		try 
				run_mfa(MFA,Others)
		catch
				Error:Reason -> {exception,Error,Reason}
		end.
		
			


lookup_func(Mod,Func,ArgCount) when erlang:is_list(Func)->
	FuncBin = erlang:list_to_binary(Func),
	lookup_func(Mod,FuncBin,ArgCount);
lookup_func(Mod,Func,ArgCount)->
	Exports = Mod:module_info(exports),
	lists:filter(fun(FuncInMod)->
					{FuncName,AC} = FuncInMod,
					FuncNameBin = erlang:atom_to_binary(FuncName,latin1),
					FuncNameBin =:= Func andalso AC == ArgCount
				end,Exports).
apply_func(Mod,Func,ArgCount,Args)->
	Res = lookup_func(Mod,Func,ArgCount),
	[{Fun,_AC}] = Res,
	erlang:apply(Mod,Fun,Args).