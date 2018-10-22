-module(ai_function).
-export([run_mfa/1,run_mfa/2,run_catch_mfa/1, run_catch_mfa/2]).
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
		
			

