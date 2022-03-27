-module(ailib_atom_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").


all() -> 
[
 not_exist_prefix,
 not_exist_suffix,
 exist_prefix,
 exist_suffix
].

init_per_testcase(_,Config)->  
  [
   {prefix_atom,<<"atom">>},
   {atom_suffix,<<"atom">>},
   {none_exist_atom,<<"other_atom">>}
  | Config].
end_per_testcase(_,_)-> ok.

not_exist_prefix(Config)->
  ct:log("ailib_atom:preix with none exist atom prefix"),
  Text = ?config(none_exist_atom,Config),
  try
    ailib_atom:prefix(Text, prefix,true),
    false
  catch
    error:badarg -> true
  end.
not_exist_suffix(Config)->
  ct:log("ailib_atom:suffix with none exist atom suffix"),
  Text = ?config(none_exist_atom,Config),
  try
    ailib_atom:suffix(Text, suffix,true),
    false
  catch
    error:badarg -> true
  end.

exist_prefix(Config)->
  ct:log("ailib_atom:preix with exist atom prefix"),
  Text = ?config(prefix_atom,Config),
  try
    prefix_atom == ailib_atom:prefix(Text, prefix,true)
  catch
    error:badarg -> false
  end.

exist_suffix(Config)->
  ct:log("ailib_atom:suffix with  exist atom suffix"),
  Text = ?config(atom_suffix,Config),
  try
    atom_suffix == ailib_atom:suffix(Text, suffix,true)
  catch
    error:badarg -> false
  end.

