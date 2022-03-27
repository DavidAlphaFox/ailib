-module(ailib_atom_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").


all() -> 
[
 auto_create_with_prefix,
 auto_create_with_suffix,
 no_auto_create_with_prefix,
 no_auto_create_with_suffix,
 using_exist_with_prefix,
 using_exist_with_suffix
].

init_per_testcase(_,Config)->  
  [
   {prefix_atom,<<"atom">>},
   {atom_suffix,<<"atom">>},
   {none_exist_atom,<<"other_atom">>}
  | Config].
end_per_testcase(_,_)-> ok.

auto_create_with_prefix(Config)->
  ct:log("ailib_atom:preix auto create new atom"),
  Text = ?config(none_exist_atom,Config),
  NewAtom =  ailib_atom:prefix(Text, "prefix_",false),
  NewAtom0 = erlang:atom_to_binary(NewAtom),
  case NewAtom0 == <<"prefix_other_atom">> of
    true -> ok;
    false ->  ct:fail(prefix_create_wrong_atom)
  end.

auto_create_with_suffix(Config)->
  ct:log("ailib_atom:suffix auto create new atom"),
  Text = ?config(none_exist_atom,Config),
  NewAtom =  ailib_atom:suffix(Text, "_suffix",false),
  NewAtom0 = erlang:atom_to_binary(NewAtom),
  case NewAtom0 == <<"other_atom_suffix">> of
    true -> ok;
    false ->  ct:fail(suffix_create_wrong_atom)
  end.

no_auto_create_with_prefix(Config)->
  ct:log("ailib_atom:prefix won't auto create new atom"),
  Text = ?config(none_exist_atom,Config),
  try
    ailib_atom:prefix(Text, suffix,true),
    ct:fail(prefix_auto_create_new_atom)
  catch
    error:badarg -> ok
  end.

no_auto_create_with_suffix(Config)->
  ct:log("ailib_atom:suffix won't auto create new atom"),
  Text = ?config(none_exist_atom,Config),
  try
    ailib_atom:suffix(Text, suffix,true),
    ct:fail(suffix_auto_create_new_atom)
  catch
    error:badarg -> ok
  end.

using_exist_with_prefix(Config)->
  ct:log("ailib_atom:preix with exist atom prefix"),
  Text = ?config(prefix_atom,Config),
  case prefix_atom == ailib_atom:prefix(Text, "prefix_",true) of
    true -> ok;
    false -> ct:fail(prefix_create_wrong_atom)
  end.


using_exist_with_suffix(Config)->
  ct:log("ailib_atom:suffix with  exist atom suffix"),
  Text = ?config(atom_suffix,Config),
  case atom_suffix == ailib_atom:suffix(Text, "_suffix",true) of
    true -> ok;
    false -> ct:fail(suffix_create_wrong_atom)
  end.
