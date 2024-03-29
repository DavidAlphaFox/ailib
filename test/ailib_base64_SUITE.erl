-module(ailib_base64_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").


all() ->[encode,decode].

init_per_testcase(_,Config)->  [{text,<<"ailib_base64 common test">>} | Config].
end_per_testcase(_,_)-> ok.

encode(Config)->
  ct:log("ailib_base64:encode/1 shoule equal base64:encode/1"),
  Text = ?config(text,Config),
  case ailib_base64:encode(Text) == base64:encode(Text) of 
    true -> ok;
    false -> ct:fail(not_euqal)
  end.

decode(Config)->
  ct:log("ailib_base64:decode/1 can decode result of  base64:encode/1"),
  Text = ?config(text,Config),
  case Text == ailib_base64:decode(base64:encode(Text)) of
    true -> ok;
    false -> ct:fail(can_not_decode)
  end.
