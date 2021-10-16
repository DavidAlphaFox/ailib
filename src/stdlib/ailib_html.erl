-module(ailib_html).

-export([escape/1,
        unescape/1]).

-define(HTML_ESCAPE,[
                     {"\\&","\\&amp"},{"<","\\&lt;"},{">","\\&gt;"},
                     {"\"","\\&quot;"},{"'","\\&#39;"},{"/","\\&#x2F;"},
                     {"=","\\&#x3D;"},
                     {"`","\\&#x60;"} %% delimiter in IE
                    ]).
escape(Str)->
  BinStr = ai_string:to_binary(Str),
  lists:foldl(
    fun({El,Replace},Acc)->
        re:replace(Acc,El,Replace,[global,{return,binary}])
    end,BinStr,?HTML_ESCAPE).

unescape(Str)->
  BinStr = ai_string:to_binary(Str),
  lists:foldr(
    fun({Replace,El},Acc)->
        re:replace(Acc,El,Replace,[global,{return,binary}])
    end,BinStr,?HTML_ESCAPE).
