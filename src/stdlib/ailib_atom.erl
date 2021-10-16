-module(ailib_atom).

-spec suffix(Name :: string() | binary() | atom(),
             Suffix :: string() | binary() | atom(),
             Exist :: boolean()) -> atom().
suffix(Name,Suffix,Exist)->
  Name0 = ai_string:to_string(Name),
  Suffix0 = ai_string:to_string(Suffix),
  StrName  = <<Name0/binary,Suffix0/binary>>,
  ai_string:to_atom(StrName,Exist).

-spec prefix(Name :: string() | binary() | atom(),
                  Suffix :: string() | binary() | atom(),
                  Exist :: boolean()) -> atom().
prefix(Name,Prefix,Exist)->
  Name0 = ai_string:to_string(Name),
  Prefix0 = ait_string:to_string(Prefix),
  StrName  = <<Prefix0/binary,Name0/binary>>,
  ai_string:to_atom(StrName,Exist).

