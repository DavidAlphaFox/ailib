-module(ai_rfc822_date).
-include("ailib.hrl").

-export([parse/1]).
-export([month_to_number/1,number_to_month/1]).
-export([day_to_number/1,number_to_day/1]).
-export([universal_time/1]).
parse(Date) when erlang:is_list(Date)->
		parse(erlang:list_to_binary(Date));
parse(Date) ->
	parse(Date,day,#rfc822_date{}).

parse(<<Day:3/binary, $, ," " ,Rest/bits>>,day,R)->
		parse(Rest,date,R#rfc822_date{day = Day});

parse(<<Date:2/binary," ",Rest/bits>>,date,R) ->
    parse(Rest,month,R#rfc822_date{date = erlang:binary_to_integer(Date)});

parse(<<Month:3/binary," ",Rest/bits>>,month,R) ->
    parse(Rest,year,R#rfc822_date{month = Month});
parse(<<Year:4/binary," ",Rest/bits>>,year,R) ->
    parse(Rest,hour,R#rfc822_date{year = erlang:binary_to_integer(Year)});
parse(<<H:2/binary,":",Rest/bits>>,hour,R) ->
    parse(Rest,minute,R#rfc822_date{hour = erlang:binary_to_integer(H)});
parse(<<M:2/binary,":",Rest/bits>>,minute,R) ->
    parse(Rest,second,R#rfc822_date{minute = erlang:binary_to_integer(M)});
parse(<<Sec:2/binary," ",Rest/bits>>,second,R) ->
    parse(Rest,zone,R#rfc822_date{second = erlang:binary_to_integer(Sec)});
parse(<<"+",ZH:2/binary,ZM:2/binary>> = Z,zone,R)->
    R#rfc822_date{zone = Z,utc_diff =  {erlang:binary_to_integer(ZH),erlang:binary_to_integer(ZM)}};
parse(<<"-",ZH:2/binary,ZM:2/binary>> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff =  {- erlang:binary_to_integer(ZH),- erlang:binary_to_integer(ZM)}};
parse(<<"GMT">> = Z,zone,R)->
    R#rfc822_date{zone = Z,utc_diff = {0,0}};
parse(<<"UT">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff = {0,0}};
parse(<<"EST">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-5,0}};
parse(<<"EDT">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-4,0}};
parse(<<"CST">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-6,0}};
parse(<<"CDT">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-5,0}};
parse(<<"MST">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-7,0}};
parse(<<"MDT">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-6,0}};
parse(<<"PST">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-8,0}};
parse(<<"PDT">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-7,0}};
parse(<<"A">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-1,0}};
parse(<<"M">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {-12,0}};
parse(<<"N">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {1,0}};
parse(<<"Y">> = Z,zone,R) ->
    R#rfc822_date{zone = Z,utc_diff= {12,0}}.

months()->
    [
     {<<"Jan">>,1},
     {<<"Feb">>,2},
     {<<"Mar">>,3},
     {<<"Apr">>,4},
     {<<"May">>,5},
     {<<"Jun">>,6},
     {<<"Jul">>,7},
     {<<"Aug">>,8},
     {<<"Sep">>,9},
     {<<"Oct">>,10},
     {<<"Nov">>,11},
     {<<"Dec">>,12}
    ].
days()->
    [
     {<<"Mon">>,1},
     {<<"Tue">>,2},
     {<<"Wed">>,3},
     {<<"Thu">>,4},
     {<<"Fri">>,5},
     {<<"Sat">>,6},
     {<<"Sun">>,7}
    ].
dates()->
    [
     {<<"Jan">>,31},
     {<<"Feb">>,28},
     {<<"Mar">>,31},
     {<<"Apr">>,30},
     {<<"May">>,31},
     {<<"Jun">>,30},
     {<<"Jul">>,31},
     {<<"Aug">>,31},
     {<<"Sep">>,30},
     {<<"Oct">>,31},
     {<<"Nov">>,30},
     {<<"Dec">>,31}
    ].
month_to_number(Month) when erlang:is_list(Month)->
    month_to_number(erlang:list_to_binary(Month));
month_to_number(Month) ->
    proplists:get_value(Month,months(),error).

number_to_month(Num) when Num < 1; Num > 12->
    error;
number_to_month(Num)->
    {Month,Num} =  lists:nth(Num,months()),
    Month.

day_to_number(Day) when erlang:is_list(Day)->
    day_to_number(erlang:list_to_binary(Day));
day_to_number(Day) ->
    proplists:get_value(Day,days(),error).

number_to_day(Num) when Num < 1; Num > 7->
    error;
number_to_day(Num)->
    {Day,Num} =  lists:nth(Num,days()),
    Day.

universal_time(#rfc822_date{utc_diff = {0,0}} =  R)->
    {
     { R#rfc822_date.year, month_to_number(R#rfc822_date.month),R#rfc822_date.date},
     { R#rfc822_date.hour,R#rfc822_date.minute,R#rfc822_date.second}
    };
universal_time(#rfc822_date{utc_diff = {DH,DM}} = R) ->
    {SH,NewMinute} = diff_minute(R#rfc822_date.minute,DM,R),
    {SD,NewHour} = diff_hour(R#rfc822_date.hour,(SH + DH),R),
    {SM,NewDate} = diff_date(R#rfc822_date.date,SD,R),
    {SY,NewMonth} = diff_month(R#rfc822_date.month,SM,R),
    NewYear = diff_year(R#rfc822_date.year,SY,R),
    {
     {NewYear,NewMonth,NewDate},
     {NewHour,NewMinute,R#rfc822_date.second}
    }.
diff_minute(M,0,_R)->{0,M};
diff_minute(M,Diff,_R) -> 
    NewMinute = M + Diff,
    if 
        NewMinute >= 60 -> {1,NewMinute - 60};
        NewMinute < 0 -> {-1,NewMinute + 60};
        true ->{0,NewMinute}
    end.
diff_hour(H,0,_R) -> {0,H};
diff_hour(H,Diff,_R) -> 
    NewHour = H + Diff,
    if 
        NewHour >= 24 -> {1,NewHour - 24};
        NewHour < 0 -> {-1,NewHour + 24};
        true ->{0,NewHour}
    end.
diff_date(D,0,_R)-> {0,D};
diff_date(D,Diff,R) -> 
    Max = dates_of_month(R),
    NewDate = D + Diff,
    if 
        NewDate > Max -> {1,NewDate - Max};
        NewDate < 1 -> {-1,NewDate + Max};
        true -> {0,NewDate}
    end.
dates_of_month(#rfc822_date{year = Year,month = Month} )->
    Default = proplists:get_value(Month,dates()),
    Leap = calendar:is_leap_year(Year),
    if
        (Default == 28) and (Leap == true) -> 29;
        true -> Default
    end.
diff_month(M,0,_R) ->{0,M};
diff_month(M,Diff,_R) ->
    NewMonth = M + Diff,
    if
        NewMonth > 12 -> {1,1};
        NewMonth < 1 -> {-1,12};
        true -> {0,NewMonth}
    end.

diff_year(Y,Diff,_R)-> Y + Diff.
     
                            
             

     
    
                                                              
            
            
