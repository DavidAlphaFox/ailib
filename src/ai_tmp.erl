-module(ai_tmp).
-include_lib("kernel/include/file.hrl").

-export([dir/0]).
-export([name/1,name/2]).
-export([run_with_tmp/3]).

-spec dir() -> string() | false.
dir() ->
	Keys = ["TMPDIR","TMP","TMP"],
	case lists:search(fun(I)-> found_in_env(with_env(I)) end,Keys) of
		{value,Key}-> with_env(Key);
		false -> try_default_tmp()
	end.

run_with_tmp(Name,Options,MFA)->
    Dir = name(Name,Options),
    case ai_file:create_dir(Dir) of
        ok ->
            try
                ai_function:run_mfa(MFA,[Dir])
            after
                case proplists:get_value(clean,Options,false) of
                    true -> ai_file:remove_recursive(Dir);
                    _ -> ok
                end
            end;
        E -> E
    end.
        
name(Name)->
    name(Name,[]).
name(Name,Options)->
    Options1 = maps:from_list(Options),
    Suffix = case maps:get(suffix,Options1,undefined) of
                 undefined -> "";
                 [] -> "";
                 [$.] -> "";
                 [$.|_] = Suffix1 -> Suffix1;
                 Suffix2 -> [$.|Suffix2]                      
             end,
    Prefix = case maps:get(prefix,Options1,undefined) of
                 undefined -> "";
                 [] -> "";
                 Prefix1 -> 
                     case {erlang:length(Prefix1),lists:last(Prefix1)} of
                         {1,$_} -> "";
                         {N,$_} when N > 1 -> Prefix1;
                         {N,_Any} when N >= 1 -> Prefix1 ++ [$_];
                         _ -> ""
                     end                     
             end,
		Path = maps:get(path, Options1,dir()),
    TmpName = lists:flatten([Prefix,Name,"-",ai_ascii_random:rand(20),Suffix]),
		filename:join([Path, TmpName]).



with_env(Key)->
	case os:getenv(Key) of
			false -> false;
			Tmp -> Tmp
	end.

found_in_env(false) -> false;
found_in_env(_Tmp) -> true.


try_default_tmp() ->
	case is_dir_writable("/tmp") of
		false -> try_cwd();
		Path	-> Path
	end.

try_cwd()->
	Cwd = case file:get_cwd() of
					{ok, Dir} -> Dir;
					_ -> "."
				end,
	case is_dir_writable(Cwd) of
		false -> false;
		LTmp -> LTmp
	end.

is_writable(read_write)-> true;
is_writable(write) -> true; 
is_writable(_) ->  false.


is_dir_writable(Path) ->
		case file:read_file_info(Path) of
				{ok, #file_info{type = directory, access = Access}} ->
            case is_writable(Access) of
                true -> Path;
                fasle -> false
            end;
				_ -> false
		end.

