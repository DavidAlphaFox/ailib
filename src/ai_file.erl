-module(ai_file).
-include_lib("kernel/include/file.hrl").

-export([files_in_dir/1,priv_dir/2]).
-export([remove_recursive/1,create_dir/1,create_priv_dir/2]).
-export([open_for_write/1,open_for_read/1,file_size/1]).
-export([hash_to_path/2,hash_to_fullname/2]).


files_in_dir(Dir) ->
		{ok, SubFiles} = file:list_dir(Dir),
		[filename:join(Dir, SubFile) || SubFile <- SubFiles].

priv_dir(App,Dir)->
    PrivDir = code:priv_dir(App),
		filename:join([PrivDir,Dir,"."]).


create_priv_dir(App,Dir)->
		PrivDir = code:priv_dir(App),
		filelib:ensure_dir(filename:join([PrivDir,Dir,"."])).

create_dir(Path)->
    filelib:ensure_dir(filename:join([Path, "."])).

remove_recursive(Path) ->
	case filelib:is_dir(Path) of
			false ->file:delete(Path);
			true ->
				lists:foreach(fun remove_recursive/1, files_in_dir(Path)),
				file:del_dir(Path)
		end.
open_for_write(Filename) ->
    filelib:ensure_dir(Filename),
    case file:open(Filename, [exclusive, write, binary, raw]) of
		{ok, Fd} -> {ok,Fd};
		Error -> Error
	end.			
open_for_read(Filename) ->
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} -> {ok,Fd};
        Error -> Error
    end.
file_size(Filename)->
    case open_for_read(Filename) of
        {ok,Fd}->
            case file:position(Fd,eof) of
                {ok,Size} ->
                    file:close(Fd),
                    {ok,Size};
                Error -> Error
              end;
         Error-> Error
    end.
hash_to_path(Level,HashString) when erlang:is_binary(HashString)->
    hash_to_path(Level,erlang:binary_to_list(HashString));
hash_to_path(Level,HashString)->
    {Path,_Rest} = hash_to_path(Level,HashString,[]),
    filename:join(Path).
hash_to_fullname(Level,HashString) when erlang:is_binary(HashString)->
    hash_to_fullname(Level,erlang:binary_to_list(HashString));
hash_to_fullname(Level,HashString)->
    {Path,Rest} = hash_to_path(Level,HashString,[]),
    HashPath = filename:join(Path),
    filename:join([HashPath,Rest]).

hash_to_path(0,Rest,Acc)->
    {lists:reverse(Acc),Rest};
hash_to_path(_N,[],_Acc)->
    erlang:throw({error,file_path_too_deep});
hash_to_path(N,[H1,H2|T],Acc)->
    hash_to_path(N-1,T,[[H1,H2] | Acc]).    
