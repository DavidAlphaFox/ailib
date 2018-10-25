-module(ai_file).
-include_lib("kernel/include/file.hrl").

-export([files_in_dir/1,priv_dir/2]).
-export([remove_recursive/1,create_dir/1,create_priv_dir/2]).
-export([open_for_write/1,open_for_read/1,open_for_append/1,file_size/1]).
-export([hash_to_path/2,hash_to_fullname/2]).


files_in_dir(Dir) ->
		{ok, SubFiles} = file:list_dir(Dir),
		[filename:join(Dir, SubFile) || SubFile <- SubFiles].

priv_dir(App,Dir)->
    PrivDir = code:priv_dir(App),
	filename:join([PrivDir,Dir]).


create_priv_dir(App,Dir)->
        TheDir = priv_dir(App,Dir),
		filelib:ensure_dir(filename:join([TheDir,"."])).

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
    filelib:ensure_dir(Filename), %% 每次都是新建，如果存在的时候就报错
    case file:open(Filename, [exclusive, write, binary, raw]) of
		{ok, Fd} -> {ok,Fd};
		Error -> Error
	end.			
open_for_append(Filename)->
    filelib:ensure_dir(Filename),
    case file:open(Filename,[append,binary,raw]) of 
        {ok, Fd} -> {ok,Fd};
		Error -> Error
	end.			
open_for_read(Filename) ->
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} -> {ok,Fd};
        Error -> Error
    end.

file_size(Filename)->
    case file:read_file_info(Filename) of 
        {ok, FileInfo} -> {ok,FileInfo#file_info.size};
        Error -> Error
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
