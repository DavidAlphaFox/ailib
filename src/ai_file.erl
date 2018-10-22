-module(ai_file).
-include_lib("kernel/include/file.hrl").

-export([files_in_dir/1,priv_dir/2]).
-export([remove_recursive/1,create_dir/1,create_priv_dir/2]).
-export([open_for_write/1,open_for_read/1,file_size/1]).
    
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
    
                
  
