-module(ai_file).
-include_lib("kernel/include/file.hrl").

-export([files_in_dir/1]).
-export([remove_recursive/1,create_dir/1,ensure_in_priv/2]).

ensure_in_priv(App,Dir)->
		PrivDir = code:priv_dir(App),
		filelib:ensure_dir(filename:join([PrivDir,Dir,"."])).

files_in_dir(Dir) ->
		{ok, SubFiles} = file:list_dir(Dir),
		[filename:join(Dir, SubFile) || SubFile <- SubFiles].

create_dir(Path)->
	filelib:ensure_dir(filename:join([Path, "."])).

remove_recursive(Path) ->
	case filelib:is_dir(Path) of
			false ->file:delete(Path);
			true ->
				lists:foreach(fun remove_recursive/1, files_in_dir(Path)),
				file:del_dir(Path)
		end.