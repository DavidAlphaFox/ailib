-module(ai_mnesia).
-export([ensure/1]).

ensure(TableCreateFun)->
  ok = application:ensure_started(mnesia),
	case is_virgin_node() of
		true ->
			case bootstrap(TableCreateFun) of
					{atomic,ok} ->	aborted_on_error((catch init_mnesia()));
					Res -> Res
			end;
		false ->
			aborted_on_error((catch init_mnesia()))
	end.

dir() -> mnesia:system_info(directory).
ensure_mnesia_dir() ->
	MnesiaDir = dir() ++ "/",
	case filelib:ensure_dir(MnesiaDir) of
		{error, Reason} ->throw({aborted, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
    ok -> ok
	end.

ensure_mnesia_schema()->
  ok = application:stop(mnesia),
  ok = mnesia:create_schema([node()]),
  ok = application:ensure_started(mnesia),
  ok.

is_virgin_node() ->
  case prim_file:list_dir(dir()) of
    {error, enoent} -> true;
    {ok, []} -> true;
    {ok, _} -> false
  end.

init_mnesia()->
  ok = application:ensure_started(mnesia),
  mnesia:wait_for_tables(mnesia:system_info(local_tables),infinity).

init_table(SchemaCreated,TableCreateFun)->
	case SchemaCreated of
		{atomic,ok}-> aborted_on_error((catch TableCreateFun()));
		Res -> Res
	end.

bootstrap(TableCreateFun)->
		MnesiaDirCreated = aborted_on_error((catch ensure_mnesia_dir())),
		case MnesiaDirCreated of
			{atomic,ok} ->
						SchemaCreated = aborted_on_error((catch ensure_mnesia_schema())),
						init_table(SchemaCreated,TableCreateFun);
			Res -> Res
		end.

aborted_on_error(Result)->
  case Result of
    {aborted, Reason}   -> {aborted, Reason};
    {'EXIT', Reason} -> {aborted, Reason};
    Res -> {atomic, Res}
  end.
