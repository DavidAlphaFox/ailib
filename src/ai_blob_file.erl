-module(ai_blob_file).

-export([open_for_write/1,write/2,close/1]).
-export([open_for_read/1,open_for_read/2,read/3]).
-export([data_range/1,digest/1,data_size/1]).
-export([read_extend_header/1,write_extend_header/2]).
-export([open_for_read_write/1]).

-define(MAGIC_NUMBER, <<16#7334:16/big-unsigned-integer>>).
-define(MAGIC_NUMBER_SIZE_BYTES, 2).

-define(VERSION_NUMBER, <<16#2:16/big-unsigned-integer>>).
-define(VERSION_NUMBER_SIZE_BYTES,2).

-define(FILESIZE_SIZE_BYTES, 8).
-define(CHECKSUM_SIZE_BYTES, 20).

-define(EXTEND_BIT_SIZE_BYTES, 1).
-define(EXTEND_HEADER_SIZE_BYTES, 1).
-define(EXTEND_CHECKSUM_SIZE_BYTES, 20).

-define(EXTEND_HEADER_MAX_SIZE,255).

-define(EXTEND_HEADER_OFFSET,
    ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES + ?CHECKSUM_SIZE_BYTES + ?FILESIZE_SIZE_BYTES).

-define(TOTAL_HEADER_SIZE_BYTES, 1024).
-define(BLOCK_SIZE_BYTES,64 * 1024).

-record(ai_blob_file,{fd,filename,size,ctx,mode}).

%% blob file is append only file
%% blob file can be reopen
%% blob file can be opened readonly,writeonly and readwrite.
%% blob file support an extend header which less then 255 bytes.


bof_seek(Fd,Pos)-> file:position(Fd,{bof,Pos}).
seek_end(Fd) -> file:position(Fd,{eof,0}).
seek_read(Fd,Pos,Size,{ok,Pos})-> file:read(Fd,Size);
seek_read(_Fd,_Pos,_Size,Error) -> Error.
seek_write(Fd,Data,{ok,_Pos}) -> file:write(Fd,Data);
seek_write(_Fd,_Data,Error) -> Error.
seek_write(Fd,Pos,Data,{ok,Pos}) -> file:write(Fd,Data);
seek_write(_Fd,_Pos,_Data,Error) -> Error.

internal_read(Fd,Pos,Size)->
    List = [
        {fun bof_seek/2,[Fd]},
        {fun seek_read/4,[Fd,Pos,Size]}
    ],
    ai_lists:run_pipe(List,[Pos]).
internal_write(Fd,Pos,Data)->
    List = [
        {fun bof_seek/2,[Fd]},
        {fun seek_write/4,[Fd,Pos,Data]}
    ],
    ai_lists:run_pipe(List,[Pos]).
internal_append(Fd,Data)->
    List = [
        {fun seek_end/1,[Fd]},
        {fun seek_write/3,[Fd,Data]}
    ],
    ai_lists:run_pipe(List).
write_header(Fd)->
    FillSize = 
        (?TOTAL_HEADER_SIZE_BYTES - ?MAGIC_NUMBER_SIZE_BYTES - ?VERSION_NUMBER_SIZE_BYTES)*8,
    Data = <<?MAGIC_NUMBER/binary,?VERSION_NUMBER/binary,0:FillSize/integer>>,
    internal_write(Fd,0,Data).

calculate_checksum(Fd,Ctx,Final)->
    case file:read(Fd,?BLOCK_SIZE_BYTES) of 
        {ok,Data} -> calculate_checksum(Fd, crypto:hash_update(Ctx, Data),Final);
        eof ->
            if 
                Final == true -> {ok,crypto:hash_final(Ctx)};
                true -> {ok,Ctx}
            end;
        Error -> Error
    end.
check_consistency(Fd,Ctx,Digest)->
    case calculate_checksum(Fd,Ctx,true) of 
        {ok,Digest} -> {ok,Fd,Digest};
        {error,Reason} -> {error,Reason};
        _AnyDigest -> {error,corrupt_blob_file}
    end.

check_broken(Fd)->
    Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES + ?FILESIZE_SIZE_BYTES,
    case internal_read(Fd,Pos,?CHECKSUM_SIZE_BYTES) of
        {ok,Digest} -> 
            case file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}) of 
                {ok, ?TOTAL_HEADER_SIZE_BYTES} -> check_consistency(Fd,crypto:hash_init(sha),Digest);
                {error,Reason} -> {error,Reason}
            end;
        {error,Reason} -> {error,Reason}
    end.

check_magic(Fd)->
    case file:read(Fd, ?MAGIC_NUMBER_SIZE_BYTES) of
        {ok,?MAGIC_NUMBER} -> {ok,Fd};
        {error,Reason}  -> {error,Reason};
        _AnyData -> {error, not_blob_file}
    end.
warp_fd(Fd)->
    Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES + ?FILESIZE_SIZE_BYTES,
    case internal_read(Fd,Pos,?CHECKSUM_SIZE_BYTES) of 
        {ok,Digest} -> warp_fd(Fd,Digest);
        Error -> Error 
    end.
warp_fd(Fd,Digest)->
    Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES,
    case internal_read(Fd,Pos,?FILESIZE_SIZE_BYTES) of 
        {ok,SizeBinary} ->
            <<Size:64/big-unsigned-integer>> = SizeBinary,
            case file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}) of 
                {ok, ?TOTAL_HEADER_SIZE_BYTES} -> {ok,#ai_blob_file{fd = Fd,size = Size,ctx = Digest, mode = read}};
                {error,Reason} -> {error,Reason}
            end;
        {error,Reason} -> {error,Reason} 
    end.

check_file(Fd)->
    case check_magic(Fd) of 
        {ok,Fd} -> 
            case check_broken(Fd) of 
                {ok,Fd,Digest} -> warp_fd(Fd,Digest);
                Error -> Error 
            end;
        Error -> Error
    end.          
open_for_write(Filename) ->
    case ai_file:open_for_write(Filename) of
        {ok, Fd} ->
            case write_header(Fd) of
                ok ->
                    {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
                    {ok, #ai_blob_file{fd = Fd, size = 0,ctx = crypto:hash_init(sha),mode = write}};
                Error ->
                    file:close(Fd),
                    Error
            end;
        Error -> Error
    end.
write(#ai_blob_file{fd=Fd, size = Size, ctx=Ctx,mode = Mode}=Ref, Data) when is_binary(Data) ->
    WirteTask = fun()->
            DataSize = erlang:byte_size(Data),
            case internal_append(Fd,Data) of
                ok -> {ok, Ref#ai_blob_file{ size = Size + DataSize,ctx = crypto:hash_update(Ctx, Data)}};
                {error,Reason} -> {error,Reason}
            end
        end,
    case Mode of 
        write-> WirteTask();
        read_write-> WirteTask();
        _ -> {error,not_writable_blob}
    end.
write_extend_header(#ai_blob_file{mode = write} = Ref,Data)->
    Size  = erlang:byte_size(Data),
    if 
        Size > ?EXTEND_HEADER_MAX_SIZE -> {error,overflow};
        true -> write_extend_header(Ref,Data,Size)
    end;
write_extend_header(#ai_blob_file{mode = read_write} = Ref,Data)->
    Size  = erlang:byte_size(Data),
    if 
        Size > ?EXTEND_HEADER_MAX_SIZE -> {error,overflow};
        true -> write_extend_header(Ref,Data,Size)
    end;
write_extend_header(#ai_blob_file{mode = _} ,_Data)->
    {error,not_writable_blob}.

write_extend_header(#ai_blob_file{fd = Fd} = Ref,Data,Size)->
    Digest = crypto:hash(sha,Data),
    Ext = <<1:8/big-unsigned-integer,Size:8/big-unsigned-integer,Digest/binary,Data/binary>>,
    case internal_write(Fd,?EXTEND_HEADER_OFFSET,Ext) of 
        ok -> {ok,Ref};
        Error -> Error 
    end.
writable_close(#ai_blob_file{fd = Fd,size = Size,ctx = Ctx} = Ref)->
    case file:sync(Fd) of   
        ok ->
            Digest = crypto:hash_final(Ctx),
            Data = <<Size:64/big-unsigned-integer,Digest/binary>>,
            Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES,
            case internal_write(Fd,Pos,Data) of 
                ok -> 
                    case file:close(Fd) of 
                        ok -> {ok,Ref#ai_blob_file{fd = undefined, ctx = Digest,mode = close},Digest};
                        Error -> Error 
                    end;
                Error -> Error 
            end;
        Error ->
            file:close(Fd),
            Error
    end.
close(#ai_blob_file{mode = write} = Ref)->
    writable_close(Ref);
close(#ai_blob_file{mode = read_write} = Ref)->
    writable_close(Ref);
close(#ai_blob_file{fd=Fd,ctx = Ctx,mode = read} = Ref)->
    case file:close(Fd) of 
        ok -> {ok,Ref#ai_blob_file{fd = undefined,mode = close},Ctx};
        Error -> Error 
    end;
close(#ai_blob_file{ctx = Ctx} = Ref)-> {ok,Ref,Ctx}.

open_for_read(Filename)->
    open_for_read(Filename,true).

open_for_read(Filename,CheckConsistency)->
    Opened = ai_file:open_for_read(Filename),
    case {CheckConsistency,Opened} of 
        {true,{ok,Fd}} -> with_check(Filename,Fd,true);
        {false,{ok,Fd}} -> with_check(Filename,Fd,false);
        {_Any,{error,Reason}}-> {error,Reason} 
    end.
with_check(Filename,Fd,true)->
    case data_size(Filename) of 
        {ok,Size} ->
            case check_file(Fd) of 
                {ok,BlobFd} -> {ok,BlobFd#ai_blob_file{size = Size}};
                Error -> 
                    file:close(Fd),
                    Error
            end;
        Error ->
            file:close(Fd),
            Error 
    end;
with_check(Filename,Fd,false)->
    case data_size(Filename) of 
        {ok,Size} ->
            case warp_fd(Fd) of 
                {ok,BlobFd} -> {ok,BlobFd#ai_blob_file{size = Size}};
                Error ->
                    file:close(Fd),
                    Error
            end;
        Error ->
            file:close(Fd),
            Error
    end.
read_extend_header(_Ref,<<0:8/big-unsigned-integer,_Rest/binary>>)-> {error,not_extend_blob};
read_extend_header(#ai_blob_file{fd = Fd} = Ref,<<1:8/big-unsigned-integer,
        Size:8/big-unsigned-integer,Digest/binary>>)->
    Pos = ?EXTEND_HEADER_OFFSET + 22,
    case internal_read(Fd,Pos,Size) of 
        {ok,Data} -> 
            case  crypto:hash(sha,Data) of 
                Digest -> {ok,Ref,Data};
                _ -> {error,corrupt_extend_header}
            end;
        Error -> Error
    end.  
read_extend_header({fd = Fd} = Ref)->
    case internal_read(Fd,?EXTEND_HEADER_OFFSET,22) of 
        {ok,Data}-> read_extend_header(Ref,Data);
        Error -> Error
    end.

read(#ai_blob_file{fd = Fd,mode = Mode} = Ref,Offset, Size) ->
    ReadTask = fun() ->
            Pos = Offset + ?TOTAL_HEADER_SIZE_BYTES,
            case internal_read(Fd,Pos,Size) of
                {ok,Data} -> {ok,Ref,Data};
                Error -> Error
            end
        end,
    case Mode of 
        read -> ReadTask();
        read_write -> ReadTask();
        write -> {error,not_readable_blob}
    end.

open_for_read_write(Filename)->
    Opened = ai_file:open_for_read_write(Filename),
    case Opened of 
        {ok,Fd} ->
            case with_check(Filename,Fd,false) of 
                {ok,BlobFd} -> update_checksum(BlobFd#ai_blob_file{mode = read_write});
                Error -> Error 
            end;
        Error ->  Error
    end.
update_checksum(#ai_blob_file{fd = Fd} = Ref)->
   case calculate_checksum(Fd,crypto:hash_init(sha),false) of 
       {ok,Ctx}-> {ok,Ref#ai_blob_file{ctx = Ctx}};
       Error ->
           file:close(Fd),
           Error 
    end.
data_size(#ai_blob_file{size = Size})-> {ok,Size};
data_size(Filename)->
    case ai_file:file_size(Filename) of 
        {ok,Size} -> 
            if 
                Size >= ?TOTAL_HEADER_SIZE_BYTES -> {ok,Size - ?TOTAL_HEADER_SIZE_BYTES};
                true -> {error,corrupt_blob_file}
            end;
        Error -> Error 
    end.
data_range(#ai_blob_file{size = Size})-> {?TOTAL_HEADER_SIZE_BYTES,Size};
data_range(Filename) ->
    case ai_file:file_size(Filename) of 
        {ok,Size} ->
            if 
                Size >= ?TOTAL_HEADER_SIZE_BYTES ->  {?TOTAL_HEADER_SIZE_BYTES,Size - ?TOTAL_HEADER_SIZE_BYTES};
                true ->  {error,corrupt_blob_file}
            end;
        Error -> Error 
    end.
digest(#ai_blob_file{ctx = Digest,mode = Mode}) ->
    case Mode of 
        write -> {error,not_hava_digest};
        read_write -> {error,not_hava_digest};
        _ -> {ok,Digest}
    end;
digest(_Any) -> {error,not_hava_digest}.