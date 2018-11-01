-module(ai_blob_file).

-export([open_for_write/1,write/2,close/1]).
-export([open_for_read/1,open_for_read/2,read/2]).
-export([data_range/1,digest/1,data_size/1]).

-define(MAGIC_NUMBER, <<16#61697334:32/big-unsigned-integer>>).
-define(MAGIC_NUMBER_SIZE_BYTES, 4).
-define(VERSION_NUMBER,<<16#1:32/big-unsigned-integer>>).
-define(VERSION_NUMBER_SIZE_BYTES,4).
-define(CHECKSUM_SIZE_BYTES, 20).
-define(FILESIZE_SIZE_BYTES, 8).
-define(TOTAL_HEADER_SIZE_BYTES, 4 * 1024).
-define(BLOCK_SIZE_BYTES,64 * 1024).

-record(ai_blob_file,{fd,filename,size,ctx,mode}).
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

calculate_checksum(Fd,Ctx)->
    case file:read(Fd,?BLOCK_SIZE_BYTES) of 
        {ok,Data} -> calculate_checksum(Fd, crypto:hash_update(Ctx, Data));
        eof -> {ok,crypto:hash_final(Ctx)};
        Error -> Error
    end.
check_consistency(Fd,Ctx,Digest)->
    case calculate_checksum(Fd,Ctx) of 
        {ok,Digest} -> {ok,Fd,Digest};
        {error,Reason} -> {error,Reason};
        _AnyDigest -> {error,corrupt_blob_file}
    end.

check_broken(Fd)->
    Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES,
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
    Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES,
    case internal_read(Fd,Pos,?CHECKSUM_SIZE_BYTES) of 
        {ok,Digest} -> warp_fd(Fd,Digest);
        Error -> Error 
    end.
warp_fd(Fd,Digest)->
    Pos = ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES + ?CHECKSUM_SIZE_BYTES,
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
    case Mode of 
        write->
            DataSize = erlang:byte_size(Data),
            case internal_append(Fd,Data) of
                ok -> {ok, Ref#ai_blob_file{ size = Size + DataSize,ctx = crypto:hash_update(Ctx, Data)}};
                {error,Reason} -> {error,Reason}
            end;
        _ -> {error,not_writable_blob}
    end.
close(#ai_blob_file{fd = Fd,size = Size,ctx = Ctx,mode = write} = Ref)->
    case file:sync(Fd) of
        ok ->
            Digest = crypto:hash_final(Ctx),
            Data = <<Digest/binary,Size:64/big-unsigned-integer>>,
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
    end;
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
        {true,{ok,Fd}} -> with_check(Fd,true);
        {false,{ok,Fd}} -> with_check(Fd,false);
        {_Any,{error,Reason}}-> {error,Reason} 
    end.
with_check(Fd,true)->
    case check_file(Fd) of 
        {ok,BlobFd} -> {ok,BlobFd};
        Error -> 
            file:close(Fd),
            Error
    end;
with_check(Fd,false)->
    case warp_fd(Fd) of 
        {ok,BlobFd} -> {ok,BlobFd};
        Error ->
            file:close(Fd),
            Error
    end.
read(#ai_blob_file{fd = Fd,mode = Mode} = Ref, Size) ->
    case Mode of 
        read ->
            case file:read(Fd, Size) of
                {ok,Data} -> {ok,Ref,Data};
                Error -> Error
            end;
        write -> {error,not_readable_blob}
    end.
data_size(#ai_blob_file{size = Size})-> Size;
data_size(Filename)->
    case ai_file:file_size(Filename) of 
        {ok,Size} -> Size;
        Error -> Error 
    end.
data_range(#ai_blob_file{size = Size})-> {?TOTAL_HEADER_SIZE_BYTES,Size};
data_range(Filename) ->
    case ai_file:file_size(Filename) of 
        {ok,Size} -> {?TOTAL_HEADER_SIZE_BYTES,Size - ?TOTAL_HEADER_SIZE_BYTES};
        Error -> Error 
    end.
digest(#ai_blob_file{ctx = Digest,mode = Mode}) ->
    case Mode of 
        write -> {error,not_hava_digest};
        _ -> {ok,Digest}
    end;
digest(_Any) -> {error,not_hava_digest}.