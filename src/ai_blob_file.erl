-module(ai_blob_file).

-export([open_for_write/1,write/2,close/1]).
-export([open_for_read/1,open_for_read/2,read/2]).
-export([file_data_range/1,digest/1]).

-define(MAGIC_NUMBER, <<16#7334:16>>).
-define(MAGIC_NUMBER_SIZE_BYTES, 2).
-define(VERSION_NUMBER,<<16#1:16>>).
-define(VERSION_NUMBER_SIZE_BYTES,2).
-define(CHECKSUM_SIZE_BYTES, 20).
-define(FILESIZE_SIZE_BYTES, 8).
-define(TOTAL_HEADER_SIZE_BYTES, 4 * 1024).
-define(BLOCK_SIZE_BYTES,64 * 1024).

-record(ai_blob_file,{fd,filename,size,ctx,mode}).

blob_init(Fd)->
    case file:write(Fd, ?MAGIC_NUMBER) of
        ok -> file:write(Fd,?VERSION_NUMBER);
        Error ->  Error
    end.

checksum(Fd,Ctx)->
    case file:read(Fd,?BLOCK_SIZE_BYTES) of 
        {ok,Data} -> checksum(Fd, crypto:hash_update(Ctx, Data));
        eof -> {ok,crypto:hash_final(Ctx)};
        Error -> Error
    end.
checksum_consistency(Fd,Ctx,Digest)->
    case checksum(Fd,Ctx) of 
        {ok,Digest} ->
            {ok,Fd,Digest};
        {error,Reason} -> {error,Reason};
        _AnyDigest -> {error,corrupt_blob_file}
    end.

is_blob_broken(Fd)->
    {ok, _Any} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES}),
    case file:read(?CHECKSUM_SIZE_BYTES) of
        {ok,Digest} -> 
            {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
            checksum_consistency(Fd,crypto:hash_init(sha),Digest);
        Error -> Error
    end.

is_blob_file(Fd)->
    case file:read(Fd, ?MAGIC_NUMBER_SIZE_BYTES) of
        {ok,?MAGIC_NUMBER} -> {ok,Fd};
        {error,Reason}  -> {error,Reason};
        _AnyData -> {error, not_blob_file}
    end.
blob_fd(Fd)->
    {ok, _Any} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES}),
    case  file:read(?CHECKSUM_SIZE_BYTES) of 
        {ok,Digest} -> blob_fd(Fd,Digest);
        Error -> Error 
    end.
blob_fd(Fd,Digest)->
    {ok, _Any} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES + ?CHECKSUM_SIZE_BYTES}),
    case file:read(Fd,?FILESIZE_SIZE_BYTES) of 
        {ok,SizeBinary} ->
            <<Size:64/big-unsigned-integer>> = SizeBinary,
            {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
            {ok,#ai_blob_file{fd = Fd,size = Size,ctx = Digest, mode = read}};
        Error -> Error 
    end.

blob_check(Fd)->
    case is_blob_file(Fd) of 
        {ok,Fd} -> 
            case is_blob_broken(Fd) of 
                {ok,Fd,Digest} -> blob_fd(Fd,Digest);
                Error -> Error 
            end;
        Error -> Error
    end.          
open_for_write(Filename) ->
    case ai_file:open_for_write(Filename) of
        {ok, Fd} ->
            case blob_init(Fd) of
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
            case file:write(Fd, Data) of
                ok -> {ok, Ref#ai_blob_file{ size = Size+ DataSize,ctx = crypto:hash_update(Ctx, Data)}};
                Error -> Error
            end;
        _ -> {error,not_writable_blob}
    end.
close(#ai_blob_file{fd=Fd,size = Size,ctx = Ctx} = Ref)->
    case file:sync(Fd) of
        ok ->
            Digest = crypto:hash_final(Ctx),
            {ok, _Any} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES + ?VERSION_NUMBER_SIZE_BYTES}),
            file:write(Fd, Digest),
            file:write(Fd,<<Size:64/big-unsigned-integer>>),
            file:close(Fd),
            {ok,Ref#ai_blob_file{ctx = Digest,mode = close},Digest};
        Error ->
            file:close(Fd),
            Error
    end.
open_for_read(Filename)->
    open_for_read(Filename,true).

open_for_read(Filename,CheckConsistency)->
    Opened = ai_file:open_for_read(Filename),
    case {CheckConsistency,Opened} of 
        {true,{ok,Fd}} ->
            case blob_check(Fd) of 
                {ok,BlobFd} -> {ok,BlobFd};
                Error -> 
                    file:close(Fd),
                    Error
            end;
        {flase,{ok,Fd}} ->
            case blob_fd(Fd) of 
                {ok,BlobFd} -> {ok,BlobFd};
                Error ->
                    file:close(Fd),
                    Error
            end;
        {_Any,Error}-> Error 
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

file_data_range(#ai_blob_file{size = Size})-> {?TOTAL_HEADER_SIZE_BYTES,Size}.
digest(#ai_blob_file{ctx = Digest,mode = Mode}) ->
    case Mode of 
        write -> {error,not_hava_digest};
        _ -> {ok,Digest}
    end.