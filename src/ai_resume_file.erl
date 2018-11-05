-module(ai_resume_file).
%% resume file is a special case of blob
%% a file download from internet which can be used for http range transfer

-export([open/1]).

-export([file_length/1,received/1,etag/1,last_modified/1]).

-define(RANGE_SIZE_BYTES, 8).
-define(LENGTH_SIZE_BYTES, 8).
-define(ETAG_SIZE_BYTES, 2).
-define(LAST_MODIFIED_SIZE_BYTES, 2).

-record(ai_resume_file,{fd,etag,last_modified,received,length}).


open(Filenmae)->
    case ai_blob_file:open_for_read_write(Filenmae) of 
        {ok,Fd} -> try_recover(Fd);
        Error -> Error 
    end.
try_recover(Fd)->
    case ai_blob_file:read_extend_header(Fd) of 
        {ok,NewFd,Data} -> wrap_fd(NewFd,Data);
        {error,not_extend_blob} -> write_extend_header(Fd);
        Error ->
            ai_blob_file:close(Fd),
            Error 
    end.
write_extend_header(Fd)->
    Ext = <<0:64/big-unsigned-integer,0:64/big-unsigned-integer,0:16/big-unsigned-integer,0:16>>,
    case ai_blob_file:write_extend_header(Fd,Ext) of 
        ok -> 
            {ok,#ai_resume_file{fd = Fd, etag = undefined,last_modified = undefined,received = 0,length =0}};
        Error ->
            ai_blob_file:close(Fd),
            Error 
    end.
wrap_fd(Fd,<<Received:64/big-unsigned-integer,Length:64/big-unsigned-integer,
            EtagSize:16/big-unsigned-integer,LastModifiedSize:16/big-unsigned-integer,Rest/binary>>)->
    <<Etag:EtagSize/binary,LastModified:LastModifiedSize/binary,_Other/binary>> = Rest,
    {ok,BlobSize} = ai_blob_file:data_size(Fd),
    if 
        BlobSize > Received ->
            case ai_blob_file:truncate(Fd,Received) of 
                {ok,NewFd} ->
                      {ok,#ai_resume_file{fd = NewFd,etag = Etag,last_modified = LastModified,
                        received = Received,length = Length}};
                Error ->
                    ai_blob_file:close(Fd),
                    Error 
            end;
        true ->
            {ok,#ai_resume_file{fd = Fd,etag = Etag,last_modified = LastModified,
                        received = Received,length = Length}}
    end.

file_length(#ai_resume_file{length = Length}) -> {ok,Length}.
received(#ai_resume_file{received = Received}) -> {ok,Received}.
etag(#ai_resume_file{etag = Etag}) -> {ok,Etag}.
last_modified(#ai_resume_file{last_modified = LastModified}) -> {ok,LastModified}.