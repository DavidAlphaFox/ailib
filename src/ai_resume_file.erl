-module(ai_resume_file).
%% resume file is a special case of blob
%% a file download from internet which can be used for http range transfer

-export([open/1,write/2,close/1,read/3,resume/4]).

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
        {error,not_extend_blob} ->  write_extend_header(Fd);
        Error ->
            ai_blob_file:close(Fd),
            Error 
    end.
write_extend_header(Fd)->
    Ext = <<0:64/big-unsigned-integer,0:64/big-unsigned-integer,
                    0:16/big-unsigned-integer,0:16/big-unsigned-integer>>,
    case ai_blob_file:write_extend_header(Fd,Ext) of 
        {ok,NewFd} -> 
            {ok,#ai_resume_file{fd = NewFd, etag = undefined,last_modified = undefined,received = 0,length =0}};
        Error ->
            ai_blob_file:close(Fd),
            Error 
    end.
wrap_fd(Fd,<<Received:64/big-unsigned-integer,Length:64/big-unsigned-integer,
            EtagSize:16/big-unsigned-integer,LastModifiedSize:16/big-unsigned-integer,Rest/binary>>)->
    <<Etag:EtagSize/binary,LastModified:LastModifiedSize/binary,_Other/binary>> = Rest,
    {ok,BlobSize} = ai_blob_file:data_size(Fd),
    WarpFd = #ai_resume_file{fd = Fd,etag = Etag,last_modified = LastModified,
                        received = Received,length = Length},
    if 
        BlobSize > Received -> truncate(WarpFd,Received);
        true -> {ok,WarpFd}
    end.

file_length(#ai_resume_file{length = Length}) -> {ok,Length}.
received(#ai_resume_file{received = Received}) -> {ok,Received}.
etag(#ai_resume_file{etag = Etag}) -> {ok,Etag}.
last_modified(#ai_resume_file{last_modified = LastModified}) -> {ok,LastModified}.

etag_size(undefined) -> 0;
etag_size(Etag) -> erlang:byte_size(Etag).
last_modified_size(undefined) -> 0;
last_modified_size(LastModified) -> erlang:byte_size(LastModified).

persist_header(#ai_resume_file{fd = Fd,etag = Etag,last_modified = LastModified,
                received = Received,length = Length} = Ref)->
    EtagSize = etag_size(Etag),
    LastModifiedSize = last_modified_size(LastModified),
    Ext = <<Received:64/big-unsigned-integer,
            Length:64/big-unsigned-integer,
            EtagSize:16/big-unsigned-integer,
            LastModifiedSize:16/big-unsigned-integer>>,
    Ext1 = if 
                EtagSize > 0 -> <<Ext/binary,Etag/binary>>;
                true -> Ext
            end,
    Ext2 = if 
                LastModifiedSize > 0 -> <<Ext1/binary,LastModified/binary>>;
                true -> Ext1
            end,
    case ai_blob_file:write_extend_header(Fd,Ext2) of 
        {ok,NewFd} -> {ok,Ref#ai_resume_file{fd = NewFd}};
        Error -> Error 
    end.


write(#ai_resume_file{fd = Fd,received = Received} = Ref,Data)->
    case ai_blob_file:write(Fd,Data) of 
        {ok,NewFd,Size}-> 
            case persist_header(Ref#ai_resume_file{fd = NewFd,received = Received + Size}) of 
                {ok,NewRef} -> {ok,NewRef,Size};
                Error -> Error 
            end;
        Error -> Error
    end.
close(#ai_resume_file{fd = Fd} = Ref)->
    case ai_blob_file:close(Fd) of 
        {ok,NewFd,Digest} -> {ok,Ref#ai_resume_file{fd = NewFd},Digest};
        Error -> Error 
    end. 
read(#ai_resume_file{fd = Fd,received = Received} = Ref, Offset, Size)->
    Need = Offset + Size,
    if 
        Need > Received -> {error,overflow};
        true ->
            case ai_blob_file:read(Fd,Offset,Size) of 
                {ok,NewFd,Data} -> {ok,Ref#ai_resume_file{fd = NewFd},Data};
                Error -> Error 
            end 
    end.

truncate(#ai_resume_file{fd = Fd} = Ref,Received)->
    case ai_blob_file:truncate(Fd,Received) of 
        {ok,NewFd} ->
            {ok,Ref#ai_resume_file{fd = NewFd,received = Received}};
        Error ->
            ai_blob_file:close(Fd),
            Error 
    end.
clean_all(Ref,Length,true)->
    case persist_header(Ref#ai_resume_file{received = 0,length = Length}) of 
        {ok,NewRef} -> truncate(NewRef,0);
        Error -> Error
    end;
clean_all(Ref,Length,false)-> {ok,Ref#ai_resume_file{length = Length}}.

resume(Ref,Etag,LastModified,Length)->
    case {Etag,LastModified} of 
        {undefined,undefined} -> clean_all(Ref,Length,true);
        {undefined,_} ->
            R = Ref#ai_resume_file.last_modified == LastModified,
            clean_all(Ref,Length,R);
        {_,undefined}->
            R = Ref#ai_resume_file.etag == Etag,
            clean_all(Ref,Length,R);
        {_,_}->
            R = (Ref#ai_resume_file.etag == Etag) and (Ref#ai_resume_file.last_modified == LastModified),
            clean_all(Ref,Length,R)
    end. 