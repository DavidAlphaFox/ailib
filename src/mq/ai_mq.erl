-module(ai_mq).

-export([new/0,new/1,new/2,
         subscribe/2,subscribe/3,
         pull/2,pull/3,push/3,now/2
        ]).

-spec new()-> {ok,pid()}.
new()-> ai_mq_queue:new().

-spec new(Name :: atom() | list() | binary())-> {ok,pid()}.
new(Name)-> ai_mq_queue:new(Name).

-spec new(Name :: atom() | list() | binary(),MaxAge::integer())-> {ok,pid()}.
new(Name,MaxAge)-> ai_mq_queue:new(Name,MaxAge).

-spec subscribe(Queue::pid()|atom(),Channel::atom()|string()|binary())-> {ok,pid(),integer()}.
subscribe(Queue, Channel)->
  Timestamp = erlang:system_time(millisecond),
  ai_mq_queue:subscribe(Queue, Channel, Timestamp).
-spec subscribe(Queue::pid()|atom(),Channel::atom()|string()|binary(),
                Timestamp :: integer())-> {ok,pid(),integer()}.
subscribe(Queue,Channel,Timestamp) -> ai_mq_queue:subscribe(Queue, Channel, Timestamp).

-spec pull(Queue::pid()|atom(),Channel::term())-> {ok,pid(),integer(),list()}.
pull(Queue,Channel) ->
  Timestamp = erlang:system_time(millisecond),
  ai_mq_queue:pull(Queue, Channel, Timestamp).
-spec pull(Queue::pid()|atom(),Channel::atom()|string()|binary(),
           Timestamp :: integer())-> {ok,pid(),integer(),list()}.
pull(Queue,Channel,Timestamp) -> ai_mq_queue:pull(Queue, Channel, Timestamp).

-spec push(Queue::pid()|atom(),Channel::atom()|string()|binary(),
           Message::term())-> {ok,pid(),integer()}.
push(Queue,Channel,Message) -> ai_mq_queue:push(Queue,Channel,Message).

-spec now(Queue::pid()|atom(),Channel::atom()|string()|binary())->{ok,pid(),integer()}.
now(Queue,Channel) -> ai_mq_queue:now(Queue,Channel).
