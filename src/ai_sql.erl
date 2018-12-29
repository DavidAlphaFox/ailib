-module(ai_sql).

-export([select/2,insert/3,update/3,delete/1,where/2]).
-export([build/1]).

%-type field_name() :: atom().

%-type field_value() :: term().
%-type operator() :: '<' | '>' | '==' | '=<' | '>=' | '/=' | 'like' .
%-type condition() :: {'and', [condition()]} | {'or', [condition()]}
%                   | {'not', condition()} | {field_name(),field_value()}
%                   | {operator(),field_name(),field_value()}
%                   | {operator(),field_name(),field_name()}.

%-type conditions() :: condition() | [condition()].

-record(ai_sql,{
    op = undefined,
    table = undefined,
    where = undefined,
    fields = undefined,
    values = undefined
}).

%% from -> where -> group -> having -> select -> order by -> limit 


select(Table,Fields) -> #ai_sql{op = select,table =Table, fields  = Fields}.
insert(Table,Fields,undefined) -> #ai_sql{op = insert, table = Table,fields = Fields,values = []};
insert(Table,Fields,Values) when erlang:is_list(Values)-> #ai_sql{op = insert, table = Table,fields = Fields,values = Values};
insert(_Table,_Fields,Values)-> throw({unsupported,value,Values}).
update(Table,Fields,undefined) -> #ai_sql{op = update, table = Table,fields = Fields,values = []};
update(Table,Fields,Values) when erlang:is_list(Values) -> #ai_sql{op = update, table = Table,fields = Fields,values = Values};
update(_Table,_Fields,Values) -> throw({unsupported,value,Values}).
delete(Table) -> #ai_sql{op = delete, table = Table}.

where(SQL,Conditions)-> SQL#ai_sql{where = Conditions}.

build(SQL)->
    build(SQL#ai_sql.op,SQL).
build(select,SQL)->
    {Hodler,OPClause} = op_clause(select,SQL,1),
    build(where,SQL,Hodler,OPClause,[]);
build(update,SQL)->
    {Hodler,OPClause} = op_clause(update,SQL,1),
    build(where,SQL,Hodler,OPClause,SQL#ai_sql.values);
build(delete,SQL)->
    {Hodler,OPClause} = op_clause(delete,SQL,1),
    build(where,SQL,Hodler,OPClause,SQL#ai_sql.values);
build(insert,SQL)->
    {_Hodler,OPClause} = op_clause(insert,SQL,1),
    {OPClause,SQL#ai_sql.values}.
build(where,SQL,Hodler,Acc,Values)->
    case SQL#ai_sql.where of 
        undefined -> {Acc,Values};
        Conditions ->
            {WhereValues, CleanConditions,_Hodler0} = prepare_where(Conditions,Hodler),
            WhereClause = where_clause(CleanConditions),
            Query = <<Acc/binary," WHERE ", WhereClause/binary>>,
            {Query,Values ++ WhereValues}
    end.

op_clause(select,SQL,Holder)->
    
    S = 
        case SQL#ai_sql.fields of 
            [_F] ->  escape_field(SQL#ai_sql.fields);
            Fields when erlang:is_list(Fields) ->
                SelectFields = [escape_field(F) || F <- Fields],
                ai_string:join(SelectFields,<<" , ">>);
            _ -> SQL#ai_sql.fields
        end,
    Table = ai_string:to_string(SQL#ai_sql.table),
    {Holder,<<"SELECT ",S/binary," FROM ",Table/binary," ">>};
op_clause(update,SQL,Holder)->
    {Hodler0,SetFields} = 
        lists:foldl(fun(I,{H,Acc})->
            P = placeholder({'$',H}),
            F = escape_field(I), 
            {H + 1, [<<" SET ",F/binary," = ", P/binary, " ">>|Acc]}
        end,{Holder,[]},SQL#ai_sql.fields),
    Table = ai_string:to_string(SQL#ai_sql.table),
    S = ai_string:join(SetFields,<<" , ">>),
    {Hodler0,<<"UPDATE ",Table/binary,S/binary>>};
op_clause(insert,SQL,Hodler)->
    {Hodler0,Fields,Holders} =
        lists:foldl(fun(I,{H,FAcc,HAcc})->
                P = placeholder({'$',H}),
                F = escape_field(I), 
                {H + 1,[F|FAcc],[P|HAcc] }
            end,{Hodler,[],[]},SQL#ai_sql.fields),
    S1 = ai_string:join(Fields,<<",">>),
    S2 = ai_string:join(Holders,<<",">>),
    Table = ai_string:to_string(SQL#ai_sql.table),
    {Hodler0,<<"INSERT INTO ",Table/binary," ( ",S1/binary," ) VALUES ( ",S2/binary," ) ">>};
op_clause(delete,SQL,Hodler)->
    Table = ai_string:to_string(SQL#ai_sql.table),
    {Hodler, <<"DELETE FROM ",Table/binary," ">>}.


prepare_where(Expr,Holder)->
    {Values, CleanExprs, Holder0} = prepare_where_clause(Expr, {[], [], Holder}),
    io:format("clean exprs ~p~n",[CleanExprs]),
    {lists:reverse(Values), lists:reverse(CleanExprs),Holder0}.

prepare_where_clause(Exprs, Acc) when is_list(Exprs) ->
  lists:foldl(fun prepare_where_clause/2, Acc, Exprs);

prepare_where_clause({LogicalOp, Exprs}, {Values, CleanExprs, Count})
    when (LogicalOp == 'and') 
        or (LogicalOp == 'or') 
        or (LogicalOp == 'not') ->
    {NewValues, NewCleanExprs, NewCount} = prepare_where_clause(Exprs, {Values, [], Count}),
    {NewValues,
        [{LogicalOp, lists:reverse(NewCleanExprs)} | CleanExprs],
    NewCount};
prepare_where_clause({Op,Name,{value,Value}},{Values, CleanExprs, Count})->
    {[Value | Values],
        [{Op,Name, {'$', Count}} | CleanExprs],
    Count + 1};
prepare_where_clause({Op,Name,{field,_} = F},{Values, CleanExprs, Count})->
    {Values,
        [{Op,Name, F} | CleanExprs],
    Count};
prepare_where_clause({ Op,Name, Value}, {Values, CleanExprs, Count})
    when not is_atom(Value) ->
    {[Value | Values],
         [{Op,Name, {'$', Count}} | CleanExprs],
    Count + 1};
prepare_where_clause({Op,Name1, Name2}, {Values, CleanExprs, Count})
    when is_atom(Name2) ->
    {Values,
        [{Op,Name1, Name2} | CleanExprs],
    Count};
prepare_where_clause({Name, Value}, {Values, CleanExprs, Count})
    when Value =/= 'null' 
    andalso Value =/= 'not_null' ->
    {[Value | Values],
        [{Name, {'$', Count}} | CleanExprs],
    Count + 1};
prepare_where_clause({Name, Value}, {Values, CleanExprs, Count}) ->
    {Values,
        [{Name, Value} | CleanExprs],
    Count};
prepare_where_clause([], Acc) -> Acc;
prepare_where_clause(Expr, _) -> throw({unsupported,expression, Expr}).



where_clause([]) ->[];
where_clause(Exprs) when is_list(Exprs) ->
    Clauses = [where_clause(Expr) || Expr <- Exprs],
    ai_string:join(Clauses,<<" AND ">>);
where_clause({'and', Exprs}) -> 
        BinaryClauses = where_clause(Exprs),
    <<" ( ",BinaryClauses/binary," ) ">>;
where_clause({'or', Exprs}) ->
    Clauses = [where_clause(Expr) || Expr <- Exprs],
    BinaryClauses = ai_string:join(Clauses,<<" OR ">>),
    <<" ( ",BinaryClauses/binary," ) ">>;

where_clause({'not', Expr}) ->
    BinaryClauses = where_clause(Expr), 
    <<" NOT ( ",BinaryClauses/binary," ) ">>;


where_clause({Op,Name, { '$', _ } = Slot}) ->
    P = placeholder(Slot),
    N = escape_field(Name),
    O = operator(Op),
    <<N/binary,O/binary,P/binary>>;
where_clause({Op,Name1, {field,Name2}}) ->
    N1 = escape_field(Name1),
    N2 = escape_field(Name2),
    O = operator(Op),
    <<N1/binary,O/binary,N2>>;
where_clause({Op,Name1,Name2}) ->
    N1 = escape_field(Name1),
    N2 = escape_field(Name2),
    O = operator(Op),
    <<N1/binary,O/binary,N2>>;
where_clause({Name,  { '$', _ } = Slot}) ->
    P = placeholder(Slot),
    N = escape_field(Name),
    <<N/binary,"=",P/binary>>;
where_clause({Name, 'null'}) ->
    N = escape_field(Name),
    <<N/binary," IS NULL ">>;
where_clause({Name, 'not null'})->
    N = escape_field(Name),
    <<N/binary," IS NOT NULL ">>.


placeholder({Prefix, N}) ->
    P = ai_string:to_string(Prefix),
    H = ai_string:to_string(N),
    <<$\s,P/binary,H/binary,$\s>>.

escape_field(Field) -> 
    F = ai_string:to_string(Field),
    if 
        F == <<"*">> -> <<$\s,F/binary,$\s>>;
        true->
            F1 = re:replace(F,"\"","\\\"",[global,{return,binary}]),
            <<" \"",F1/binary,"\" ">>
    end.
%% https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-DOLLAR-QUOTING
%escape_value(Value)->
%    F = ai_string:to_string(Value),
%    case binary:match(F,<<"'">>) of 
%        nomatch ->  <<" '",F,"' ">>;
%        _-> 
%            F1 = re:replace(F,"'","\\'",[global,{return,binary}]),
%            <<" E'",F1,"' ">>
%    end.
    
operator('=<') -> <<"<=">>;
operator('/=') -> <<"!=">>;
operator('==') -> <<"=">>;
operator(Op) -> ai_string:to_string(Op).