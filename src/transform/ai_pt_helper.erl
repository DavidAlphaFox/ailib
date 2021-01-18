-module(ai_pt_helper).
-export([add_function/4,
         add_record/3,
         transform/3]).

-record(ai_pt_ctx, {ast,
                    options,
                    main_file,
                    compile_options,
                    last_line,
                    module_name,
                    exports = [],
                    exports_pos = -1,
                    directives = [],
                    functions = [],
                    function_pos = -1,

                    added_functions = [],
                    added_exports = [],
                    added_records = []}).

-record(ai_pt_fun, {index,
                    name,
                    visibility,
                    arity,
                    clauses,
                    ast}).
-record(ai_pt_record, {index,
                       name,
                       fields,
                       ast,
                       ast_type,
                       has_type}).

-type ai_pt_ctx() :: #ai_pt_ctx{}.
-type ast() :: tuple() | [ast()].

-spec add_function(ai_pt_ctx(), export | not_export, atom(), tuple() | list()) -> ai_pt_ctx().
add_function(Ctx = #ai_pt_ctx{added_functions = AddedFunctions},
             Visibility,Name,Clauses) ->
  ArityAndClause =
    case is_list(Clauses) of
      true -> {S, A} = get_arity_(Clauses), {S, A, Clauses};
      false -> {S, A} = get_arity_([Clauses]), {S, A, [Clauses]}
  end,
  case ArityAndClause of
    {ok, Arity, ASTClauses} ->
      NewFun = #ai_pt_fun{name = Name,
                          visibility = Visibility,
                          arity = Arity,
                          clauses = Clauses,
                          ast =  ai_pt:build_function(Name, Arity, ASTClauses)},
      Ctx#ai_pt_ctx{added_functions = AddedFunctions ++ [NewFun]};
    _ -> throw(function_clause_add_function)
  end.
get_arity_(Clauses) ->
  lists:foldl(
    fun(Clause, {Status, Arity}) ->
        if
          Status =:= ok ->
            case Clause of
              {clause, _, Parameters, _, _} ->
                if
                  Arity =:= -1 orelse Arity =:= length(Parameters) -> {Status, length(Parameters)};
                  true -> {error, Arity}
                end;
              _ -> {error, Arity}
            end;
          true -> {Status, Arity}
        end
    end, {ok, -1}, Clauses).


-spec add_record(ai_pt_ctx(), atom(), list()) -> ai_pt_ctx().
add_record(Ctx = #ai_pt_ctx{added_records = AddedRecs}, Name, Attributes) ->
  {Attrs, AttrTypes, HasType} = add_record_fields_(Attributes, [], [], false),
  AstRecord = ai_pt:build_attribute(record,{Name, Attrs}),
  AstType = ai_pt:build_attribute(type,{{record, Name}, AttrTypes}),
  Record = #ai_pt_record{name = Name,
                         fields = Attributes,
                         ast = AstRecord,
                         ast_type = AstType,
                         has_type = HasType},
  Ctx#ai_pt_ctx{added_records = AddedRecs ++ [Record]}.

add_record_fields_([], Result, ResultType, HasTypes) ->
  {Result, ResultType, HasTypes};
add_record_fields_([{Attr, Types}|Attributes], Result, ResultType, HasTypes) when is_list(Types) ->
  RecordField = ai_pt:build_record_field(Attr),
  {NewResultType, NewHasType} =
    if
      length(Types) > 0 ->
        TypesList = lists:map(fun(E) -> {type, 1, E, []} end, Types),
        {ResultType ++ [{typed_record_field,
                         RecordField,
                         {type, 1, union, [{atom, 1, undefined}] ++ TypesList}}
                       ], true};
    true -> {ResultType ++ [RecordField], HasTypes}
  end,
  add_record_fields_(Attributes,Result ++ [RecordField],
                     NewResultType,NewHasType);

add_record_fields_([{Attr, Type}|Attributes], Result, ResultType, _HasTypes) when is_atom(Type) ->
  RecordField = ai_pt:build_record_field(Attr),
  add_record_fields_(Attributes,Result ++ [RecordField],
                     ResultType ++ [{typed_record_field,
                                     RecordField,
                                     {type, 1, union, [{atom, 1, undefined}, {type,1, Type, []}]}}],
                     true);
add_record_fields_([Attr|Attributes], Result, ResultType, HasTypes) when is_atom(Attr) ->
  RecordField = ai_pt:build_record_field(Attr),
  add_record_fields_(Attributes,Result ++ [RecordField],
                     ResultType ++ [RecordField],HasTypes).


-spec transform(function(), string(), list()) -> ast().
transform(Fun, AST, Options) ->
  Ctx = parse(AST, Options),
  Ctx1 = Fun(Ctx),
  generate(Ctx1).

-spec parse(string(), list()) -> ai_pt_ctx().
parse(AST, Options) ->
  [{attribute, _, file, {FileName, _}}|NextAST] = AST,
  Ctx = #ai_pt_ctx{ main_file = FileName,
                       ast = AST,
                       options = Options},
  lists:foldl(fun parse_definition/2,Ctx,NextAST).

parse_definition({attribute, _, Directive, Data},
                 Ctx = #ai_pt_ctx{directives = Directives}) ->
  Ctx#ai_pt_ctx{directives = Directives ++ [{Directive, Data}]};
parse_definition({eof, N}, Ctx) -> Ctx#ai_pt_ctx{last_line = N};
parse_definition(_Def,Ctx) -> Ctx.

-spec generate(ai_pt_ctx()) -> ast().
generate(#ai_pt_ctx{ast = AST,
                    exports_pos = ExportPos,
                    function_pos = FunctionPos,
                    added_functions = AddedFunctions,
                    added_records = AddedRecords
                   }) ->
  {AstToExport, AstTypes, AstFunctions, AstEof} =
    cut_ast_(AST,ExportPos,FunctionPos),
  AstToExport ++
    generate_exports_(AddedFunctions) ++
    AstTypes ++
    generate_records_(AddedRecords) ++
    AstFunctions ++
    generate_functions_(AddedFunctions) ++
    AstEof.

cut_ast_(AST, -1, -1) ->
  {lists:sublist(AST, 1, length(AST) - 1),
   [],
   [],
   [lists:nth(length(AST), AST)]};
cut_ast_(AST, -1, FunctionPos) ->
  {lists:sublist(AST, 1, FunctionPos),
   [],
   lists:sublist(AST, FunctionPos + 1, length(AST) - FunctionPos - 1),
   [lists:nth(length(AST), AST)]};
cut_ast_(AST, ExportPos, -1) ->
  {lists:sublist(AST, 1, ExportPos + 1),
   lists:sublist(AST, ExportPos + 2, length(AST) - ExportPos - 2),
   [],
   [lists:nth(length(AST), AST)]};
cut_ast_(AST, ExportPos, FunctionPos) ->
  {lists:sublist(AST, 1, ExportPos + 1),
   lists:sublist(AST, ExportPos + 2, FunctionPos - ExportPos - 1),
   lists:sublist(AST, FunctionPos + 1, length(AST) - FunctionPos - 1),
   [lists:nth(length(AST), AST)]}.

generate_exports_(AddedFunctions) ->
  Exports =
    lists:foldl(
      fun(#ai_pt_fun{name = Name, arity = Arity,visibility = Visibility},
          Ast) ->
          if Visibility == true -> Ast ++ [{Name, Arity}];
             true -> Ast
          end
      end, [], AddedFunctions),
  if
    length(Exports) > 0 -> [{attribute, 1, export, Exports}];
    true -> []
  end.

generate_functions_(AddedFunctions) ->
  lists:foldl(
    fun(#ai_pt_fun{ast = AstFunction}, Ast) ->
        Ast ++ [AstFunction]
    end, [], AddedFunctions).

generate_records_(AddedRecords) ->
  lists:foldl(
    fun(#ai_pt_record{ast = AstRecord,
                      ast_type = AstRecordType,
                      has_type = HasType}, Ast) ->
        Ast ++ [AstRecord] ++ if
                                HasType =:= true -> [AstRecordType];
                                true -> []
                              end
    end, [], AddedRecords).

