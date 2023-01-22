-module(showtime_ffi).

-export([run_test/3, functions/0, diff/2]).

run_test(Module, Function, Args) ->
    try
        % io:fwrite("Testing~n"),
        Result = apply(Module, Function, Args),
        % io:fwrite("Test result: ~p~n", [Result]),
        FinalResult = case Result of
            {test, {meta, _Description, Tags}, TestFun} ->
                case lists:any(fun(Tag) ->
                    Tag == <<"ignore">>
                end, Tags) of
                    true -> {ignored, ignore};
                    false -> {test_function_return, TestFun()}
                end;
            DirectResult -> {test_function_return, DirectResult}
        end,
        {ok, FinalResult}
    catch
        Class:Reason:Stacktrace ->
            GleamReason =
                case Reason of
                    {Assertion, ReasonList} ->
                        ErlangReasonList =
                            lists:map(fun(ReasonDetail) ->
                                         case ReasonDetail of
                                             {line, LineNo} -> {reason_line, LineNo};
                                             {expression, List} ->
                                                 {expression, list_to_binary(List)};
                                             {module, ModuleAtom} ->
                                                 {module, atom_to_binary(ModuleAtom)};
                                             Other -> Other
                                         end
                                      end,
                                      ReasonList),
                        % io:fwrite("Reason ~p", [ErlangReasonList]),
                        GleamAssertionType = case Assertion of
                            assertEqual -> assert_equal;
                            OtherAssertionType -> OtherAssertionType
                        end,
                        {GleamAssertionType, ErlangReasonList};
                    #{function := GleamFunction,
                      gleam_error := GleamError,
                      line := Line,
                      message := Message,
                      module := GleamModule,
                      value := Value} ->
                        {gleam_error,
                         {GleamError, GleamModule, GleamFunction, Line, Message, Value}}
                end,
            GleamClass =
                case Class of
                    error ->
                        erlang_error;
                    Other ->
                        Other
                end,
            GleamTraceList =
                lists:map(fun(Trace) ->
                             case Trace of
                                 {ModuleName, FunctionName, Arity, ExtraInfoList} ->
                                     {trace_module,
                                      atom_to_binary(ModuleName),
                                      atom_to_binary(FunctionName),
                                      map_arity(Arity),
                                      map_extra_info_list(ExtraInfoList)};
                                 {FunctionName, Arity, ExtraInfoList} ->
                                     {trace,
                                      atom_to_binary(FunctionName),
                                      map_arity(Arity),
                                      map_extra_info_list(ExtraInfoList)}
                             end
                          end,
                          Stacktrace),
            {error, {erlang_exception, GleamClass, GleamReason, {trace_list, GleamTraceList}}}
    end.

map_extra_info_list(ExtraInfoList) ->
    lists:map(fun(ExtraInfo) ->
                 case ExtraInfo of
                     {file, FileNameList} -> {file, list_to_binary(FileNameList)};
                     Other -> Other
                 end
              end,
              ExtraInfoList).

map_arity(Arity) ->
    case Arity of
        [head | tail] ->
            {arg_list, [head | tail]};
        Num ->
            {num, Num}
    end.

diff(List1, List2) when is_list(List1), is_list(List2) ->
    InBoth = sets:to_list(sets:intersection([sets:from_list(List1),sets:from_list(List2)])),
    Annotated1 = lists:map(fun(Element) ->
                case lists:member(Element, InBoth) of
                    true -> {in_both, Element};
                    false -> {unique, Element}
                end
            end,
            List1),
    Annotated2 = lists:map(fun(Element) ->
                case lists:member(Element, InBoth) of
                    true -> {in_both, Element};
                    false -> {unique, Element}
                end
            end,
            List2),
    {diff, Annotated1, Annotated2};

diff(V1, V2) ->
    {literal, V1, V2}.
functions() ->
    Funs = module_info(exports),
    Funs.
