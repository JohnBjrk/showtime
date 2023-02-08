-module(showtime_ffi).

-export([run_test/3, functions/0]).

run_test(Module, Function, IgnoreTags) ->
    try
        % io:fwrite("Testing~n"),
        Result = apply(Module, Function, []),
        % io:fwrite("Test result: ~p~n", [Result]),
        FinalResult =
            case Result of
                {test, {meta, _Description, Tags}, TestFun} ->
                    case lists:any(fun(Tag) ->
                                      lists:any(fun(IgnoreTag) -> IgnoreTag == Tag end, IgnoreTags)
                                   end,
                                   Tags)
                    of
                        true ->
                            {ignored, ignore};
                        false ->
                            {test_function_return, TestFun()}
                    end;
                DirectResult ->
                    {test_function_return, DirectResult}
            end,
        {ok, FinalResult}
    catch
        Class:Reason:Stacktrace ->
            % io:fwrite("Class ~p~nReason~p~nStacktrace~p~n", [Class, Reason, Stacktrace]),
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
                                             {pattern, Pattern} ->
                                                 {pattern, list_to_binary(Pattern)};
                                             Other -> Other
                                         end
                                      end,
                                      ReasonList),
                        % io:fwrite("Reason ~p~n", [ErlangReasonList]),
                        GleamAssertionType =
                            case Assertion of
                                assertEqual ->
                                    assert_equal;
                                assertNotEqual ->
                                    assert_not_equal;
                                assertMatch ->
                                    assert_match;
                                OtherAssertionType ->
                                    OtherAssertionType
                            end,
                        {GleamAssertionType, ErlangReasonList};
                    #{function := GleamFunction,
                      gleam_error := GleamError,
                      line := Line,
                      message := Message,
                      module := GleamModule,
                      value := Value} ->
                        case Value of
                            {error, {OkValue, _, _, _}} when OkValue == not_eq; OkValue == eq ->
                                {gleam_error,
                                 {GleamError, GleamModule, GleamFunction, Line, Message, Value}};
                            {error, {OkValue, _, _}} when OkValue == is_ok; OkValue == is_error ->
                                {gleam_error,
                                 {GleamError, GleamModule, GleamFunction, Line, Message, Value}};
                            {error, {OkValue, _}} when OkValue == fail ->
                                {gleam_error,
                                 {GleamError, GleamModule, GleamFunction, Line, Message, Value}};
                            _ ->
                                {gleam_assert, Value}
                        end;
                    OtherReason ->
                        {generic_exception, OtherReason}
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
            % io:fwrite("Reason ~p", [GleamReason]),
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

functions() ->
    Funs = module_info(exports),
    Funs.
