-module(showtime_ffi).

-export([run_test/3, functions/0]).

run_test(Module, Function, Args) ->
    try
        Result = apply(Module, Function, Args),
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            % io:fwrite("Reason ~p", [Reason]),
            GleamReason =
                case Reason of
                    {Assertion, ReasonList} ->
                        ErlangReasonList = lists:map(fun(ReasonDetail) ->
                                     case ReasonDetail of
                                         {line, LineNo} -> {reason_line, LineNo};
                                         {expression, List} -> {expression, list_to_binary(List)};
                                         Other -> Other
                                     end
                                  end,
                                  ReasonList),
                        {Assertion, ErlangReasonList};
                    #{function := GleamFunction, gleam_error := GleamError, line := Line, message := Message, module := GleamModule, value := Value} ->
                        {gleam_error, {GleamError, GleamModule, GleamFunction, Line, Message, Value}}
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
                                      ModuleName,
                                      FunctionName,
                                      map_arity(Arity),
                                      map_extra_info_list(ExtraInfoList)};
                                 {FunctionName, Arity, ExtraInfoList} ->
                                     {trace,
                                      FunctionName,
                                      map_arity(Arity),
                                      map_extra_info_list(ExtraInfoList)}
                             end
                          end,
                          Stacktrace),
            {error, {erlang_exception,
             GleamClass,
             GleamReason,
             {trace_list, GleamTraceList}}}
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
