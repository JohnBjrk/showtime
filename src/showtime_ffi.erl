-module(showtime_ffi).

-export([run_test/3, functions/0, capture_output/1]).

start_output_capture() ->
    OldGroupLeader = group_leader(),
    CapturePid = spawn(showtime_ffi, capture_output, [{[], OldGroupLeader}]),
    group_leader(CapturePid, self()),
    {CapturePid, OldGroupLeader}.

stop_output_capture({CapturePid, OldGroupLeader}) ->
    group_leader(OldGroupLeader, self()),
    CapturePid ! {capture_done, self()},
    receive
        Buffer ->
            Buffer
    end.

capture_output({Buffer, OldGroupLeader}) ->
    receive
        {io_request, From, ReplyAs, {put_chars, unicode, BitString}} ->
            From ! {io_reply, ReplyAs, ok},
            capture_output({[BitString | Buffer], OldGroupLeader});
        {capture_done, SenderPid} ->
            SenderPid ! Buffer;
        OtherMessage ->
            OldGroupLeader ! OtherMessage,
            capture_output({Buffer, OldGroupLeader})
    end.

run_test(Module, Function, IgnoreTags) ->
    OutputCapture = start_output_capture(),
    try
        % io:fwrite("Testing~n"),
        Result = apply(Module, Function, []),
        {ResultType, FinalResult} =
            case Result of
                {test, {meta, _Description, Tags}, TestFun} ->
                    case
                        lists:any(
                            fun(Tag) ->
                                lists:any(fun(IgnoreTag) -> IgnoreTag == Tag end, IgnoreTags)
                            end,
                            Tags
                        )
                    of
                        true ->
                            {ignored, ignore};
                        false ->
                            {test_function_return, TestFun()}
                    end;
                DirectResult ->
                    {test_function_return, DirectResult}
            end,
        OutputCaptureBuffer = stop_output_capture(OutputCapture),
        {ok, {ResultType, FinalResult, OutputCaptureBuffer}}
    catch
        Class:Reason:Stacktrace ->
            % io:fwrite("Class ~p~nReason~p~nStacktrace~p~n", [Class, Reason, Stacktrace]),
            GleamReason =
                case Reason of
                    {Assertion, ReasonList} ->
                        ErlangReasonList =
                            lists:map(
                                fun(ReasonDetail) ->
                                    case ReasonDetail of
                                        {line, LineNo} ->
                                            {reason_line, LineNo};
                                        {expression, List} ->
                                            {expression, list_to_binary(List)};
                                        {module, ModuleAtom} ->
                                            {module, atom_to_binary(ModuleAtom)};
                                        {pattern, Pattern} ->
                                            {pattern, list_to_binary(Pattern)};
                                        Other ->
                                            Other
                                    end
                                end,
                                ReasonList
                            ),
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
                    #{
                        function := GleamFunction,
                        gleam_error := GleamError,
                        line := Line,
                        message := Message,
                        module := GleamModule,
                        value := Value
                    } ->
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
                                {gleam_assert, Value, Line}
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
                lists:map(
                    fun(Trace) ->
                        case Trace of
                            {ModuleName, FunctionName, Arity, ExtraInfoList} ->
                                {trace_module, atom_to_binary(ModuleName),
                                    atom_to_binary(FunctionName), map_arity(Arity),
                                    map_extra_info_list(ExtraInfoList)};
                            {FunctionName, Arity, ExtraInfoList} ->
                                {trace, atom_to_binary(FunctionName), map_arity(Arity),
                                    map_extra_info_list(ExtraInfoList)}
                        end
                    end,
                    Stacktrace
                ),
            OutputCaptureBufferCatch = stop_output_capture(OutputCapture),
            {error,
                {erlang_exception, GleamClass, GleamReason, {trace_list, GleamTraceList},
                    OutputCaptureBufferCatch}}
    end.

map_extra_info_list(ExtraInfoList) ->
    lists:map(
        fun(ExtraInfo) ->
            case ExtraInfo of
                {file, FileNameList} -> {file, list_to_binary(FileNameList)};
                Other -> Other
            end
        end,
        ExtraInfoList
    ).

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
