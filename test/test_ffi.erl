-module(test_ffi).

-export([throw_exception/1]).

throw_exception(Exception) ->
  error(Exception).
