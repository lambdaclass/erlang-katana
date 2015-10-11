-module(ktn_xref_SUITE).
-author('elbrujohalcon@inaka.net').

-ignore_xref([all/0]).
-ignore_xref([xref/1]).

-export([all/0]).
-export([xref/1]).

-spec all() -> [xref].
all() -> [xref].

-spec xref(any()) -> {comment, []}.
xref(_Config) ->
  Dirs = [filename:absname("../../ebin")], %%, filename:absname("../../test")],
  [] = xref_runner:check(undefined_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(undefined_functions, #{dirs => Dirs}),
  [] = xref_runner:check(locals_not_used, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_function_calls, #{dirs => Dirs}),
  [] = xref_runner:check(deprecated_functions, #{dirs => Dirs}),
  {comment, ""}.
