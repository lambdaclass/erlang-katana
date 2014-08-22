-module(ktn_recipe_example).
-author('igarai@gmail.com').

%%% Required
-export(
  [ transitions/0
  , process_result/1
  , process_error/1
  ]).
%%% Steps
-export(
  [ s1/1
  , s2/1
  , s3/1
  , s4/1
  , s5/1
  , s6/1
  , s7/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          s3
%%%         /  \
%%% s1 -> s2 -> s5 -> s6 -> s7
%%%         \        /
%%%          s4------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transitions() ->
  [ s1 % -> {s1, ok, s2}
  , {s2, i1, s3}
  , {s2, i2, s4}
  , {s2, i3, s5}
  , {s3, i4, s5}
  , {s4, i5, s6}
  , s5
  , s6
  , s7
  ].

process_result(_S) ->
  result.

process_error(_S) ->
  error.

s1(S) -> io:format("s1~n"), {ok, [s1_ok|S]}.

s2(S) -> io:format("s2~n"), {i1, [s2_ok|S]}.

s3(S) -> io:format("s3~n"), {i4, [s3_ok|S]}.

s4(S) -> io:format("s4~n"), {i5, [s4_ok|S]}.

s5(S) -> io:format("s5~n"), {ok, [s5_ok|S]}.

s6(S) -> io:format("s6~n"), {ok, [s6_ok|S]}.

s7(S) -> io:format("s7~n"), {ok, [s7_ok|S]}.