-module(ktn_recipe_example).
-author('igarai@gmail.com').

-behaviour (ktn_recipe).

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
  , s8/1
  , s9/1
  , s0/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          s3                      error
%%%         /  \                    /
%%% s1 -> s2 -> s5 -> s6 -> s7 -> s8 -> s9 -> s0
%%%         \        /              \
%%%          s4------                halt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transitions() ->
  [ s1
  , {s2, i1, s3}
  , {s2, i2, s4}
  , {s2, i3, s5}
  , {s3, i4, s5}
  , {s4, i5, s6}
  , s5
  , {s6, ok, fun ktn_recipe_example:s7/1}
  , {fun ktn_recipe_example:s7/1, ok, s8}
  , s8
  , {s8, i6, error}
  , {s8, i7, halt}
  , fun ktn_recipe_example:s9/1
  , s0
  ].

process_result(S) ->
  [result | S].

process_error(S) ->
  [error | S].

s1(S) -> {ok, [s1_ok | S]}.
s2(S) -> {i1, [s2_ok | S]}.
s3(S) -> {i4, [s3_ok | S]}.
s4(S) -> {i5, [s4_ok | S]}.
s5(S) -> {ok, [s5_ok | S]}.
s6(S) -> {ok, [s6_ok | S]}.
s7(S) -> {ok, [s7_ok | S]}.
s8(S) -> {ok, [s8_ok | S]}.
s9(S) -> {ok, [s9_ok | S]}.
s0(S) -> {ok, [s0_ok | S]}.
