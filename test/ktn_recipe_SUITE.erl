-module(ktn_recipe_SUITE).

-export(
  [ all/0
  , module_test/1
  , transition_test/1
  ]).

-export(
  [ s1/1
  , s2/1
  , s3/1
  , s4/1
  , process_error/1
  , process_result/1
  ]).

all() ->
  [ module_test
  , transition_test
  ].

%%% ----------------------------------------------------------------------------
%%% CASE: module_test
%%%

%%% ----------------------------------------------------------------------------

module_test(_Config) ->
  ok = ktn_recipe:verify(ktn_recipe_example),
  [ result
  , s0_ok
  , s9_ok
  , s8_ok
  , s7_ok
  , s6_ok
  , s5_ok
  , s3_ok
  , s2_ok
  , s1_ok
  ] = ktn_recipe:run(ktn_recipe_example, []),
  ok.

%%% ----------------------------------------------------------------------------
%%% CASE: transition_test
%%%
%%% s1 -> s2 -> s3 -> s4
%%%              |
%%%              +-> error
%%%              +-> halt
%%% ----------------------------------------------------------------------------

s1(S) -> {ok, [s1_ok | S]}.
s2(S) -> {ok, [s2_ok | S]}.
s3(S) -> {ok, [s3_ok | S]}.
s4(S) -> {ok, [s4_ok | S]}.
process_result(S) -> [result | S].
process_error(S)  -> [error | S].

transition_test(_Config) ->
  Transitions =
    [ fun ?MODULE:s1/1
    , fun ?MODULE:s2/1
    , {fun ?MODULE:s3/1, i1, halt}
    , {fun ?MODULE:s3/1, i2, error}
    , {fun ?MODULE:s3/1, ok, fun ?MODULE:s4/1}
    , fun ?MODULE:s4/1
    ],
  ok = ktn_recipe:verify(Transitions),
  [ result
  , s4_ok
  , s3_ok
  , s2_ok
  , s1_ok
  ] = ktn_recipe:run(
      Transitions,
      fun ?MODULE:process_result/1,
      fun ?MODULE:process_error/1,
      []
    ),
  ok,
  Transitions.
