-module(ktn_recipe_SUITE).

%% Common test
-export(
  [ all/0
  ]).
%% Test cases
-export(
  [ implicit_test/1
  , explicit_test/1
  , loop_test/1
  ]).
%% Test case explicit_test/1
-export(
  [ t1_s1/1
  , t1_s2/1
  , t1_s3/1
  , t1_s4/1
  ]).
%% Test case implicit_test/1
-export(
  [ t2_s1/1
  , t2_s2/1
  , t2_s3/1
  , t2_s4/1
  ]).
%% Auxiliary exports
-export(
  [ process_error/1
  , process_result/1
  ]).

all() ->
  [ implicit_test
  , explicit_test
  , loop_test
  ].

%%% ----------------------------------------------------------------------------
%%% CASE: module_test
%%%
%%%          s3                      error
%%%         /  \                    /
%%% s1 -> s2 -> s5 -> s6 -> s7 -> s8 -> s9 -> s0
%%%         \        /              \
%%%          s4------                halt
%%% ----------------------------------------------------------------------------

implicit_test(_Config) ->
  ct:comment("Verifying transitions..."),

  ok = ktn_recipe:verify(ktn_recipe_example),

  ct:comment("Running recipe"),
  Result = {ok, [s0_ok, s9_ok, s8_ok, s7_ok, s6_ok, s5_ok, s3_ok, s2_ok, s1_ok]},
  Result = ktn_recipe:run(ktn_recipe_example, []),

  {comment, ""}.

%%% ----------------------------------------------------------------------------
%%% CASE: explicit_test
%%%
%%% s1 -> s2 -> s3 -> s4
%%%              |
%%%              +-> error
%%%              +-> halt
%%% ----------------------------------------------------------------------------

t1_s1(S) -> io:format("s1~n"), {ok, [t1_s1_ok | S]}.
t1_s2(S) -> io:format("s2~n"), {ok, [t1_s2_ok | S]}.
t1_s3(S) -> io:format("s3~n"), {ok, [t1_s3_ok | S]}.
t1_s4(S) -> io:format("s4~n"), {ok, [t1_s4_ok | S]}.

explicit_test(_Config) ->
  Transitions =
    [  fun ?MODULE:t1_s1/1
    ,  fun ?MODULE:t1_s2/1
    , {fun ?MODULE:t1_s3/1, i1, halt}
    , {fun ?MODULE:t1_s3/1, i2, error}
    , {fun ?MODULE:t1_s3/1, ok, fun ?MODULE:t1_s4/1}
    ,  fun ?MODULE:t1_s4/1
    ],
  ResultFun    = fun ?MODULE:process_result/1,
  ErrorFun     = fun ?MODULE:process_error/1,
  InitialState = [],
  Result       = {ok, [t1_s4_ok, t1_s3_ok, t1_s2_ok, t1_s1_ok]},

  ct:comment("Verifying transitions..."),
  ok = ktn_recipe:verify(Transitions),

  ct:comment("Running recipe"),
  Result = ktn_recipe:run(Transitions, ResultFun, ErrorFun, InitialState),
  {comment, ""}.

%%% ----------------------------------------------------------------------------
%%% CASE: loop_test
%%%          ____
%%%         /    \
%%%        v     |
%%% s1 -> s2 -> s3 -> s4
%%% ----------------------------------------------------------------------------

t2_s1(S) -> {ok, S#{s1 => ok}}.
t2_s2(S = #{i := I}) -> {ok, S#{s2 => ok, i := I + 1}}.
t2_s3(S = #{i := I}) when I < 4 -> {inc, S};
t2_s3(S) -> {ok, S#{s3 => ok}}.
t2_s4(S) -> {ok, S#{s4 => ok}}.

loop_test(_Config) ->
  Transitions =
    [ fun ?MODULE:t2_s1/1
    , fun ?MODULE:t2_s2/1
    , {fun ?MODULE:t2_s3/1, inc, fun ?MODULE:t2_s2/1}
    , {fun ?MODULE:t2_s3/1, ok, fun ?MODULE:t2_s4/1}
    , fun ?MODULE:t2_s4/1
    ],
  ResultFun    = fun ?MODULE:process_result/1,
  ErrorFun     = fun ?MODULE:process_error/1,
  InitialState = #{i => 0},
  Result       = {ok, #{i => 4,s1 => ok,s2 => ok,s3 => ok,s4 => ok}},

  ct:comment("Verifying transitions..."),
  ok = ktn_recipe:verify(Transitions),

  ct:comment("Running recipe"),
  Result = ktn_recipe:run(Transitions, ResultFun, ErrorFun, InitialState),
  {comment, ""}.

%%% ----------------------------------------------------------------------------
%%% Auxiliary functions
%%% ----------------------------------------------------------------------------

process_result(S) ->
  {ok, S}.

process_error(S) ->
  {error, S}.

