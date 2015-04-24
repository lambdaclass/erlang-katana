-module(ktn_lists_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         delete_first/1,
         split_when/1,
         map/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_first(config()) -> ok.
delete_first(_Config) ->
    Fun = fun(N) -> 0 == N rem 2 end,

    [] = ktn_lists:delete_first(Fun, []),
    [] = ktn_lists:delete_first(Fun, [4]),
    [4] = ktn_lists:delete_first(Fun, [4, 4]),
    [1, 3] = ktn_lists:delete_first(Fun, [1, 3]),
    [1, 3] = ktn_lists:delete_first(Fun, [1, 4, 3]),
    [1, 3, 4] = ktn_lists:delete_first(Fun, [1, 4, 3, 4]).

-spec split_when(config()) -> ok.
split_when(_Config) ->
    IsDot = fun(Ch) -> $. == Ch end,

    ["{a}.", " {b}."] = ktn_lists:split_when(IsDot, "{a}. {b}."),
    [] = ktn_lists:split_when(IsDot, ""),
    ["."] = ktn_lists:split_when(IsDot, "."),
    ["{a}.", " {b}.", "{c, d, e}"] =
        ktn_lists:split_when(IsDot, "{a}. {b}.{c, d, e}"),
    ["{a} {b}{c, d, e}"] = ktn_lists:split_when(IsDot, "{a} {b}{c, d, e}").

-spec map(config()) -> ok.
map(_Config) ->
    Sum = fun(X, Y) -> X + Y end,
    [3, 4, 5] = ktn_lists:map(Sum, [2], [1, 2, 3]),

    Multiply = fun(X, Y) -> X * Y end,
    [2, 4, 6] = ktn_lists:map(Multiply, [2], [1, 2, 3]),

    SumMultiply = fun(X, Y, Z) -> (X + Y) * Z end,
    [30, 40, 50] = ktn_lists:map(SumMultiply, [2, 10], [1, 2, 3]).
