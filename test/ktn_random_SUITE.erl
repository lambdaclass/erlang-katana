-module(ktn_random_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         string/1,
         uniform/1,
         pick/1
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

-spec string(config()) -> ok.
string(_Config) ->
    true = is_list(ktn_random:string()),
    16 = length(ktn_random:string()),
    25 = length(ktn_random:string(25)),
    ok.

-spec uniform(config()) -> ok.
uniform(_Config) ->
    Times = 10000,
    do_times(fun (_) -> in_range(ktn_random:uniform(10), 1, 10) end, Times),
    {error, _} = ktn_random:uniform(0),

    do_times(fun (_) -> in_range(ktn_random:uniform(5, 90), 5, 90) end, Times),
    {error, _} = ktn_random:uniform(165, 165),
    {error, _} = ktn_random:uniform(15, 5),
    ok.

-spec pick(config()) -> ok.
pick(_Config) ->
    [1, 2, 3, 4] = [ktn_random:pick([I]) || I <- lists:seq(1, 4)],
    lists:foreach(
        fun(I) ->
            List = lists:seq($a, $a + I),
            K = ktn_random:pick(List),
            true = lists:member(K, List)
        end, lists:seq(1, 1000)).

do_times(Fun, N) ->
    lists:foreach(Fun, lists:seq(1, N)).

in_range(X, Min, Max) when Min =< X, X =< Max -> ok.
