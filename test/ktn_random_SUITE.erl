-module(ktn_random_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2
        ]).

-export([
         generate/1,
         uniform/1,
         pick/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite,
         init_per_testcase
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

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
    {ok, _} = ktn_random:start_link(),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec generate(config()) -> ok.
generate(_Config) ->
    true = is_list(ktn_random:generate()),
    16 = length(ktn_random:generate()),
    25 = length(ktn_random:generate(25)).

-spec uniform(config()) -> ok.
uniform(_Config) ->
    Times = 10000,
    do_times(fun (_) -> in_range(ktn_random:uniform(10), 1, 10) end, Times),
    {error, _} = ktn_random:uniform(0),

    do_times(fun (_) -> in_range(ktn_random:uniform(5, 90), 5, 90) end, Times),
    {error, _} = ktn_random:uniform(165, 165),
    {error, _} = ktn_random:uniform(15, 5).

-spec pick(config()) -> ok.
pick(_Config) ->
    [1, 2, 3, 4] = [ktn_random:pick([I]) || I <- lists:seq(1, 4)],
    lists:foreach(
        fun(I) ->
            List = lists:seq($a, $a + I),
            K = ktn_random:pick(List),
            true = lists:member(K, List)
        end, lists:seq(1, 1000)),
    try ktn_random:pick([]) of
        K -> ct:fail("Unexpected result: ~p", [K])
    catch
        _:function_clause -> ok
    end.

do_times(Fun, N) ->
    lists:foreach(Fun, lists:seq(1, N)).

in_range(X, Min, Max) when Min =< X, X =< Max -> ok.
