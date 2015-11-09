-module(ktn_binary_SUITE).


-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         join/1
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

-spec join(config()) -> ok.
join(_Config) ->
    Binaries = [<<"foo">>, <<"bar">>, <<"buzz">>],
    <<>> = ktn_binary:join([], <<", ">>),
    <<"foo">> = ktn_binary:join([<<"foo">>], <<", ">>),
    <<"foo, bar, buzz">> = ktn_binary:join(Binaries, <<", ">>),
    ok.
