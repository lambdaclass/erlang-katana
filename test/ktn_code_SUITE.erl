-module(ktn_code_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         consult/1
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

-spec consult(config()) -> ok.
consult(_Config) ->
    [{a}, {b}] = ktn_code:consult("{a}. {b}."),
    [] = ktn_code:consult(""),
    [{a}, {b}, {c, d, e}] = ktn_code:consult("{a}. {b}. {c, d, e}."),
    [{a}, {b}, {c, d, e}] = ktn_code:consult("{a}.\r\n{b}.\r\n{c, d, e}."),

    [{'.'}] = ktn_code:consult("{'.'}.\n"),
    [{<<"ble.bla">>}, {"github.com"}] =
        ktn_code:consult("{<<\"ble.bla\">>}.\n{\"github.com\"}.\r\n").
