-module(ktn_maps_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         find_nested_values/1,
         find_shallow_values/1,
         dont_find_nested_values/1,
         dont_find_shallow_values/1,
         provide_default/1
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
    Map = #{user => "john.doe",
            name => "John",
            last_name => "Doe",
            location => #{latitude => 1.5,
                          longitude => 2.5},
            conversation => #{destination => #{ip => "127.0.0.1",
                                               port => 8080}}
           },
    [{map, Map} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec find_nested_values(config()) -> ok.
find_nested_values(Config) ->
    Map = proplists:get_value(map, Config),
    8080 = ktn_maps:get([conversation, destination, port], Map),
    1.5 = ktn_maps:get([location, latitude], Map).

-spec find_shallow_values(config()) -> ok.
find_shallow_values(Config) ->
    Map = proplists:get_value(map, Config),
    "john.doe" = ktn_maps:get(user, Map),
    "John" = ktn_maps:get(name, Map),
    "Doe" = ktn_maps:get(last_name, Map),
    "john.doe" = ktn_maps:get([user], Map).

-spec dont_find_nested_values(config()) -> ok.
dont_find_nested_values(Config) ->
    Map = proplists:get_value(map, Config),
    undefined = ktn_maps:get([address, country, city], Map),
    undefined = ktn_maps:get([social, facebook], Map).


-spec dont_find_shallow_values(config()) -> ok.
dont_find_shallow_values(Config) ->
    Map = proplists:get_value(map, Config),
    undefined = ktn_maps:get(username, Map),
    undefined = ktn_maps:get(email, Map),
    undefined = ktn_maps:get([email], Map).


-spec provide_default(config()) -> ok.
provide_default(Config) ->
    Map = proplists:get_value(map, Config),

    8080 = ktn_maps:get([conversation, destination, port], Map, default),
    1.5 = ktn_maps:get([location, latitude], Map, default),

    "john.doe" = ktn_maps:get(user, Map, default),
    "John" = ktn_maps:get(name, Map, default),
    "Doe" = ktn_maps:get(last_name, Map, default),
    "john.doe" = ktn_maps:get([user], Map, default),

    default = ktn_maps:get([address, country, city], Map, default),
    default = ktn_maps:get([social, facebook], Map, default),

    default = ktn_maps:get(username, Map, default),
    default = ktn_maps:get(email, Map, default),
    default = ktn_maps:get([email], Map, default).
