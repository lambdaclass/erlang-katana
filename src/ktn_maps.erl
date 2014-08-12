-module(ktn_maps).

-export([
         get/2,
         get/3
        ]).

-spec get(term(), map()) -> term().
get(Keys, Map) ->
    get(Keys, Map, undefined).

-spec get(term(), map(), term()) -> term().
get([Key], Map, Default) ->
    get(Key, Map, Default);
get([Key | Rest], Map, Default) ->
    case get(Key, Map, Default) of
        NewMap when is_map(NewMap) ->
            get(Rest, NewMap, Default);
        _ ->
            Default
    end;
get(Key, Map, Default) ->
    case maps:is_key(Key, Map) of
        true ->
            maps:get(Key, Map);
        false ->
            Default
    end.
