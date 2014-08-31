%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_maps: functions useful for handling maps
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_maps).

-export([
         get/2,
         get/3
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get(term(), map()) -> term().
get(Keys, Map) ->
    get(Keys, Map, undefined, error).

-spec get(term(), map(), term()) -> term().
get(Keys, Map, Default) ->
    get(Keys, Map, Default, default).

%% @private
-spec get(term(), map(), term(), error | default) -> term().
get([Key], Map, Default, Type) ->
    get(Key, Map, Default, Type);
get([Key | Rest], Map, Default, Type) ->
    case get(Key, Map, Default, Type) of
        NewMap when is_map(NewMap) ->
            get(Rest, NewMap, Default, Type);
        _ ->
            Default
    end;
get(Key, Map, Default, Type) ->
    case {Type, maps:is_key(Key, Map)} of
        {_, true} ->
            maps:get(Key, Map);
        {default, false} ->
            Default;
        {error, false} ->
            error(bad_path)
    end.
