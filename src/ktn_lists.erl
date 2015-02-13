-module(ktn_lists).

-export([
         delete_first/2,
         split_when/2
        ]).

%% @doc Returns a copy of List deleting the first Element where Fun(Element)
%%      returns true, if there is such an element.
%% @end
-spec delete_first(fun((term()) -> boolean()), list()) -> list().
delete_first(Fun, List) ->
    delete_first(Fun, List, []).

delete_first(Fun, [], Acc) when is_function(Fun, 1) ->
    lists:reverse(Acc);
delete_first(Fun, [Head | Tail], Acc) ->
    case Fun(Head) of
        false ->
            delete_first(Fun, Tail, [Head | Acc]);
        true ->
            lists:concat([lists:reverse(Acc), Tail])
    end.

-spec split_when(fun(), list()) -> list().
split_when(When, List) ->
    split_when(When, List, [[]]).

split_when(When, [], [[] | Results]) ->
    split_when(When, [], Results);
split_when(_When, [], Results) ->
    Reversed = lists:map(fun lists:reverse/1, Results),
    lists:reverse(Reversed);
split_when(When, [Head | Tail], [Current0 | Rest]) ->
    Current = [Head | Current0],
    Result = case When(Head) of
                 true ->
                     [[], Current | Rest];
                 false ->
                     [Current | Rest]
             end,
    split_when(When, Tail, Result).
