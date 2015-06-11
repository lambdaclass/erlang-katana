-module(ktn_lists).

-export([
         delete_first/2,
         split_when/2,
         map/3,
         filter/3
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

%% @doc Splits a list whenever an element satisfies the When predicate.
%%      Returns a list of lists where each list includes the matched element
%%      as its last one.
%%      E.g.
%%        split_when(fun (X) -> $. == X end, "a.b.c") = ["a.", "b.", "c"]
%% @end
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

%% @doc Like lists:map/2 but allows specifying additional arguments.
%%      E.g.
%%        ktn_lists:map(fun (X, Y) -> X * Y end, [2], [1, 2, 3]) = [2, 4, 6]
%% @end
-spec map(fun(), list(), list()) -> list().
map(Fun, Args, [Head | Tail]) ->
    [apply(Fun, [Head | Args])| map(Fun, Args, Tail)];
map(Fun, _, []) when is_function(Fun) ->
    [].

%% @doc Like lists:filter/2 but allows specifying additional arguments.
%%      E.g.
%%        ktn_lists:filter(fun (X, Y) -> X * Y < 3 end, [2], [1, 2, 3]) = [2]
%% @end
-spec filter(fun(), list(), list()) -> list().
filter(Pred, Args, List) when is_function(Pred) ->
    [Elem || Elem <- List, apply(Pred, [Elem | Args])].
