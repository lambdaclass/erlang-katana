-module(ktn_lists).

-export([
         delete_first/2
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