%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_numbers: functions useful for processing numeric values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_numbers).

-export([
         binary_to_number/1
        ]).

-spec binary_to_number(binary()) -> float() | integer().
binary_to_number(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _Rest} -> F
    end.
