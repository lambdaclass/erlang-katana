%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_date: functions useful for handling dates and time values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_date).

-export([
         now_human_readable/0
        ]).

-type date()     :: {date, {non_neg_integer(), 1..12, 1..31}}.
-type datetime() :: { datetime
                    , {{integer(), 1..12, 1..31}
                    , {1..24, 1..60, 1..60}}
                    }.

-export_type(
  [ date/0
  , datetime/0
  ]).

%% @doc Returns the current date in a human readable format binary.
-spec now_human_readable() -> binary().
now_human_readable() ->
    TimeStamp = {_, _, Micro} = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} = calendar:now_to_universal_time(TimeStamp),
    DateList = io_lib:format("~p-~2..0B-~2..0BT~p:~p:~p.~6..0wZ",
                             [Year, Month, Day, Hour, Minute, Second, Micro]),
    list_to_binary(DateList).
