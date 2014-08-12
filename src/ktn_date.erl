-module(ktn_date).

-export([
         now_human_readable/0
        ]).

%% @doc Returns the current date in a human readable format binary.
-spec now_human_readable() -> binary().
now_human_readable() ->
    TimeStamp = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} = calendar:now_to_universal_time(TimeStamp),
    DateList = io_lib:format("~p-~p-~pT~p:~p:~pZ",
                             [Year, Month, Day, Hour, Minute, Second]),
    list_to_binary(DateList).
