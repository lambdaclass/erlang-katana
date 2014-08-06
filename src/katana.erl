-module(katana).

-export([
         wait_for/2,
         wait_for/4,
         now_human_readable/0
        ]).

wait_for(Task, Answer) ->
    wait_for(Task, Answer, 200, 10).

wait_for(Task, ExpectedAnswer, SleepTime, Retries) ->
    wait_for(Task, ExpectedAnswer, undefined, SleepTime, Retries).
wait_for(_Task, ExpectedAnswer, ReceivedAnswer, _SleepTime, 0) ->
    {error, {timeout, {expected, ExpectedAnswer}, {last_received, ReceivedAnswer}}};
wait_for(Task, ExpectedAnswer, _ReceivedAnswer, SleepTime,Retries) ->
    case Task() of
        ExpectedAnswer ->
            {ok, ExpectedAnswer};
        NewReceivedAnswer ->
            timer:sleep(SleepTime),
            wait_for(Task, ExpectedAnswer, NewReceivedAnswer, SleepTime, Retries - 1)
    end.

now_human_readable() ->
    TimeStamp = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} = calendar:now_to_universal_time(TimeStamp),
    DateList = io_lib:format("~p-~p-~pT~p:~p:~pZ",
                             [Year, Month, Day, Hour, Minute, Second]),
    list_to_binary(DateList).
