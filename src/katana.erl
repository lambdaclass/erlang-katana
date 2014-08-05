-module(katana).

-export([
         wait_for/2,
         wait_for/4,
         now_human_readable/0
        ]).

wait_for(Task, Answer) ->
    wait_for(Task, Answer, 200, 10).

wait_for(_Task, _Answer, _SleepTime,  0) ->
    error(timeout_while_waiting);
wait_for(Task, Answer, SleepTime, Retries) ->
    case Task() of
        Answer ->
            ok;
        _ ->
            timer:sleep(SleepTime),
            wait_for(Task, Answer, SleepTime, Retries - 1)
    end.

now_human_readable() ->
    TimeStamp = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} = calendar:now_to_universal_time(TimeStamp),
    DateList = io_lib:format("~p-~p-~pT~p:~p:~pZ",
                             [Year, Month, Day, Hour, Minute, Second]),
    list_to_binary(DateList).
