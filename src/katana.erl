-module(katana).

-export([
         wait_for/2,
         wait_for/4
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
