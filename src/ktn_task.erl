%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_task: functions useful for managing asyncronous tasks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_task).

-export([
         wait_for/2,
         wait_for/4,
         wait_for_success/1,
         wait_for_success/3
        ]).

wait_for(Task, ExpectedAnswer) ->
    wait_for(Task, ExpectedAnswer, 200, 10).

wait_for(Task, ExpectedAnswer, SleepTime, Retries) ->
    wait_for_success(fun() ->
                             ExpectedAnswer = Task()
                     end, SleepTime, Retries).

wait_for_success(Task) ->
    wait_for_success(Task, 200, 10).

wait_for_success(Task, SleepTime, Retries) ->
    wait_for_success(Task, undefined, SleepTime, Retries).

wait_for_success(_Task, Exception, _SleepTime, 0) ->
    {error, {timeout, Exception}};
wait_for_success(Task, _Exception, SleepTime, Retries) ->
    try
        Task()
    catch
        _:NewException ->
            timer:sleep(SleepTime),
            wait_for_success(Task, NewException, SleepTime, Retries - 1)
    end.
