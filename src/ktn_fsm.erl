%%% @doc "Nicer" interfact to gen_fsm.
%%% With this module you can:
%%%   - use ktn_fsm:call instead of gen_fsm:sync_send_event
%%%   - use ktn_fsm:call_through instead of gen_fsm:sync_send_all_state_event
%%%   - use ktn_fsm:cast instead of gen_fsm:send_event
%%%   - use ktn_fsm:cast_through instead of gen_fsm:sync_send_event
%%%   - use ktn_fsm:state to retrieve the current gen_fsm state
%%% @todo implement the inner functional changes:
%%%   - leave some of the callbacks undefined. If a callback function is
%%%     undefined, the caller will throw an exception, but the fsm will keep
%%%     looping. This way you don't have to have catch-all clauses there.
-module(ktn_fsm).

-export(
  [ state/1
  , call/2
  , call/3
  , call_through/2
  , call_through/3
  , cast/2
  , cast_through/2
  ]).

-export(['$handle_undefined_function'/2]).

-spec state(sys:name()) -> atom().
state(Fsm) ->
  {StateName, _State} = sys:get_state(Fsm),
  StateName.

-spec call(sys:name(), term()) -> term().
call(Fsm, Call) ->
  gen_fsm:sync_send_event(Fsm, Call).

-spec call(sys:name(), term(), timeout()) -> term().
call(Fsm, Call, Timeout) ->
  gen_fsm:sync_send_event(Fsm, Call, Timeout).

-spec call_through(sys:name(), term()) -> term().
call_through(Fsm, Call) ->
  gen_fsm:sync_send_all_state_event(Fsm, Call).

-spec call_through(sys:name(), term(), timeout()) -> term().
call_through(Fsm, Call, Timeout) ->
  gen_fsm:sync_send_all_state_event(Fsm, Call, Timeout).

-spec cast(sys:name(), term()) -> ok.
cast(Fsm, Cast) ->
  gen_fsm:send_event(Fsm, Cast).

-spec cast_through(sys:name(), term()) -> ok.
cast_through(Fsm, Cast) ->
  gen_fsm:send_all_state_event(Fsm, Cast).

-spec '$handle_undefined_function'(atom(), [any()]) -> any().
'$handle_undefined_function'(Func, Args) ->
  {module, gen_fsm} =
    case code:is_loaded(gen_fsm) of
      false -> code:load_file(gen_fsm);
      _ -> {module, gen_fsm}
    end,
  case erlang:function_exported(gen_fsm, Func, length(Args)) of
    true -> erlang:apply(gen_fsm, Func, Args);
    false -> error_handler:raise_undef_exception(?MODULE, Func, Args)
  end.
