-module(secure_vault).

-behaviour(gen_fsm).

-ignore_xref([{ktn_fsm, start, 3}]).

-export([ start/2
        , state/1
        , contents/1
        , count/1
        , push/2
        , pop/1
        , lock/2
        , unlock/2
        , stop/2
        ]).

-export([ init/1
        , handle_event/3
        , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4
        ]).

-export([ unlocked/3
        , unlocked/2
        , locked/3
        , locked/2
        , closed/3
        , closed/2
        ]).

-dialyzer({no_missing_calls, [start/2]}).

start(MasterPassword, MaxAttempts) ->
  ktn_fsm:start(
    ?MODULE,
    {MasterPassword, MaxAttempts},
    [{debug, [log, statistics, trace]}]).

state(Vault) ->
  ktn_fsm:state(Vault).

contents(Vault) ->
  call(Vault, contents).

count(Vault) ->
  call(Vault, count).

push(Vault, Content) ->
  ktn_fsm:cast(Vault, {push, Content}).

pop(Vault) ->
  call(Vault, pop).

lock(Vault, Password) ->
  call(Vault, {lock, Password}).

unlock(Vault, Password) ->
  call(Vault, {unlock, Password}).

stop(Vault, Password) ->
  ktn_fsm:cast_through(Vault, {stop, Password}).

init({MasterPassword, Attempts}) ->
  StateData =
    #{ masterpwd    => MasterPassword
     , max_attempts => Attempts
     , contents     => []
     },
  {ok, unlocked, StateData}.

unlocked({lock, Password}, _From, StateData) ->
  #{max_attempts := Attempts} = StateData,
  {reply, ok, locked, StateData#{password => Password, attempts => Attempts}};
unlocked(pop, _From, #{contents := []} = StateData) ->
  {reply, {ok, undefined}, unlocked, StateData};
unlocked(pop, _From, StateData) ->
  #{contents := [Content|Contents]} = StateData,
  {reply, {ok, Content}, unlocked, StateData#{contents := Contents}};
unlocked(contents, _From, StateData) ->
  #{contents := Contents} = StateData,
  {reply, {ok, Contents}, unlocked, StateData};
unlocked(_, _From, StateData) ->
  {reply, {error, badstate}, unlocked, StateData}.

unlocked({push, Content}, StateData) ->
  #{contents := Contents} = StateData,
  {next_state, unlocked, StateData#{contents := [Content|Contents]}}.

locked({unlock, Password}, _From, #{password := Password} = StateData) ->
  {reply, ok, unlocked, maps:remove(password, StateData)};
locked({unlock, _NotPassword}, _From, #{attempts := 1} = StateData) ->
  {reply, {error, closed}, closed, StateData};
locked({unlock, _NotPassword}, _From, StateData) ->
  #{attempts := Attempts} = StateData,
  {reply, {error, badpwd}, locked, StateData#{attempts := Attempts - 1}};
locked(_, _From, StateData) ->
  {reply, {error, badstate}, locked, StateData}.

locked(_, StateData) ->
  {next_state, locked, StateData}.

closed(_, _From, StateData) ->
  {reply, {error, badstate}, closed, StateData}.

closed(_, StateData) ->
  {next_state, closed, StateData}.

handle_sync_event(count, _From, StateName, StateData) ->
  #{contents := Contents} = StateData,
  {reply, {ok, length(Contents)}, StateName, StateData}.

handle_event({stop, Pwd}, _StateName, #{masterpwd := Pwd} = StateData) ->
  {stop, normal, StateData};
handle_event(_, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_info(_, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

call(Vault, count) -> call(Vault, count, fun ktn_fsm:call_through/2);
call(Vault, Call) -> call(Vault, Call, fun ktn_fsm:call/2).

call(Vault, Call, Fun) ->
  case Fun(Vault, Call) of
    ok -> ok;
    {ok, Result} -> Result;
    {error, Error} -> throw(Error)
  end.
