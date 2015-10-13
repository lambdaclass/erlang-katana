-module(ktn_fsm_SUITE).

-export([all/0]).

-export([ secure_vault/1
        ]).

-type config() :: [{atom(), term()}].

all() -> [secure_vault].

-spec secure_vault(config()) -> {comment, []}.
secure_vault(_Config) ->
  ct:comment("We start the vault with a master password and 3 attempts limit"),
  {ok, V} = secure_vault:start(masterpwd, 3),

  ct:comment("While it's unlocked, we can push, pop and check contents"),
  unlocked = secure_vault:state(V),
  [] = secure_vault:contents(V),
  0 = secure_vault:count(V),
  ok = secure_vault:push(V, a),
  ok = secure_vault:push(V, b),
  [b, a] = secure_vault:contents(V),
  2 = secure_vault:count(V),
  b = secure_vault:pop(V),
  [a] = secure_vault:contents(V),
  1 = secure_vault:count(V),
  V ! ignored,

  ct:comment("We lock it with password 'pwd'"),
  ok = secure_vault:lock(V, pwd),
  locked = secure_vault:state(V),
  V ! ignored,

  ct:comment("While it's locked we can't do anything besides count & unlock"),
  1 = secure_vault:count(V),
  badstate = fails(fun() -> secure_vault:lock(V, pwd2) end),
  badstate = fails(fun() -> secure_vault:lock(V, pwd) end),
  badstate = fails(fun() -> secure_vault:pop(V) end),
  ok = secure_vault:push(V, notpushed),
  1 = secure_vault:count(V),
  badstate = fails(fun() -> secure_vault:contents(V) end),
  V ! ignored,

  ct:comment("We try to unlock it with a wrong password"),
  badpwd = fails(fun() -> secure_vault:unlock(V, notpwd) end),
  locked = secure_vault:state(V),
  V ! ignored,

  ct:comment("We unlock it with the right password"),
  ok = secure_vault:unlock(V, pwd),
  V ! ignored,

  ct:comment("While it's unlocked, we can push, pop and check contents again"),
  [a] = secure_vault:contents(V),
  1 = secure_vault:count(V),
  a = secure_vault:pop(V),
  [] = secure_vault:contents(V),
  0 = secure_vault:count(V),
  ok = secure_vault:push(V, x),
  V ! ignored,

  ct:comment("While it's unlocked, we can't unlock it again"),
  badstate = fails(fun() -> secure_vault:unlock(V, pwd2) end),
  badstate = fails(fun() -> secure_vault:unlock(V, pwd) end),

  ct:comment("We lock it again"),
  ok = secure_vault:lock(V, pwd3),

  ct:comment("After 3 invalid pwd attempts, the vault is closed"),
  badpwd = fails(fun() -> secure_vault:unlock(V, notpwd3) end),
  badpwd = fails(fun() -> secure_vault:unlock(V, notpwd3) end),
  closed = fails(fun() -> secure_vault:unlock(V, notpwd3) end),
  closed = secure_vault:state(V),

  ct:comment("When a vault is closed, there is nothing we can do..."),
  badstate = fails(fun() -> secure_vault:unlock(V, pwd3) end),
  badstate = fails(fun() -> secure_vault:lock(V, pwd3) end),
  badstate = fails(fun() -> secure_vault:lock(V, pwd) end),
  badstate = fails(fun() -> secure_vault:pop(V) end),
  ok = secure_vault:push(V, notpushed),
  1 = secure_vault:count(V),
  badstate = fails(fun() -> secure_vault:contents(V) end),

  ct:comment("...except stopping it with the master password"),
  1 = secure_vault:count(V),
  ok = secure_vault:stop(V, notmasterpwd),
  1 = secure_vault:count(V),
  badstate = fails(fun() -> secure_vault:contents(V) end),
  V ! ignored,

  ok = secure_vault:stop(V, masterpwd),
  noproc =
    ktn_task:wait_for(
      fun() ->
        fails(fun() -> secure_vault:contents(V) end)
      end, noproc, 500, 10),

  {comment, ""}.

fails(Fun) ->
  try Fun() of
    Response ->
      ct:fail("Unexpected response from ~p: ~p", [Fun, Response])
  catch
    _:{noproc, _} -> noproc;
    _:Error -> Error
  end.

