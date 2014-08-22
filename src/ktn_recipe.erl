-module(ktn_recipe).
-author('igarai@gmail.com').

-export(
  [ run/2
  , run/4
  , pretty_print/1
  % , verify/1
  ]).

-type output()                 :: ok | error | halt | term().
-type mod_fun()                :: {atom(), atom()}.
-type transition()             :: {mod_fun(), output(), mod_fun()}.
-type step()                   :: atom() | mod_fun() | transition().
-type transitions()            :: [step()].
-type normalized_transitions() :: [transition()].

-spec run(atom(), term()) -> term().
run(Mod, InitialState) when is_atom(Mod) ->
  NormalizedTransitions = normalize(Mod:transitions()),
  InitialFun            = initial_fun(Mod:transitions()),
  ResultFun             = {Mod, process_result},
  ErrorFun              = {Mod, process_error},
  run(NormalizedTransitions, InitialFun, ResultFun, ErrorFun, InitialState).

-spec run(transitions(), mod_fun(), mod_fun(), term()) -> term().
run(Transitions, ResultFun, ErrorFun, InitialState) ->
  NormalizedTransitions = normalize(Transitions),
  InitialFun            = initial_fun(Transitions),
  run(NormalizedTransitions, InitialFun, ResultFun, ErrorFun, InitialState).

-spec run(normalized_transitions(), output(), mod_fun(), mod_fun(), term()) -> term().
run(_Transitions, error, _ResultFun, {M, F}, State) ->
  M:F(State);
run(_Transitions, halt, {M, F}, _ErrorFun, State) ->
  M:F(State);
run(Transitions, StepFun, ResultFun, ErrorFun, State) ->
  {SM, SF} = StepFun,
  {RM, RF} = ResultFun,
  {EM, EF} = ErrorFun,
  case SM:SF(State) of
    {error, NewState} ->
      EM:EF(NewState);
    {halt, NewState} ->
      RM:RF(NewState);
    {Output, NewState} ->
      run(Transitions, next(StepFun, Output, Transitions), ResultFun, ErrorFun, NewState);
    BadReturnValue ->
      Throw =
        [ {value,       BadReturnValue}
        , {step,        StepFun}
        , {transitions, Transitions}
        , {state,       State}
        ],
      throw({bad_step_return_value, Throw})
  end.

initial_fun(Transitions) ->
  case hd(Transitions) of
    {InitialFun, _, _} -> InitialFun;
    InitialFun         -> InitialFun
  end.

normalize(Mod) when is_atom(Mod) ->
  Transitions = Mod:transitions(),
  [ case Transition of
      Step when is_atom(Step) ->
        {{Mod, Step}, ok, next({Mod, Step}, Transitions)};
      {StepMod, StepFun} when is_atom(StepMod) andalso is_atom(StepFun) ->
        {{StepMod, StepFun}, ok, next({StepMod, StepFun}, Transitions)};
      {Step1, Input, Step2} ->
        {normalize_step(Mod, Step1), Input, normalize_step(Mod, Step2)};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ];
normalize(Transitions) when is_list(Transitions) ->
  [ case Transition of
      {StepMod, StepFun} when is_atom(StepMod) andalso is_atom(StepFun) ->
        {{StepMod, StepFun}, ok, next({StepMod, StepFun}, Transitions)};
      {{Step1Mod, Step1Fun}, Input, {Step2Mod, Step2Fun}} ->
        {{Step1Mod, Step1Fun}, Input, {Step2Mod, Step2Fun}};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ].

normalize_step(Mod, Step) when is_atom(Step) ->
  {Mod, Step};
normalize_step(_Mod, {StepMod, StepFun}) ->
  {StepMod, StepFun};
normalize_step(_Mod, Step) ->
  throw({normalization_error, bad_step, Step}).

%% next/3 computes the next step from a given step and an input.
next(_Step, _Input, [])                      -> error;
next(Step, Input, [{Step, Input, Next} | _]) -> Next;
next(Step, Input, [_ | Ts])                  -> next(Step, Input, Ts).

%% next/2 and next2/2 compute the implied next state in a transition table for
%% states which do not explicitly name their next state.
next(_, [])                          -> error;
next(X, [X])                         -> halt;
next(X, [{Y, _, _} | T]) when X /= Y -> next(X, T);
next(X, [Y|T])           when X /= Y -> next(X, T);
next(X, [{X, _, _} | T])             -> next2(X, T);
next(X, [X | T])                     -> next2(X, T).

next2(_, [])                          -> error;
next2(X, [{X, _, _}])                 -> halt;
next2(X, [X])                         -> halt;
next2(X, [{X, _, _} | T])             -> next2(X, T);
next2(X, [X | T])                     -> next2(X, T);
next2(X, [{Y, _, _} | _]) when X /= Y -> Y;
next2(X, [Y | _])         when X /= Y -> Y.

%% Pretty prints a procedure module's transition table.
-spec pretty_print(atom()) -> ok.
pretty_print(Mod) when is_atom(Mod) ->
  pretty_print(Mod:transitions());
pretty_print(Transitions) when is_list(Transitions) ->
  PrintFun =
    fun ({{SM1, SF1}, I, {SM2, SF2}}) ->
      io:format("~p:~p(~p) -> ~p:~p~n", [SM1, SF1, I, SM2, SF2])
    end,
  NormalizedTransitions = normalize(Transitions),
  lists:foreach(PrintFun, NormalizedTransitions).
