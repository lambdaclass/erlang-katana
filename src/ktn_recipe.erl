-module(ktn_recipe).
-author('igarai@gmail.com').

% TODO:
% * [x] implement ktn_recipe:verify/1 as a debugging aid.
% * [x] implement ktn_recipe_SUITE.
%   - module with atom funs
%   - module with {M,F}
%   - module with {atom(),I,atom()}
%   - module with {atom(),I,{M,F}}
%   - module with {{M,F},I,atom()}
%   - module with {{M,F},I,{M,F1}}
%   - explicit transition table with implicit {M,F}
%   - explicit transition table with explicit {S1,I,S2}
% * [ ] rewrite because you are an ignoramus ({M,F} -> fun mod:fun/ari)
% * [ ] implement full example
% * [ ] document
% * [ ] reimplement verify as a ktn_recipe

-export(
  [ run/2
  , run/4
  , pretty_print/1
  , verify/1
  , normalize/1
  ]).

-type output()                 :: ok | error | halt | term().
-type mod_fun()                :: {atom(), atom()}.
-type transition()             :: {mod_fun(), output(), mod_fun()}.
-type step()                   :: atom() | mod_fun() | transition().
-type transitions()            :: [step()].
-type normalized_transitions() :: [transition()].

-spec run(atom(), term()) -> term().
run(Mod, InitialState) when is_atom(Mod) ->
  NormalizedTransitions = normalize(Mod),
  InitialFun            = initial_fun(NormalizedTransitions),
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
      run(Transitions, next_step(StepFun, Output, Transitions), ResultFun, ErrorFun, NewState);
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
        {{Mod, Step}, ok, next(Mod, Step, Transitions)};
      {StepMod, StepFun} when is_atom(StepMod), is_atom(StepFun) ->
        {{StepMod, StepFun}, ok, next(Mod, {StepMod, StepFun}, Transitions)};
      {Step1, Input, Step2} ->
        {normalize_step(Mod, Step1), Input, normalize_step(Mod, Step2)};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ];
normalize(Transitions) when is_list(Transitions) ->
  [ case Transition of
      {StepMod, StepFun} when is_atom(StepMod), is_atom(StepFun) ->
        {{StepMod, StepFun}, ok, next({StepMod, StepFun}, Transitions)};
      {{Step1Mod, Step1Fun}, Input, {Step2Mod, Step2Fun}} ->
        {{Step1Mod, Step1Fun}, Input, {Step2Mod, Step2Fun}};
      {{Step1Mod, Step1Fun}, Input, Action} when Action == error; Action == halt ->
        {{Step1Mod, Step1Fun}, Input, Action};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ].

normalize_step(_Mod, halt) ->
  halt;
normalize_step(_Mod, error) ->
  error;
normalize_step(Mod, Step) when is_atom(Step) ->
  {Mod, Step};
normalize_step(_Mod, {StepMod, StepFun}) ->
  {StepMod, StepFun};
normalize_step(_Mod, Step) ->
  throw({normalization_error, bad_step, Step}).

%% next_step/3 computes the next step from a given step and an input.
next_step(_Step, _Input, [])                      -> error;
next_step(Step, Input, [{Step, Input, Next} | _]) -> Next;
next_step(Step, Input, [_ | Ts])                  -> next_step(Step, Input, Ts).

%% next/2-3 computes the implied next state in a transition table for states
%% which do not explicitly name their next state.
next(Mod, Step, Transitions) ->
  case next(Step, Transitions) of
    error             -> error;
    halt              -> halt;
    S when is_atom(S) -> {Mod, S};
    {M, F}            -> {M, F}
  end.

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
  pretty_print(normalize(Mod));
pretty_print(Transitions) when is_list(Transitions) ->
  pretty_print_normalized(normalize(Transitions)).

pretty_print_normalized(NormalizedTransitions) ->
  PrintFun =
    fun
      ({{SM1, SF1}, I, {SM2, SF2}}) ->
        io:format("~p:~p(~p) -> ~p:~p~n", [SM1, SF1, I, SM2, SF2]);
      ({{SM1, SF1}, I, Action}) when Action == error; Action == halt ->
        io:format("~p:~p(~p) -> ~p~n", [SM1, SF1, I, Action])
    end,
  lists:foreach(PrintFun, NormalizedTransitions).

%% Verifies that a procedure module's transition table meets certain minimal
%% criteria. Returns ok if the transition table is a list of either atoms
%% or ternary tuples, whose first and third elements are atoms, and that
%% all states in the transition table are exported from the module with the
%% correct arity.
%% It does not verify structural properties of the FSM defined by the transition
%% table (e.g. connected, acyclic, arboreal).
-spec verify(atom()) -> ok | term().
verify(Mod) when is_atom(Mod) ->
  case verify_exports(Mod) of
    ok ->
      case verify_normalizability(Mod) of
        {ok, NormalizedTransitions} ->
          case verify_implicit_transitions(NormalizedTransitions) of
            ok ->
              verify_transition_exports(NormalizedTransitions);
            TransitionError ->
              TransitionError
          end;
        NormalizationError ->
          NormalizationError
      end;
    Error ->
      Error
  end;
verify(Transitions) when is_list(Transitions) ->
  case verify_normalizability(Transitions) of
    {ok, NormalizedTransitions} ->
      case verify_explicit_transitions(NormalizedTransitions) of
        ok ->
          verify_transition_exports(NormalizedTransitions);
        TransitionError ->
          TransitionError
      end;
    NormalizationError ->
      NormalizationError
  end.

verify_exports(Mod) when is_atom(Mod) ->
  % Ensure that the module exported transitions/0, process_result/1 and
  % process_error/1.
  Exports             = proplists:get_value(exports, Mod:module_info()),
  TransitionsExported = lists:member({transitions, 0}, Exports),
  ResultFunExported   = lists:member({process_result, 1}, Exports),
  ErrorFunExported    = lists:member({process_error, 1}, Exports),
  case {TransitionsExported, ResultFunExported, ErrorFunExported} of
    {false, _, _}      -> {not_exported, transitions};
    {_, false, _}      -> {not_exported, process_result};
    {_, _, false}      -> {not_exported, process_error};
    {true, true, true} -> ok
  end.

verify_normalizability(Recipe) when is_atom(Recipe); is_list(Recipe) ->
  try
    {ok, normalize(Recipe)}
  catch
    _:NormalizationError ->
      NormalizationError
  end;
verify_normalizability(_Transitions) ->
  {error, non_list_transition_table}.

verify_implicit_transitions(Transitions) ->
  F =
    fun
      (X, A) when
        is_atom(X) ->
          A;
      ({X, Y}, A) when
        is_atom(X),
        is_atom(Y) ->
          A;
      ({X, _, Y}, A) when
        is_atom(X),
        is_atom(Y) ->
          A;
      ({{M, F}, _, Y}, A) when
        is_atom(M),
        is_atom(F),
        is_atom(Y) ->
          A;
      ({X, _, {M, F}}, A) when
        is_atom(M),
        is_atom(F),
        is_atom(X) ->
          A;
      ({{M1, F1}, _, {M2, F2}}, A) when
        is_atom(M1),
        is_atom(F1),
        is_atom(M2),
        is_atom(F2) ->
          A;
      ({X, I, Y}, A) ->
          [{X, I, Y} | A];
      (X, A) ->
          [X | A]
    end,
  verify_transitions(F, Transitions).

verify_explicit_transitions(Transitions) ->
  F =
    fun
      ({X, Y}, A) when
        is_atom(X),
        is_atom(Y) ->
          A;
      ({{M1, F1}, _, Action}, A) when
        is_atom(M1),
        is_atom(F1),
        (Action == error orelse Action == halt) ->
          A;
      ({{M1, F1}, _, {M2, F2}}, A) when
        is_atom(M1),
        is_atom(F1),
        is_atom(M2),
        is_atom(F2) ->
          A;
      (X, A) ->
          [X | A]
    end,
  verify_transitions(F, Transitions).

verify_transitions(F, Transitions) ->
  case lists:foldl(F, [], Transitions) of
    []              -> ok;
    InvalidElements -> {invalid_transition_table_elements, InvalidElements}
  end.

verify_transition_exports(Transitions) ->
  % Gather all the step functions mentioned in the transition table...
  % ...and ensure that they are exported.
  F =
    fun
      ({{M1, F1}, _, {M2, F2}}, A) ->
        Exports1  = proplists:get_value(exports, M1:module_info()),
        Exports2  = proplists:get_value(exports, M2:module_info()),
        Exported1 = lists:member({F1, 1}, Exports1),
        Exported2 = lists:member({F2, 1}, Exports2),
        case {Exported1, Exported2} of
          {true,  true}  -> A;
          {true,  false} -> [{M1, F1} | A];
          {false, true}  -> [{M2, F2} | A];
          {false, false} -> [{M1, F1}, {M2, F2} | A]
        end;
      ({{M, F}, _, Action}, A) when Action == halt; Action == error ->
        Exports  = proplists:get_value(exports, M:module_info()),
        Exported = lists:member({F, 1}, Exports),
        case Exported of
          true  -> A;
          false -> [{M, F} | A]
        end
    end,
  case lists:foldl(F, [], Transitions) of
    []          -> ok;
    NotExported -> {not_exported, NotExported}
  end.
