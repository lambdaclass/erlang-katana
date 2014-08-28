-module(ktn_recipe_verify).

-behaviour(ktn_recipe).

%% Behaviour exports
-export(
  [ transitions/0
  , process_result/1
  , process_error/1
  ]).
%% Steps
-export(
  [ verify_exports/1
  , verify_normalizability/1
  , verify_transitions/1
  , verify_transition_exports/1
  ]).

transitions() ->
  [ verify_exports
  , verify_normalizability
  , verify_transitions
  , verify_transition_exports
  ].

verify_exports(State = #{recipe_type := implicit, recipe := Mod}) ->
  % Ensure that the module exported transitions/0, process_result/1 and
  % process_error/1.
  Exports             = proplists:get_value(exports, Mod:module_info()),
  TransitionsExported = lists:member({transitions, 0}, Exports),
  ResultFunExported   = lists:member({process_result, 1}, Exports),
  ErrorFunExported    = lists:member({process_error, 1}, Exports),
  case {TransitionsExported, ResultFunExported, ErrorFunExported} of
    {false, _, _}      -> {error, State#{ error => {not_exported, transitions}} };
    {_, false, _}      -> {error, State#{ error => {not_exported, process_result}} };
    {_, _, false}      -> {error, State#{ error => {not_exported, process_error}} };
    {true, true, true} -> {ok, State}
  end;
verify_exports(State = #{recipe_type := explicit}) ->
  % Nothing needs to be done here for explicit recipes.
  {ok, State}.

verify_normalizability(State = #{recipe := Recipe}) ->
  try
    {ok, State#{transitions => ktn_recipe:normalize(Recipe)}}
  catch
    _:NormalizationError ->
      {error, State#{error => NormalizationError}}
  end.

verify_transitions(State = #{recipe_type := implicit, transitions := Transitions}) ->
  F =
    fun
      (X, A) when
        is_atom(X) ->
          A;
      (F, A) when
        is_function(F, 1) ->
          A;
      ({X, _, Y}, A) when
        is_function(X, 1),
        is_function(Y, 1) ->
          A;
      ({F, _, Y}, A) when
        is_function(F),
        is_atom(Y) ->
          A;
      ({X, _, F}, A) when
        is_function(F, 1),
        is_atom(X) ->
          A;
      ({F1, _, F2}, A) when
        is_function(F1, 1),
        is_function(F2, 1) ->
          A;
      (X, A) ->
          [X | A]
    end,
  case verify_transitions(F, Transitions) of
    ok    -> {ok, State};
    Error -> {error, State#{error => Error}}
  end;
verify_transitions(State = #{recipe_type := explicit, transitions := Transitions}) ->
  F =
    fun
      (F, A) when
        is_function(F, 1) ->
          A;
      ({F, _, Action}, A) when
        is_function(F, 1),
        (Action == error orelse Action == halt) ->
          A;
      ({F1, _, F2}, A) when
        is_function(F1, 1),
        is_function(F2, 1) ->
          A;
      (X, A) ->
          [X | A]
    end,
  case verify_transitions(F, Transitions) of
    ok    -> {ok, State};
    Error -> {error, State#{error => Error}}
  end.

verify_transitions(F, Transitions) ->
  case lists:foldl(F, [], Transitions) of
    []              -> ok;
    InvalidElements -> {invalid_transition_table_elements, InvalidElements}
  end.

verify_transition_exports(State = #{transitions := Transitions}) ->
  % Gather all the step functions mentioned in the transition table...
  % ...and ensure that they are exported.
  F =
    fun
      ({StepFun, _, Action}, A) when Action == halt; Action == error ->
        {module, M} = erlang:fun_info(StepFun, module),
        {name, F}   = erlang:fun_info(StepFun, name),
        Exported    = erlang:function_exported(M, F, 1),
        case Exported of
          true  -> A;
          false -> [StepFun | A]
        end;
      ({StepFun1, _, StepFun2}, A) ->
        {module, M1} = erlang:fun_info(StepFun1, module),
        {module, M2} = erlang:fun_info(StepFun2, module),
        {name, F1}   = erlang:fun_info(StepFun1, name),
        {name, F2}   = erlang:fun_info(StepFun2, name),
        Exported1    = erlang:function_exported(M1, F1, 1),
        Exported2    = erlang:function_exported(M2, F2, 1),
        case {Exported1, Exported2} of
          {true,  true}  -> A;
          {true,  false} -> [StepFun1 | A];
          {false, true}  -> [StepFun2 | A];
          {false, false} -> [StepFun1, StepFun2 | A]
        end
    end,
  case lists:foldl(F, [], Transitions) of
    []          -> {ok, State};
    NotExported -> {error, State#{error => {not_exported, NotExported}}}
  end.

process_result(_State) ->
  ok.

process_error(#{error := Error}) ->
  {error, Error}.
