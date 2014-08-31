%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_recipe: a tool to structure code that consists of sequential steps
%%%             in which decisions are made.
%%%
%%% * What is a recipe?
%%%
%%% Recipe (noun): A set of conditions and parameters of an industrial process
%%% to obtain a given result.
%%%
%%% A recipe is a series of steps to obtain a result. This word was chosen
%%% because 'procedure' is overloaded in computer sciences, and it is not
%%% exactly a finite state machine.
%%%
%%% ktn_recipe arose from the need to restructure code implementing large
%%% application business logic decision trees.
%%% Long functions, deeply nested case expressions, code with several
%%% responsibilities, all make for unreadable and unmaintainable code.
%%% Every time one needs to make a change or add a feature, one must think about
%%% the non-obvious data flow, complex logic, possible side effects.
%%% Exceptions pile upon exceptions, until the code is a house of cards.
%%% If one is lucky (or responsible), one has comprehensive test suites covering
%%% all cases.
%%%
%%% A better way to structure the code would be preferable, one that makes the
%%% flow obvious and separates responsibilities into appropriate code segments.
%%% One way is a finite state machine. OTP even has a behaviour for exactly this
%%% purpose. However, gen_fsm does not exactly fit our needs:
%%% To begin with, gen_fsms run in their own process, which may not be what is
%%% needed. Second, logic flow is not immediately obvious because the fsm's
%%% state depends on the result of each state function. Finally, a gen_fsm
%%% transitions only on receiving input, whereas we are looking for something
%%% that runs like normal code, "on its own". So, our fsm will be defined by
%%% something like a transition table, a single place you can look at and know
%%% how it executes. This specification will "drive" the recipe.
%%%
%%% The most common case envisioned is a sequential series of steps, each of
%%% which makes a decision affecting later outcomes, so our design should be
%%% optimized for a single, linear execution flow, allowing for writing the
%%% minimum amount of code for this particular case. Keep the common case fast.
%%%
%%% * How do I use it?
%%%
%%% The simplest use case: create a module using the ktn_recipe behaviour.
%%% It must export transitions/0, process_result/1, process_error/1 and a series
%%% of unary functions called step functions.
%%% transitions/0 must return a list of atoms naming the step functions, in the
%%% order in which they must be called. Each step function takes a State
%%% variable as input and if the step was succesful emits {ok, NewState} with
%%% the updated state, or {error, NewState} if it was unsuccessful.
%%% After running all the steps, process_result will take the resulting state
%%% as input and should emit whatever you need.
%%% If one of the state functions returns {error, NewState}, then
%%% process_error/1 will be called taking the resulting state as input and
%%% should handle all expected error conditions.
%%%
%%% To run the recipe, call ktn_recipe:run(CALLBACK_MODULE, INITIAL_STATE).
%%%
%%% For more advanced uses, continue reading.
%%%
%%% * How does it work?
%%%
%%% The main functions are ktn_recipe:run/2-4. These will run a recipe and
%%% return the result.
%%%
%%% Recipes may be specified in two ways, which we call "implicit" and
%%% "explicit", for lack of better words. In implicit mode, we pass run/2 a
%%% callback module implementing the ktn_recipe behavior, which implements all
%%% necessary functions. In explicit mode, we explicitly give run/4 the
%%% transition table, a function to process the state resulting from running all
%%% recipe steps to completion, a function to process erroneous states, and the
%%% initial state of the recipe.
%%%
%%% * Step functions
%%%
%%% Step functions have the following type:
%%%
%%%   -type state()  :: term().
%%%   -type output() :: term().
%%%   -spec Step(state()) -> {ok, state()}
%%%                        | {output(), state()}
%%%                        | {error, state()}
%%%                        | {halt, state()}.
%%%
%%% The recipe state may be any value you need, a list, a proplist, a record, a
%%% map, etc. ktn_recipe does not use maps itself, although the test suite does.
%%%
%%% The initial state passed to run/2-4 will be passed to the first step as-is,
%%% and the subsequent resulting states will be fed to each step.
%%%
%%% The step functions should, if possible, have no side effects. If so, and the
%%% recipe ends in an error state, it can be aborted without worrying about
%%% rolling back the side effects.
%%%
%%% For example, if the recipe involves making changes in a database, these
%%% should be made in the process_result/1 function, once it is certain that
%%% all steps completed successfully.
%%%
%%% * Result and Error processing
%%%
%%% In implicit mode, the module must export process_error/1 and
%%% process_result/1; in explicit mode you may pass whichever function you
%%% desire.
%%% The purpose of the 'result processing function' is to transform the
%%% resulting state into a usable value.
%%% The purpose of the 'error processing function' is to extract the error value
%%% from the state and do something according. If you are unfortunate enough
%%% to have unavoidable side effects in your step functions, you may undo them
%%% here.
%%%
%%% * Specifying transitions
%%%
%%% Transition tables are lists.
%%% As stated, the simplest transition table is a list of atoms naming the step
%%% functions, but this is not the only way recipe states may be specified.
%%% A transition table is a list of transitions.
%%% What is permissible as a transition depends on whether we are running in
%%% implicit or explicit mode.
%%%
%%% In explicit mode, a transition may be either be an external function, i.e.
%%% fun module:function/arity, or a ternary tuple {F1, I, F2} in which the F1
%%% and F2 elements are explicit functions and the I is the transition input.
%%% This form reprensents a transition from step F1 to step F2, if F1 outputs
%%% {Input, State} instead of {ok, State}.
%%%
%%% In implicit mode, in addition to the se two forms of specifying step
%%% functions, an atom may be used. The module in which the function resides is
%%% assumed to be the provided callback module, hence it is 'implicit'.
%%%
%%% Also as stated, in any mode, the return value of step functions must be one
%%% of:
%%% - {ok, State}
%%% - {Output, State}
%%% - {halt, State}
%%% - {error, State}
%%%
%%% If {error, State} is returned, run/2-4 will call the error processing
%%% function. If {halt, State} is called, run/2-4 will call the result
%%% processing function.
%%% If {ok, State} is returned, run will call the next function in the
%%% transition table. That is, it will search the transition table until it
%%% finds the current's functions position, if necessary skip over entries
%%% corresponding to the current function, and call the first function with a
%%% different name it encounters.
%%% If {Output, State} is returned, run will search the transition table for an
%%% entry matching {Step, Output, NextStep, and select NextStep as the function
%%% to call. In this way, non-linear recipes that jump ahead or loop back may
%%% be specified. It is recommended that the transition table describe a DAG,
%%% unless the program logic really requires loops.
%%%
%%% Before running a recipe, run/2-4 will 'normalize' the transition table to
%%% remove implicit assumptions and so that it is composed only of ternary
%%% tuples explicitly listing every transition. As a debugging aid, this
%%% function normalize/1 is exported by the ktn_recipe module.
%%%
%%% Example
%%%
%%% Suppose you have a web server with an endpoint /conversations, accepting the
%%% DELETE method to delete conversations between users. To delete a
%%% conversation, one must fetch the converation entity from the database, fetch
%%% the contact (the other user in the conversation) specified by the user id
%%% in the conversation's contact_id attribute, check the contact's status (if
%%% it is blocked, etc) and also check whether the client is using version 1 of
%%% our REST API or version 2, since one really deletes the conversation from
%%% the database and the other merely clears messages from the conversation.
%%%
%%% We could implement this as a recipe with four steps:
%%%
%%% transitions() ->
%%%   [ get_conversation
%%%   , get_contact
%%%   , get_status
%%%   , check_api_version
%%%   ].
%%%
%%% get_conversation fetches it from the database. If it has already been
%%% deleted, it could return {error, NewState} and store the error in the state.
%%% Likewise for get_contact, and get_status.
%%% get_api_version would extract the api version from the request header or
%%% url, and store the result in the recipe state. If all steps complete
%%% successfully, process_result will effectively make the call to delete the
%%% conversation from the database.
%%%
%%% What if something goes wrong?
%%%
%%% The function ktn_recipe:verify/1 takes either a recipe module or an explicit
%%% transition table and will run several checks to verify that it will run
%%% correctly. You may use this function to test your transition tables.
%%% Note that verify/1 is implemented as a ktn_recipe, so you can use it as an
%%% example.
%%%
%%% If a step throws an exception, it will not be caught. That means you may see
%%% some of ktn_recipe's internal functions in the stacktrace. In general, the
%%% most common errors will be:
%%% 1) badly formed transition tables
%%% 2) not exporting step functions
%%% 3) bad return values from step functions
%%% 4) bad state values set by functions
%%%
%%% 1) and 2) should be detected by running verify on your recipe. 3) and 4)
%%% will be detected at run time and an appropriate error will be returned by
%%% run/2-4. Of course, your tests should detect these cases. You do have
%%% tests, right?
%%%
%%% Finally, there is one more function, ktn_recipe:pretty_print/1, which takes
%%% either a callback module or a transition table, and prints the normalized
%%% transition table.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ktn_recipe).
-author('igarai@gmail.com').

-export(
  [ run/2
  , run/4
  , pretty_print/1
  , verify/1
  , normalize/1
  ]).

-type output()                 :: ok | error | halt | term().
-type step_fun()               :: fun ((term()) -> {output(), term()}).
-type transition()             :: {step_fun(), output(), step_fun()}.
-type step()                   :: atom() | transition() | step_fun().
-type transitions()            :: [step()].
-type normalized_transitions() :: [transition()].

-callback transitions() -> transitions().
-callback process_result(term()) -> term().
-callback process_error(term()) -> term().

-spec run(atom(), term()) -> term().
run(Mod, InitialState) when is_atom(Mod) ->
  NormalizedTransitions = normalize(Mod),
  InitialFun            = initial_fun(NormalizedTransitions),
  ResultFun             = fun Mod:process_result/1,
  ErrorFun              = fun Mod:process_error/1,
  run(NormalizedTransitions, InitialFun, ResultFun, ErrorFun, InitialState).

-spec run(transitions(), step_fun(), step_fun(), term()) -> term().
run(Transitions, ResultFun, ErrorFun, InitialState) ->
  NormalizedTransitions = normalize(Transitions),
  InitialFun            = initial_fun(Transitions),
  run(NormalizedTransitions, InitialFun, ResultFun, ErrorFun, InitialState).

-spec run(normalized_transitions(), output(), step_fun(), step_fun(), term()) ->
  term().
run(_Transitions, error, _ResultFun, ErrorFun, State) ->
  ErrorFun(State);
run(_Transitions, halt, ResultFun, _ErrorFun, State) ->
  ResultFun(State);
run(Transitions, StepFun, ResultFun, ErrorFun, State) ->
  case StepFun(State) of
    {halt, NewState} ->
      ResultFun(NewState);
    {error, NewState} ->
      ErrorFun(NewState);
    {Output, NewState} ->
      NextStep = next_step(StepFun, Output, Transitions),
      run(Transitions, NextStep, ResultFun, ErrorFun, NewState);
    BadReturnValue ->
      Throw =
        [ {value,       BadReturnValue}
        , {step,        StepFun}
        , {transitions, Transitions}
        , {state,       State}
        ],
      throw({bad_step_return_value, Throw})
  end.

-spec initial_fun(normalized_transitions()) -> step_fun().
initial_fun(Transitions) ->
  case hd(Transitions) of
    {InitialFun, _, _} -> InitialFun;
    InitialFun         -> InitialFun
  end.

-spec normalize(atom() | transitions()) -> normalized_transitions().
normalize(Mod) when is_atom(Mod) ->
  Transitions = Mod:transitions(),
  [ case Transition of
      Step when is_atom(Step) ->
        {fun Mod:Step/1, ok, next(Mod, Step, Transitions)};
      StepFun when is_function(StepFun, 1) ->
        {StepFun, ok, next(Mod, StepFun, Transitions)};
      {StepFun1, Input, StepFun2} ->
        {normalize_step(Mod, StepFun1), Input, normalize_step(Mod, StepFun2)};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ];
normalize(Transitions) when is_list(Transitions) ->
  [ case Transition of
      StepFun when
        is_function(StepFun, 1) ->
          {StepFun, ok, next(StepFun, Transitions)};
      {StepFun1, Input, StepFun2} when
        is_function(StepFun1, 1),
        is_function(StepFun2, 1) ->
          {StepFun1, Input, StepFun2};
      {StepFun1, Input, Action} when
        Action == error; Action == halt ->
          {StepFun1, Input, Action};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ].

-spec normalize_step(atom(), halt | error | step()) ->
  halt | error | step_fun().
normalize_step(_Mod, halt) ->
  halt;
normalize_step(_Mod, error) ->
  error;
normalize_step(Mod, Step) when is_atom(Mod), is_atom(Step) ->
  fun Mod:Step/1;
normalize_step(_Mod, StepFun) when is_function(StepFun, 1) ->
  StepFun;
normalize_step(_Mod, Step) ->
  throw({normalization_error, bad_step, Step}).

%% next_step/3 computes the next step from a given step and an input.
-spec next_step(step(), term(), normalized_transitions()) -> error | step().
next_step(_Step, _Input, [])                      -> error;
next_step(Step, Input, [{Step, Input, Next} | _]) -> Next;
next_step(Step, Input, [_ | Ts])                  -> next_step(Step, Input, Ts).

%% next/2-3 computes the implied next state in a transition table for states
%% which do not explicitly name their next state.
-spec next(atom(), step(), transitions()) -> error | halt | step_fun().
next(Mod, Step, Transitions) ->
  case next(Step, Transitions) of
    error                    -> error;
    halt                     -> halt;
    S when is_atom(S)        -> fun Mod:S/1;
    S when is_function(S, 1) -> S
  end.

-spec next(step(), normalized_transitions()) -> error | halt | step().
next(_, [])                          -> error;
next(X, [X])                         -> halt;
next(X, [{Y, _, _} | T]) when X /= Y -> next(X, T);
next(X, [Y | T])         when X /= Y -> next(X, T);
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
-spec pretty_print(atom() | transitions()) -> ok.
pretty_print(Mod) when is_atom(Mod) ->
  pretty_print(normalize(Mod));
pretty_print(Transitions) when is_list(Transitions) ->
  pretty_print_normalized(normalize(Transitions)).

-spec pretty_print_normalized(normalized_transitions()) -> ok.
pretty_print_normalized(NormalizedTransitions) ->
  PrintFun =
    fun
      ({SF1, I, Action}) when Action == error; Action == halt ->
        {module, M1} = erlang:fun_info(SF1, module),
        {name, F1}   = erlang:fun_info(SF1, name),
        io:format("~p:~p(~p) -> ~p~n", [M1, F1, I, Action]);
      ({SF1, I, SF2}) ->
        {module, M1} = erlang:fun_info(SF1, module),
        {module, M2} = erlang:fun_info(SF2, module),
        {name, F1}   = erlang:fun_info(SF1, name),
        {name, F2}   = erlang:fun_info(SF2, name),
        io:format("~p:~p(~p) -> ~p:~p~n", [M1, F1, I, M2, F2])
    end,
  lists:foreach(PrintFun, NormalizedTransitions).

%% Verifies that a procedure module's transition table meets certain minimal
%% criteria. Returns ok if the transition table is a list of either atoms
%% or ternary tuples, whose first and third elements are atoms, and that
%% all states in the transition table are exported from the module with the
%% correct arity.
%% It does not verify structural properties of the FSM defined by the transition
%% table (e.g. connected, acyclic, arboreal).
verify(Mod) when is_atom(Mod) ->
  InitialState = #{recipe_type => implicit, recipe => Mod},
  run(ktn_recipe_verify, InitialState);
verify(Transitions) when is_list(Transitions) ->
  InitialState = #{recipe_type => explicit, recipe => Transitions},
  run(ktn_recipe_verify, InitialState).
