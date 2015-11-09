%%% @doc Meta Testing SUITE
%%% Use with mixer or by yourself. Just include a call to each of its functions
%%% in your common test suites.
%%% Make sure to add an application property to your common test configuration.
-module(ktn_meta_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([xref/1, dialyzer/1, elvis/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [dialyzer | elvis | xref].
all() -> [dialyzer, elvis, xref].

%% @doc xref's your code using xref_runner
-spec xref(config()) -> {comment, []}.
xref(Config) ->
  BaseDir = base_dir(Config),
  Dirs = [ filename:join(BaseDir, "ebin")
         , filename:join(BaseDir, "test")
         ],

  ct:comment("Undefined Function Calls"),
  UFCs = xref_runner:check(undefined_function_calls, #{dirs => Dirs}),

  ct:comment("Undefined Functions"),
  UFs = xref_runner:check(undefined_functions, #{dirs => Dirs}),

  ct:comment("Locals not Used"),
  LNUs = xref_runner:check(locals_not_used, #{dirs => Dirs}),

  ct:comment("Deprecated Function Calls"),
  DFCs = xref_runner:check(deprecated_function_calls, #{dirs => Dirs}),

  ct:comment("Deprecated Functions"),
  DFs = xref_runner:check(deprecated_functions, #{dirs => Dirs}),

  ct:comment("There are no Warnings"),
  [] = UFCs ++ UFs ++ LNUs ++ DFCs ++ DFs,

  {comment, ""}.

%% @doc dialyzes your code.
%%      By default it uses all the plts in the project root folder.
%%      You can change that by providing a 'plts' parameter in Config.
%%      You can also change the warnings using the 'dialyzer_warnings' parameter
-spec dialyzer(config()) -> {comment, []}.
dialyzer(Config) ->
  BaseDir = base_dir(Config),
  Plts = plts(Config),
  Dirs = [ filename:join(BaseDir, "ebin")
         , filename:join(BaseDir, "test")
         ],
  Warnings =
    case test_server:lookup_config(dialyzer_warnings, Config) of
      undefined -> [error_handling, race_conditions, unmatched_returns];
      Ws -> Ws
    end,

  ct:comment("Dialyzer must emit no warnings"),
  Opts =
    [ {analysis_type, succ_typings}
    , {plts,          Plts}
    , {files_rec,     Dirs}
    , {check_plt,     true}
    , {warnings,      Warnings}
    , {get_warnings,  true}
    ],
  [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
  {comment, ""}.

%% @doc Checks your code with elvis
-spec elvis(config()) -> {comment, []}.
elvis(_Config) ->
  ct:fail("~p", [elvis:rock()]),
  {comment, ""}.

base_dir(Config) ->
  case test_server:lookup_config(application, Config) of
    undefined -> ct:fail("Missing application in Config: ~p", [Config]);
    App -> code:lib_dir(App)
  end.

plts(Config) ->
  case test_server:lookup_config(plts, Config) of
    undefined ->
      Wildcard = filename:join(base_dir(Config), "*.plt"),
      case filelib:wildcard(Wildcard) of
        [] ->
          ct:fail("No plts at ~s - you need to at least have one", [Wildcard]);
        Plts -> Plts
      end;
    Plts -> Plts
  end.
