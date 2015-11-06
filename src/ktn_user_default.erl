-module(ktn_user_default).
-export([
         xref/0
         ,l_all/1
        , cmd/1
        , all_modules/0
        , mk/0
        ]).

-spec xref() -> proplists:proplist().
xref() ->
    xref:d("ebin").

-spec l_all([module()]) -> [{module(), code:load_ret()}].
l_all(Mods) when is_list(Mods) ->
  lists:foldl(fun(Mod, A) ->
                  Ret=[{Mod, shell_default:l(Mod)}],
                  lists:append(A, Ret)
              end, [], Mods);
l_all(Mod) ->
  l_all([Mod]).

-spec cmd(iodata()) -> _.
cmd(Cmd) ->
    io:format("~s~n", [os:cmd(Cmd)]).

-spec all_modules() -> [module()].
all_modules() ->
  [ list_to_atom(
      re:replace(
        filename:basename(F), "[.]beam$", "", [{return, list}]))
    ||  P <- code:get_path(),
        string:str(P, code:lib_dir()) == 0,
        F <- filelib:wildcard(filename:join(P, "*.beam"))].

-spec mk() -> up_to_date.
mk() -> up_to_date = make:all([load]).
