-module(ktn_user_default).
-export([
         xref/0
         ,l_all/1
        ]).

xref() ->
    xref:d("ebin").

l_all(Mods) when is_list(Mods) ->
  lists:foldl(fun(Mod, A) ->
                  Ret=[{Mod, shell_default:l(Mod)}],
                  lists:append(A, Ret)
              end, [], Mods);
l_all(Mod) ->
  l_all([Mod]).
