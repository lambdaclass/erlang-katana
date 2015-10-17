-module(ktn_user_default).
-export([
         xref/0
        ]).

xref() ->
    xref:d("ebin").
