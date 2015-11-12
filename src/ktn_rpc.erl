%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_rpc: functions useful for RPC mechanisms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_rpc).

-export(
  [ multicall/3
  ]).

%% @doc runs rpc:multicall(M, F, A) and emits warnigns for errors,
%%      but returns 'ok'.
-spec multicall(module(), atom(), [term()]) ->
  ok.
multicall(M, F, A) ->
  {Results, BadNodes} = rpc:multicall(M, F, A),
  Errors = [Error || {badrpc, Error} <- Results],
  case {Errors, BadNodes} of
    {[], []}       -> ok;
    {[], BadNodes} ->
        {error, {bad_nodes, [{mfa, {M, F, A}}, {nodes, BadNodes}]}};
    {Errors, _}    -> {error, {unknown, [{mfa, {M, F, A}}, {errors, Errors}]}}
  end.
