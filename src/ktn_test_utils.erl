-module(ktn_test_utils).

-export(
  [ assert/4
  , test/4
  ]).

-type test_subject() :: status | headers | body.
-type match_type()   :: exact | partial.

-spec assert(test_subject(), match_type(), map(), chtc:response()) -> ok.
assert(Test, MatchType, Params, Response) ->
  ok = test(Test, MatchType, Params, Response).

-spec test(test_subject(), match_type(), term(), term()) ->
  ok | {error, term()}.
test(status, partial, [C, $?, $?], Response) ->
  Pattern = [C] ++ "[0-9][0-9]",
  test(status, partial, Pattern, Response);
test(status, partial, [C1, C2, $?], Response) ->
  Pattern = [C1, C2] ++ "[0-9]",
  test(status, partial, Pattern, Response);
test(status, partial, Pattern, Response) ->
  #{status := Status} = Response,
  ResStatus = ktn_strings:to_string(Status),
  case re:compile(Pattern) of
    {ok, MP} ->
      case re:run(ResStatus, MP, [global]) of
        match               -> ok;
        {match, [[{0, 3}]]} -> ok;
        nomatch             -> {error, {nomatch, Pattern, ResStatus}}
      end;
    {error, Error} ->
      {error, {regex_compile_fail, Error}}
  end;
test(status, exact, Status, Response) ->
  #{status := ResStatus} = Response,
  StatusStr    = ktn_strings:to_string(Status),
  ResStatusStr = ktn_strings:to_string(ResStatus),
  case ResStatusStr of
    StatusStr -> ok;
    _Other    -> {error, {nomatch, StatusStr, ResStatusStr}}
  end;
test(headers, partial, Headers, Response) ->
  #{headers := ResHeaders} = Response,
  HeadersNorm =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- Headers],
  ResHeadersNorm =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- ResHeaders],
  case HeadersNorm -- ResHeadersNorm of
    []             -> ok;
    MissingHeaders -> {error, {missing_headers, MissingHeaders, ResHeadersNorm}}
  end;
test(headers, exact, Headers, Response) ->
  #{headers := ResHeaders} = Response,
  HeadersNorm    =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- Headers],
  ResHeadersNorm =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- ResHeaders],
  case {HeadersNorm -- ResHeadersNorm, ResHeadersNorm -- HeadersNorm} of
    {[], []} -> ok;
    _        -> {error, {nomatch, HeadersNorm, ResHeadersNorm}}
  end;
test(body, partial, Pattern, Response) ->
  #{body := ResBody} = Response,
  ResBodyStr = ktn_strings:to_string(ResBody),
  case re:compile(Pattern) of
    {ok, MP} ->
      case re:run(ResBodyStr, MP) of
        match      -> ok;
        {match, _} -> ok;
        nomatch    -> {error, {nomatch, Pattern, ResBodyStr}}
      end;
    {error, Error} ->
      {error, {regex_compile_fail, Error}}
  end;
test(body, exact, Text, Response) ->
  #{body := ResBody} = Response,
  ResBodyStr = ktn_strings:to_string(ResBody),
  Body       = ktn_strings:to_string(Text),
  case ResBodyStr of
    Body -> ok;
    _    -> {error, {nomatch, ResBodyStr, Body}}
  end.

