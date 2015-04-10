-module(ktn_test_utils).

-export(
  [ assert_response/4
  , test_response/4
  ]).

-type test_subject() :: status | headers | body.
-type match_type()   :: exact | partial.
-type response()     :: #{status => term(), headers => term(), body => term()}.

-spec assert_response(test_subject(), match_type(), term(), response()) -> ok.
assert_response(Test, MatchType, Params, Response) ->
  ok = test_response(Test, MatchType, Params, Response).

-spec test_response(test_subject(), match_type(), term(), term()) ->
  ok | {error, term()}.
test_response(status, partial, [C, $?, $?], Response) ->
  Pattern = [C] ++ "[0-9][0-9]",
  test_response(status, partial, Pattern, Response);
test_response(status, partial, [C1, C2, $?], Response) ->
  Pattern = [C1, C2] ++ "[0-9]",
  test_response(status, partial, Pattern, Response);
test_response(status, partial, Pattern, Response) ->
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
test_response(status, exact, Status, Response) ->
  #{status := ResStatus} = Response,
  StatusStr    = ktn_strings:to_string(Status),
  ResStatusStr = ktn_strings:to_string(ResStatus),
  case ResStatusStr of
    StatusStr -> ok;
    _Other    -> {error, {nomatch, StatusStr, ResStatusStr}}
  end;
test_response(headers, partial, Headers, Response) ->
  #{headers := ResHeaders} = Response,
  HeadersNorm =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- Headers],
  ResHeadersNorm =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- ResHeaders],
  case HeadersNorm -- ResHeadersNorm of
    []             -> ok;
    MissingHeaders -> {error, {missing_headers, MissingHeaders, ResHeadersNorm}}
  end;
test_response(headers, exact, Headers, Response) ->
  #{headers := ResHeaders} = Response,
  HeadersNorm    =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- Headers],
  ResHeadersNorm =
    [{string:to_lower(X), string:to_lower(Y)} || {X, Y} <- ResHeaders],
  case {HeadersNorm -- ResHeadersNorm, ResHeadersNorm -- HeadersNorm} of
    {[], []} -> ok;
    _        -> {error, {nomatch, HeadersNorm, ResHeadersNorm}}
  end;
test_response(body, partial, Pattern, Response) ->
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
test_response(body, exact, Text, Response) ->
  #{body := ResBody} = Response,
  ResBodyStr = ktn_strings:to_string(ResBody),
  Body       = ktn_strings:to_string(Text),
  case ResBodyStr of
    Body -> ok;
    _    -> {error, {nomatch, ResBodyStr, Body}}
  end.
