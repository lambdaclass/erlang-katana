%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_json: functions useful for processing & creating JSON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_json).

-export(
  [ json_date/1
  , json_datetime/1
  ]).

%% @doc Converts a date record into a binary representation of its data.
-spec json_date(ktn_date:date()) -> binary().
json_date({date, {Yi, Mi, Di}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  iolist_to_binary([Y, "-", M, "-", D, "T00:00:00.000000Z"]).

%% @doc Converts a datetime record into a binary representation of its data.
-spec json_datetime(ktn_date:datetime()) -> binary().
json_datetime({datetime, {{Yi, Mi, Di}, {Hi, Ni, Si}}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  H = integer_to_list(Hi),
  N = integer_to_list(Ni),
  S = integer_to_list(Si),
  iolist_to_binary([Y, "-", M, "-", D, "T", H, ":", N, ":", S, ".000000Z"]).
