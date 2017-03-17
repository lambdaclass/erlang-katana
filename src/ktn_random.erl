%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_random: a wrapper for generating random alfanumeric strings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_random).

-export([
         string/0,
         string/1,
         uniform/1,
         uniform/2,
         pick/1
        ]).

-spec string() -> nonempty_string().
string() ->
    Length = get_random_length(),
    string(Length).

-spec string(pos_integer()) -> nonempty_string().
string(Length) ->
    RandomAllowedChars = get_random_allowed_chars(),
    [  random_alphanumeric(RandomAllowedChars)
    || _N <- lists:seq(1, Length)
    ].

-spec uniform(term()) -> non_neg_integer() | {error, {invalid_value, term()}}.
uniform(Max) when Max > 0->
    rand:uniform(Max);
uniform(Max) ->
    {error, {invalid_value, Max}}.

-spec uniform(term(), term()) ->
    non_neg_integer() | {error, {invalid_range, term(), term()}}.
uniform(Min, Max) when Max > Min  ->
    Min + rand:uniform(Max - Min + 1) - 1;
uniform(Min, Max) ->
    {error, {invalid_range, Min, Max}}.

%% @doc Randomly chooses one element from the list
-spec pick([X, ...]) -> X.
pick(List) -> lists:nth(uniform(length(List)), List).

%% internal
random_alphanumeric(AllowedChars) ->
    Length = erlang:length(AllowedChars),
    lists:nth(rand:uniform(Length), AllowedChars).

get_random_length() ->
    case application:get_env(katana, random_length) of
        {ok, SecretLength} ->
            SecretLength;
        undefined ->
            16
    end.

get_random_allowed_chars() ->
    case application:get_env(katana, random_allowed_chars) of
        {ok, RandomAllowedChars} ->
            RandomAllowedChars;
        undefined ->
            "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end.
