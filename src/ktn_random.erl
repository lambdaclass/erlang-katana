%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_random: a gen_server for generating random alfanumeric strings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_random).
-behaviour(gen_server).

-export([
         start_link/0,
         generate/0,
         generate/1,
         uniform/1,
         uniform/2,
         pick/1
        ]).

-export([
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-type state() :: {}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    Seed = os:timestamp(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Seed, []).

-spec generate() -> nonempty_string().
generate() ->
    gen_server:call(?MODULE, random_string).

-spec generate(pos_integer()) -> nonempty_string().
generate(Length) ->
    gen_server:call(?MODULE, {random_string, Length}).

-spec uniform(non_neg_integer()) -> non_neg_integer().
uniform(Max) ->
    gen_server:call(?MODULE, {random_uniform, Max}).

-spec uniform(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
uniform(Min, Max) ->
    gen_server:call(?MODULE, {random_uniform, Min, Max}).

%% @doc Randomly chooses one element from the list
-spec pick([X, ...]) -> X.
pick(List) -> lists:nth(uniform(length(List)), List).

%% Callback implementation
-spec init(erlang:timestamp()) -> {ok, state()}.
init(Seed) ->
    _ = random:seed(Seed),
    {ok, {}}.  % no state necessary, random uses process dictionary

-spec handle_call
    (random_string, _, state()) ->
        {reply, nonempty_string(), state()};
    ({random_string, pos_integer()}, _, state()) ->
        {reply, nonempty_string(), state()};
    ({random_uniform, non_neg_integer()}, _, state()) ->
        {reply, non_neg_integer(), state()};
    ({random_uniform, non_neg_integer(), non_neg_integer()}, _, state()) ->
        {reply, non_neg_integer(), state()}.
handle_call(random_string, _From, State) ->
    {reply, random_string(), State};
handle_call({random_string, Length}, _From, State) ->
    {reply, random_string(Length), State};
handle_call({random_uniform, Max}, _From, State) ->
    {reply, random_uniform(Max), State};
handle_call({random_uniform, Min, Max}, _From, State) ->
    {reply, random_uniform(Min, Max), State}.

%% Unused callbacks
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(_, state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% internal
random_string() ->
    Length = get_random_length(),
    random_string_cont(Length).

random_string(Length) ->
    random_string_cont(Length).

random_string_cont(Length) ->
    RandomAllowedChars = get_random_allowed_chars(),
    [  random_alphanumeric(RandomAllowedChars)
    || _N <- lists:seq(1, Length)
    ].

random_uniform(Max) when Max > 0->
    random:uniform(Max);
random_uniform(Max) ->
    {error, {invalid_value, Max}}.

random_uniform(Min, Max) when Max > Min ->
    Min + random:uniform(Max - Min + 1) - 1;
random_uniform(Min, Max) ->
    {error, {invalid_range, Min, Max}}.

%% internal
random_alphanumeric(AllowedChars) ->
    Length = erlang:length(AllowedChars),
    lists:nth(random:uniform(Length), AllowedChars).

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
