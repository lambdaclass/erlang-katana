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
         uniform/2
        ]).

-export([
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

start_link() ->
    Seed = os:timestamp(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Seed, []).

generate() ->
    gen_server:call(?MODULE, random_string).

generate(Length) ->
    gen_server:call(?MODULE, {random_string, Length}).

uniform(Max) ->
    gen_server:call(?MODULE, {random_uniform, Max}).

uniform(Min, Max) ->
    gen_server:call(?MODULE, {random_uniform, Min, Max}).

%% Callback implementation
init(Seed) ->
    _ = random:seed(Seed),
    {ok, {}}.  % no state necessary, random uses process dictionary

handle_call(random_string, _From, State) ->
    {reply, random_string(), State};
handle_call({random_string, Length}, _From, State) ->
    {reply, random_string(Length), State};
handle_call({random_uniform, Max}, _From, State) ->
    {reply, random_uniform(Max), State};
handle_call({random_uniform, Min, Max}, _From, State) ->
    {reply, random_uniform(Min, Max), State};
handle_call(_Other, _From, State) ->
    {noreply, State}.

%% Unused callbacks
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

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
