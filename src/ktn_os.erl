%% @doc Utility functions to run commands in the underlying OS.
-module(ktn_os).

-export([command/1, command/2]).

-type opts() :: #{log_fun => fun((iodata()) -> any())}.
-type exit_status() :: integer().

-spec command(iodata()) -> {exit_status(), string()}.
command(Cmd) ->
  Opts = #{log_fun => fun error_logger:info_msg/1},
  command(Cmd, Opts).

-spec command(iodata(), opts()) -> {exit_status(), string()}.
command(Cmd, Opts) ->
  PortOpts = [stream, exit_status, eof],
  Port = open_port({spawn, shell_cmd()}, PortOpts),
  erlang:port_command(Port, make_cmd(Cmd)),
  get_data(Port, Opts, []).

-spec get_data(port(), opts(), [string()]) -> {exit_status(), string()}.
get_data(Port, Opts, Data) ->
  %% Get timeout option or an hour if undefined.
  Timeout = maps:get(timeout, Opts, 600000),
  receive
    {Port, {data, NewData}} ->
      case maps:get(log_fun, Opts, undefined) of
        Fun when is_function(Fun) -> Fun(NewData);
        undefined -> ok
      end,
      get_data(Port, Opts, [NewData | Data]);
    {Port, eof} ->
      port_close(Port),
      receive
        {Port, {exit_status, ExitStatus}} ->
          {ExitStatus, lists:flatten(lists:reverse(Data))}
      end
  after
    Timeout -> throw(timeout)
  end.

-spec make_cmd(string()) -> iodata().
make_cmd(Cmd) ->
  %% We insert a new line after the command, in case the command
  %% contains a comment character.
  [$(, unicode:characters_to_binary(Cmd), "\n) </dev/null; exit\n"].

-spec shell_cmd() -> string().
shell_cmd() -> "sh -s unix:cmd 2>&1".
