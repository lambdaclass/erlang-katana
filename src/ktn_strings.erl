-module(ktn_strings).

-export([to_string/1]).

-spec to_string(atom() | integer() | binary() | list()) -> list().
to_string(Value) when is_atom(Value) ->
  atom_to_list(Value);
to_string(Value) when is_integer(Value) ->
  integer_to_list(Value);
to_string(Value) when is_binary(Value) ->
  binary_to_list(Value);
to_string(Value) when is_list(Value) ->
  Value.
