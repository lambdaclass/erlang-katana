-module(ktn_type).

-export([
         get/1
        ]).

-type type() :: integer
              | float
              | list
              | tuple
              | binary
              | bitstring
              | boolean
              | function
              | pid
              | port
              | reference
              | atom
              | unknown
              .

-spec get(term()) -> type().
get(X) when is_integer(X)   -> integer;
get(X) when is_float(X)     -> float;
get(X) when is_list(X)      -> list;
get(X) when is_tuple(X)     -> tuple;
get(X) when is_binary(X)    -> binary;
get(X) when is_bitstring(X) -> bitstring;  % will fail before e12
get(X) when is_boolean(X)   -> boolean;
get(X) when is_function(X)  -> function;
get(X) when is_pid(X)       -> pid;
get(X) when is_port(X)      -> port;
get(X) when is_reference(X) -> reference;
get(X) when is_atom(X)      -> atom;

get(_X)                     -> unknown.
