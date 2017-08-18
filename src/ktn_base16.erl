%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_base16: encoding and decoding Base16 binaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_base16).

-export([ encode/1
        , decode/1
        ]).

%% @doc Encodes a binary in base 16.
-spec encode(binary()) -> binary().
encode(Bin) ->
    << <<(encode_char(A)):8/unsigned-integer,
         (encode_char(B)):8/unsigned-integer>> ||
       <<A:4/unsigned-integer, B:4/unsigned-integer>> <= Bin>>.

%% @doc Decodes a base 16 binary.
-spec decode(binary()) -> binary().
decode(Bin) ->
    << <<(decode_char(A)):4/unsigned-integer,
         (decode_char(B)):4/unsigned-integer>> ||
       <<A:8/unsigned-integer, B:8/unsigned-integer>> <= Bin>>.

encode_char(C) when C > 9 ->
    C + $A - 10;
encode_char(C) ->
    C + $0.

decode_char(C) when C >= $A ->
    C - $A + 10;
decode_char(C) when C >= $a ->
    C - $a + 10;
decode_char(C) ->
    C - $0.