-module(erl_ofc_of13_util).

%% API
-export([pad_size/1, parse_tlv/2, parse_array_struct/4, binary_splitter/2]).

pad_size(ByteSize) ->
  case (ByteSize) rem 8 of
    0 -> 0;
    OverLength -> 8 - OverLength
  end.

parse_tlv(<<>>, Acc) ->
  lists:reverse(Acc);
parse_tlv(Bin, Acc) ->
  <<Length:16, _/binary>> = Bin,
  HeadBytes = binary:part(Bin, {0, Length}),
  RestBytes = binary:part(Bin, {Length, byte_size(Bin)-Length}),
  parse_tlv(RestBytes, [ HeadBytes | Acc ]).

parse_array_struct(<<>>, _Splitter, _Parser, Acc) ->
  lists:reverse(Acc);
parse_array_struct(Bin, Splitter, Parser, Acc) ->
  [Head, Tail] = Splitter(Bin),
  parse_array_struct(Tail, Splitter, Parser, [Parser(Head) | Acc]).

%% split binary at position
%% returns: [Head, Rest]
-spec binary_splitter(binary(), integer()) -> [binary()].
binary_splitter(Bin, At) ->
  [
    binary:part(Bin, 0, At),
    case byte_size(Bin) - At of
      RestLength when RestLength =:= 0 -> <<>>;
      RestLength -> binary:part(Bin, At, RestLength)
    end
  ].
