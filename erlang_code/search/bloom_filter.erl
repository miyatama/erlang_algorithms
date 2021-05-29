-module(bloom_filter).

-export([test/0]).

-import(search_funcs, [
  get_search_list/1,
  compare/2,
  search_test/2,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-define(OUTPUT_DEBUG(S, Args), output_debug(bloom_filter, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(bloom_filter, S)).
-define(OUTPUT_INFO(S, Args), output_info(bloom_filter, S, Args)).
-define(OUTPUT_INFO(S), output_info(bloom_filter, S)).

-include("search_record.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  and_bit_string_test(),
  or_bit_string_test(),
  convert_mod_to_bitlength_test(),
  search_test(
    bloom_filter, 
    fun(T, L) -> search(T, L) end),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(T, list(T)) -> T | not_found.
search(_, []) -> not_found;
search(Value, L) -> 
  Mod = 1024,
  Filter = create_filter_table(L, Mod),
  ?OUTPUT_DEBUG(
     "filter: ~s", [[io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Filter]]),
  filtered(Value, Filter, Mod).

-spec create_filter_table(list(), integer()) -> list().
create_filter_table(L, _) when length(L) =< 0 -> [];
create_filter_table(L, Mod) ->
  BitLength = convert_mod_to_bitlength(Mod),
  HashTable = <<0:BitLength>>,
  set_bit_to_filter_table(L, HashTable, Mod).
	
set_bit_to_filter_table([], HashTable, _) -> HashTable;
set_bit_to_filter_table(L, HashTable, Mod) ->
  BitLength = convert_mod_to_bitlength(Mod),
  [H|T] = L,
  HashValue = value_to_hash(H, Mod),
  HashTable1 = or_bit_string(HashTable, <<HashValue:BitLength>>),
  set_bit_to_filter_table(T, HashTable1, Mod).

-spec convert_mod_to_bitlength(integer()) -> integer().
convert_mod_to_bitlength(Mod) ->
  BitLength = get_bit_length(Mod),
  case (BitLength rem 8) =:= 0 of
    false ->
	 (trunc(BitLength / 8) + 1) * 8;
    _ ->
	 BitLength
  end.

get_bit_length(Mod) when Mod =< 0 -> 1;
get_bit_length(Mod) ->
  1 + get_bit_length(trunc(Mod / 2)).

-spec or_bit_string(binary(), binary()) -> binary().
or_bit_string(<<"">>, _) -> <<"">>;
or_bit_string(_, <<"">>) -> <<"">>;
or_bit_string(Src, _) when not is_binary(Src) -> <<"">>;
or_bit_string(_, Dist) when not is_binary(Dist) -> <<"">>;
or_bit_string(Src, Dist) ->
  SrcList = bitstring_to_list(Src),
  DistList = bitstring_to_list(Dist),
  erlang:list_to_bitstring(or_integer_list(SrcList, DistList)).

or_integer_list([], _) -> [];
or_integer_list(_, []) -> [];
or_integer_list(Src, Dist) ->
  [SrcH|SrcT] = Src,
  [DistH|DistT] = Dist,
  lists:append([SrcH bor DistH], or_integer_list(SrcT, DistT)).

-spec and_bit_string(binary(), binary()) -> binary().
and_bit_string(<<"">>, _) -> <<"">>;
and_bit_string(_, <<"">>) -> <<"">>;
and_bit_string(Src, _) when not is_binary(Src) -> <<"">>;
and_bit_string(_, Dist) when not is_binary(Dist) -> <<"">>;
and_bit_string(Src, Dist) ->
  SrcList = bitstring_to_list(Src),
  DistList = bitstring_to_list(Dist),
  erlang:list_to_bitstring(and_integer_list(SrcList, DistList)).

and_integer_list([], _) -> [];
and_integer_list(_, []) -> [];
and_integer_list(Src, Dist) ->
  [SrcH|SrcT] = Src,
  [DistH|DistT] = Dist,
  lists:append([SrcH band DistH], and_integer_list(SrcT, DistT)).

-spec filtered(T, list(), integer()) -> not_found | T.
filtered(Value, L, Mod) -> 
  HashValue = value_to_hash(Value, Mod),
  BitLength = convert_mod_to_bitlength(Mod),
  TestValue = <<HashValue:BitLength>>,
  ?OUTPUT_DEBUG(
     "filtered/3 - hash value: ~w, 0x~s",
     [
       HashValue,
       [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= TestValue]
     ]),
  AndValue = and_bit_string(L, TestValue),
  ?OUTPUT_DEBUG(
     "filtered/3 - Filterd: ~s", [[io_lib:format("~2.16.0B", [X]) || <<X:8>> <= AndValue]]),
  case AndValue of
    TestValue -> Value;
    _ -> not_found
  end.

-spec value_to_hash(#item{}, integer()) -> integer().
value_to_hash(Value, Mod) ->
  #item{value=SortValue} = Value,
  Text = lists:flatten(io_lib:format("~p", [SortValue])),
  hash(Text, Mod).

-spec hash(list(), integer()) -> integer().
hash(L, Mod) ->
  hash(L) rem Mod.
-spec hash(list()) -> integer().
hash(L) when length(L) =< 1 ->
  [H|_] = L,
  H;
hash(L) ->
  [H|T] = L,
  trunc(math:pow(H * 31, length(L) - 1)) + hash(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
and_bit_string_test() ->
  % 0xFF and all 0xFF
  Src1 = <<255>>,
  Dist1 = <<255>>,
  Expect1 = <<255>>,
  Result1 = and_bit_string(Src1, Dist1),
  show_and_bit_string_test_result(
    "and_bit_string_test() case 001 - 0xFF and all 0xFF: ~w", 
    Expect1, 
    Result1),
  % 0x00 and all 0x01
  Src2 = <<0:8>>,
  Dist2 = <<1:8>>,
  Expect2 = <<0:8>>,
  Result2 = and_bit_string(Src2, Dist2),
  show_and_bit_string_test_result(
    "and_bit_string_test() case 002 - 0x00 and all 0x01: ~w", 
    Expect2, 
    Result2),
  % 0x01 and all 0x03
  Src3 = <<1:8>>,
  Dist3 = <<3:8>>,
  Expect3 = <<1:8>>,
  Result3 = and_bit_string(Src3, Dist3),
  show_and_bit_string_test_result(
    "and_bit_string_test() case 003 - 0x01 and all 0x03: ~w", 
    Expect3, 
    Result3),
  % 0xFFFF and all 0xFFFF
  Src4 = <<255, 255>>,
  Dist4 = <<255, 255>>,
  Expect4 = <<255, 255>>,
  Result4 = and_bit_string(Src4, Dist4),
  show_and_bit_string_test_result(
    "and_bit_string_test() case 004 - 0xFFFF and all 0xFFFF: ~w", 
    Expect4, 
    Result4),
  % 0x0000 and all 0x0101
  Src5 = <<0:16>>,
  Dist5 = <<1:8, 1:8>>,
  Expect5 = <<0:8, 0:8>>,
  Result5 = and_bit_string(Src5, Dist5),
  show_and_bit_string_test_result(
    "and_bit_string_test() case 005 - 0x0000 and all 0x0101: ~w", 
    Expect5, 
    Result5),
  % 0x0101 and all 0x0303
  Src6 = <<1:8, 1:8>>,
  Dist6 = <<3:8, 3:8>>,
  Expect6 = <<1:8, 1:8>>,
  Result6 = and_bit_string(Src6, Dist6),
  show_and_bit_string_test_result(
    "and_bit_string_test() case 006 - 0x0101 and all 0x0303: ~w", 
    Expect6, 
    Result6),
  ok.

show_and_bit_string_test_result(Title, Expect, Result) ->
  case Expect of
    Result ->
      ?OUTPUT_INFO(Title, [true]);
    _ ->
      ?OUTPUT_INFO(Title, [false]),
      ?OUTPUT_INFO("Expect: 0x~s, Result: 0x~s", [
        [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Expect],
        [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Result]
      ])
  end.

or_bit_string_test() ->
  % 0xFF and all 0xFF
  Src1 = <<255>>,
  Dist1 = <<255>>,
  Expect1 = <<255>>,
  Result1 = or_bit_string(Src1, Dist1),
  show_or_bit_string_test_result(
    "or_bit_string_test() case 001 - 0xFF or 0xFF: ~w", 
    Expect1, 
    Result1),
  % 0x00 and all 0x01
  Src2 = <<0:8>>,
  Dist2 = <<1:8>>,
  Expect2 = <<1:8>>,
  Result2 = or_bit_string(Src2, Dist2),
  show_or_bit_string_test_result(
    "or_bit_string_test() case 002 - 0x00 or 0x01: ~w", 
    Expect2, 
    Result2),
  % 0x01 and all 0x03
  Src3 = <<1:8>>,
  Dist3 = <<3:8>>,
  Expect3 = <<3:8>>,
  Result3 = or_bit_string(Src3, Dist3),
  show_or_bit_string_test_result(
    "or_bit_string_test() case 003 - 0x01 or 0x03: ~w", 
    Expect3, 
    Result3),
  % 0xFFFF and all 0xFFFF
  Src4 = <<255, 255>>,
  Dist4 = <<255, 255>>,
  Expect4 = <<255, 255>>,
  Result4 = or_bit_string(Src4, Dist4),
  show_or_bit_string_test_result(
    "or_bit_string_test() case 004 - 0xFFFF or 0xFFFF: ~w", 
    Expect4, 
    Result4),
  % 0x0000 and all 0x0101
  Src5 = <<0:16>>,
  Dist5 = <<1:8, 1:8>>,
  Expect5 = <<1:8, 1:8>>,
  Result5 = or_bit_string(Src5, Dist5),
  show_or_bit_string_test_result(
    "or_bit_string_test() case 005 - 0x0000 or 0x0101: ~w", 
    Expect5, 
    Result5),
  % 0x0101 and all 0x0303
  Src6 = <<1:8, 1:8>>,
  Dist6 = <<3:8, 3:8>>,
  Expect6 = <<3:8, 3:8>>,
  Result6 = or_bit_string(Src6, Dist6),
  show_or_bit_string_test_result(
    "or_bit_string_test() case 006 - 0x0101 or 0x0303: ~w", 
    Expect6, 
    Result6),
  ok.

show_or_bit_string_test_result(Title, Expect, Result) ->
  case Expect of
    Result ->
      ?OUTPUT_INFO(Title, [true]);
    _ ->
      ?OUTPUT_INFO(Title, [false]),
      ?OUTPUT_INFO("Expect: 0x~s, Result: 0x~s", [
        [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Expect],
        [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Result]
      ])
  end.


convert_mod_to_bitlength_test() -> 
  % 1
  Expect1 = 8,
  Result1 = convert_mod_to_bitlength(1),
  show_convert_mod_to_bitlength_test_result(
    "convert_mod_to_bitlength_test() case 001 - convert 1: ~w",
    Expect1,
    Result1),
  % 4
  Expect2 = 8,
  Result2 = convert_mod_to_bitlength(4),
  show_convert_mod_to_bitlength_test_result(
    "convert_mod_to_bitlength_test() case 002 - convert 4: ~w",
    Expect2,
    Result2),
  % 16
  Expect3 = 8,
  Result3 = convert_mod_to_bitlength(16),
  show_convert_mod_to_bitlength_test_result(
    "convert_mod_to_bitlength_test() case 003 - convert 16: ~w",
    Expect3,
    Result3),
  % 256
  Expect4 = 16,
  Result4 = convert_mod_to_bitlength(256),
  show_convert_mod_to_bitlength_test_result(
    "convert_mod_to_bitlength_test() case 004 - convert 256: ~w",
    Expect4,
    Result4),
  % 1024
  Expect5 = 16,
  Result5 = convert_mod_to_bitlength(1024),
  show_convert_mod_to_bitlength_test_result(
    "convert_mod_to_bitlength_test() case 005 - convert 1024: ~w",
    Expect5,
    Result5),
  ok.

show_convert_mod_to_bitlength_test_result(Title, Expect, Result) ->
  case Result of
    Expect ->
      ?OUTPUT_INFO(Title, [true]);
    _ ->
      ?OUTPUT_INFO(Title, [false]),
      ?OUTPUT_INFO("Expect: ~w, Result: ~w", [Expect, Result])
  end.

