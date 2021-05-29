-module(hash_based_search).

-export([test/0]).

-import(search_funcs, [
  get_search_list/1,
  compare/2,
  search_test/2,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-define(OUTPUT_DEBUG(S, Args), output_debug(hash_based_search, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(hash_based_search, S)).
-define(OUTPUT_INFO(S, Args), output_info(hash_based_search, S, Args)).
-define(OUTPUT_INFO(S), output_info(hash_based_search, S)).

-include("search_record.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  search_test(
    hash_based_search, 
    fun(T, L) -> search(T, L) end),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(T, list(T)) -> T | not_found.
search(_, []) -> not_found;
search(Value, L) -> 
  HashTable = create_hash_table(L),
  #item{value=SortValue} = Value,
  ValueHash = hash(SortValue),
  case maps:is_key(ValueHash, HashTable) of
    false -> not_found;
    true -> maps:get(ValueHash, HashTable)
  end.

-spec create_hash_table(list()) -> map().
create_hash_table(L) ->
  create_hash_table(L, maps:new()).
-spec create_hash_table(list(), map()) -> map().
create_hash_table([], Map) -> Map;
create_hash_table(L, Map) ->
  [H|T] = L,
  #item{value=SortValue} = H,
  Map1 = maps:put(hash(SortValue), H, Map),
  create_hash_table(T, Map1).

hash(I) ->
  Text = lists:flatten(io_lib:format("~p", [I])),
  crypto:hash(md5, Text).
