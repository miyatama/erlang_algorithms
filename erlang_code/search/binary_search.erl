-module(binary_search).

-export([test/0]).

-import(search_funcs, [
  get_search_list/1,
  compare/2,
  search_test/2,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-define(OUTPUT_DEBUG(S, Args), output_debug(binary_search, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(binary_search, S)).
-define(OUTPUT_INFO(S, Args), output_info(binary_search, S, Args)).
-define(OUTPUT_INFO(S), output_info(binary_search, S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  search_test(
    binary_search, 
    fun(T, L) -> search(T, L) end),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(T, list(T)) -> T | not_found.
search(_, []) -> not_found;
search(Value, L) -> 
  search(Value, L, 1, length(L)).
search(_, _, Min, Max) when Min > Max -> not_found;
search(Value, L, Min, Max) -> 
  Index = trunc((Max + Min)  / 2),
  case compare(Value, lists:nth(Index, L)) of 
    equal_to -> Value;
    greater_than -> search(Value, L, Index + 1, Max);
    _ -> search(Value, L, Min, Index - 1)
  end.
