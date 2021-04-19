-module(sequential_search).

-export([test/0]).

-import(search_funcs, [
  get_search_list/1,
  compare/2,
  search_test/2,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-define(OUTPUT_DEBUG(S, Args), output_debug(sequential_search, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(sequential_search, S)).
-define(OUTPUT_INFO(S, Args), output_info(sequential_search, S, Args)).
-define(OUTPUT_INFO(S), output_info(sequential_search, S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  search_test(
    sequential_search, 
    fun(T, L) -> search(T, L) end),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(T, list(T)) -> T | not_found.
search(_, []) -> not_found;
search(Value, L) -> 
  [H|T] = L,
  case compare(Value, H) of
    equal_to -> H;
    _ -> search(Value, T)
  end.
