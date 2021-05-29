-module(merge_sort).

-export([start/0,
         start/1,
         test/0]).

-import(sort_funcs, [
  get_sort_list/1,
  show_list/1,
  compare/2,
  sort_test/2,
  swap/3,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-define(OUTPUT_DEBUG(S, Args), output_debug(merge_sort, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(merge_sort, S)).
-define(OUTPUT_INFO(S, Args), output_info(merge_sort, S, Args)).
-define(OUTPUT_INFO(S), output_info(merge_sort, S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  split_list_test(),
  sort_test(
    merge_sort, 
    fun(L) -> sort(L) end),
  ok.

start() ->
  ?OUTPUT_DEBUG("start/0"),
  start(10).
start(N) ->
  ?OUTPUT_DEBUG("start/1"),
  L = get_sort_list(N),
  ?OUTPUT_DEBUG("list size: ~w~n", [length(L)]),
  show_list(L),
  L1 = sort(L),
  show_list(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sort(list(T)) -> list(T).
sort([]) -> [];
sort(L) when length(L) < 2 -> L;
sort(L) when length(L) =:= 2 -> 
  [Item1, Item2] = L,
  case compare(Item1, Item2) of
    greater_than -> swap(L, 1, 2);
    _ -> L
  end;
sort(L) ->
  SplitIndex = trunc(length(L) / 2),
  {L1, L2} = split_list(SplitIndex, L),
  SortedL1 = sort(L1),
  SortedL2 = sort(L2),
  merge(SortedL1, SortedL2).

-spec merge(list(T), list(T)) -> list(T).
merge(L1, L2) -> merge(L1, L2, []).
-spec merge(list(T), list(T), list(T)) -> list(T).
merge([], [], ResultList) -> ResultList;
merge(L1, [], ResultList) -> lists:append(ResultList, L1);
merge([], L2, ResultList) -> lists:append(ResultList, L2);
merge(L1, L2, ResultList) -> 
  [H1|T1] = L1,
  [H2|T2] = L2,
  case compare(H1, H2) of
    equal_to ->
      ResultList1 = lists:append(ResultList, [H1, H2]),
      merge(T1, T2, ResultList1);
    greater_than ->
      ResultList1 = lists:append(ResultList, [H2]),
      merge(L1, T2, ResultList1);
    lower_than ->
      ResultList1 = lists:append(ResultList, [H1]),
      merge(T1, L2, ResultList1)
  end.

-spec split_list(integer(), list(T)) -> {list(T), list(T)}.
split_list(_, []) -> {[], []};
split_list(I, _) when I =< 0 -> {[], []};
split_list(I, L) when I > length(L) -> {[], []};
split_list(I, L) ->
  {
    lists:sublist(L, I),
    lists:sublist(L, I + 1, length(L) - I)
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split_list_test() ->
  % empty list
  L1Expect = {[], []},
  L1Result = split_list(1, []),
  show_split_list_test_result("case 001 list empty: ~w", L1Expect, L1Result),
  % index out of upper bound
  L2Expect = {[], []},
  L2Result = split_list(6, [1, 2, 3, 4, 5]),
  show_split_list_test_result("case 002 index out of upper bound: ~w", L2Expect, L2Result),
  % index out of lower bound
  L3Expect = {[], []},
  L3Result = split_list(0, [1, 2, 3, 4, 5]),
  show_split_list_test_result("case 003 index out of lower bound: ~w", L3Expect, L3Result),
  % head is one factor
  L4Expect = {[1], [2, 3, 4, 5]},
  L4Result = split_list(1, [1, 2, 3, 4, 5]),
  show_split_list_test_result("case 004 front one factor: ~w", L4Expect, L4Result),
  % tail is one factor
  L5Expect = {[1, 2, 3, 4], [5]},
  L5Result = split_list(4, [1, 2, 3, 4, 5]),
  show_split_list_test_result("case 005 rear one factor: ~w", L5Expect, L5Result),
  % tail is empty
  L6Expect = {[1, 2, 3, 4, 5], []},
  L6Result = split_list(5, [1, 2, 3, 4, 5]),
  show_split_list_test_result("case 006 tail is empty: ~w", L6Expect, L6Result),
  % list length is 1
  L7Expect = {[1], []},
  L7Result = split_list(1, [1]),
  show_split_list_test_result("case 007 list length is 1: ~w", L7Expect, L7Result),
  ok.

show_split_list_test_result(Title, Expect, Result) ->
  case Result of
    Expect ->
      ?OUTPUT_INFO(Title, [true]);
    _ ->
      ?OUTPUT_INFO(Title, [false]),
      ?OUTPUT_INFO("Expect: ~w, Result: ~w", [Expect, Result])
  end.
