-module(heap_sort).

-export([start/0,
         start/1,
         test/0]).

-import(sort_funcs, [
  get_sort_list/1,
  show_list/1,
  compare/2,
  sort_test/1]).

% 1: DEBUG
% 2: INFO
% 3: WARN
% 4: ERROR
% 5: FATAL
-define(LOG_LEVEL, 2).
-define(DEBUG(S),
  case ?LOG_LEVEL =< 1 of
    true -> 
      io:fwrite("[DEBUG] heap_sort: " ++ S ++ "~n");
    false ->
      io:fwrite("")
  end).
-define(DEBUG(S, Args),
  case ?LOG_LEVEL =< 1 of
    true -> 
      io:fwrite("[DEBUG] heap_sort: " ++ S ++ "~n", Args);
    false ->
      io:fwrite("")
  end).
-define(INFO(S),
  case ?LOG_LEVEL =< 2 of
    true -> 
      io:fwrite("[INFO] heap_sort: " ++ S ++ "~n");
    false ->
      io:fwrite("")
  end).
-define(INFO(S, Args),
  case ?LOG_LEVEL =< 2 of
    true -> 
      io:fwrite("[INFO] heap_sort: " ++ S ++ "~n", Args);
    false ->
      io:fwrite("")
  end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  swap_test(),
  sort_test(fun(L) -> sort(L) end).

start() ->
  ?DEBUG("start/0"),
  start(10).
start(N) ->
  ?DEBUG("start/1"),
  L = get_sort_list(N),
  io:fwrite("list size: ~w~n", [length(L)]),
  show_list(L),
  L1 = sort(L),
  show_list(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sort(L) when length(L) =< 0 ->
  [];
sort(L) ->
  F = fun(A, B) -> compare(A, B) end,
  L1 = build_heap(L, F),
  N = length(L1),
  sort(L1, F, N).
sort(L, _, I) when I =< 1 ->
  L;
sort(L, F, I) ->
  L1 = swap(L, 1, I),
  L2 = heap_sort(L1, F, 1, I),
  sort(L2, F, I - 1).

build_heap(L, F) ->
  N = length(L),
  I = trunc(N / 2),
  build_heap(L, F, I, N + 1).
build_heap(L, _, I, _) when I =< 0 -> L;
build_heap(L, F, I, N) ->
  L1 = heap_sort(L, F, I, N),
  build_heap(L1, F, I - 1, N).

-spec heap_sort(
  list(T),
  fun((T, T) -> greater_than | lower_than | equal_to),
  I, 
  I) -> list(T).
heap_sort(L, F, I, N) ->
  {LeftNodeIndex, RightNodeIndex} = get_child_index(I, N),
  RootNode = list_nth(I, L),
  LeftNode = list_nth(LeftNodeIndex, L),
  RightNode = list_nth(RightNodeIndex, L),
  case max_nodes(F, RootNode, LeftNode, RightNode) of
    right ->
      L1 = swap(L, I, RightNodeIndex),
      heap_sort(L1, F, RightNodeIndex, N);
    left ->
      L1 = swap(L, I, LeftNodeIndex),
      heap_sort(L1, F, LeftNodeIndex, N);
    _ ->
      L
  end.

-spec get_child_index(I, I) -> {I | null, I | null}.
get_child_index(1, Max) ->
  {
    replace_over_index_to_null(2, Max),
    replace_over_index_to_null(3, Max)
  };
get_child_index(I, Max) ->
  {
    replace_over_index_to_null(I * 2, Max),
    replace_over_index_to_null(I * 2 + 1, Max)
  }.

replace_over_index_to_null(I, Max) when I < Max -> I;
replace_over_index_to_null(_, _) -> null.

-spec list_nth(_, list(T)) -> T.
list_nth(_, []) -> null;
list_nth(I, _) when I < 1 -> null;
list_nth(I, L) when length(L) < I -> null;
list_nth(I, L) ->
  lists:nth(I, L).

-spec max_nodes(
  fun((T, T) -> greater_than | lower_than | equal_to),
  T, T, T) -> right | left | root.
max_nodes(_, _, null, null) ->
  root;
max_nodes(F, RootNode, LeftNode, null) ->
  case F(RootNode, LeftNode) of
    lower_than -> left;
    _ -> root
  end;
max_nodes(F, RootNode, null, RightNode) ->
  case F(RootNode, RightNode) of
    lower_than -> right;
    _ -> root
  end;
max_nodes(F, RootNode, LeftNode, RightNode) ->
  {MaxResult, MaxNode} = case F(RootNode, LeftNode) of
    lower_than -> {left, LeftNode};
    _ -> {root, RootNode}
  end,
  case F(MaxNode, RightNode) of
    lower_than -> right;
    _ -> MaxResult
  end.

-spec swap(list(T), I, I) -> list(T).
swap(L, null, _) -> L;
swap(L, _, null) -> L;
swap(L, Index, _) when Index =< 0 -> L;
swap(L, Index, _) when Index > length(L) -> L;
swap(L, _, Index) when Index =< 0 -> L;
swap(L, _, Index) when Index > length(L) -> L;
swap(L, Index1, Index2) when is_float(Index1) -> swap(L, trunc(Index1), Index2);
swap(L, Index1, Index2) when is_float(Index2) -> swap(L, Index1, trunc(Index2));
swap(L, Index1, Index2) when Index1 > Index2 -> swap(L, Index2, Index1);
swap(L, Index1, Index2) ->
  Item1 = lists:nth(Index1, L),
  Item2 = lists:nth(Index2, L),
  L1 = lists:append(
    get_swap_head(L, Index1),
    [Item2]),
  L2 = lists:append(
    L1,
    get_swap_middle(L, Index1, Index2)),
  L3 = lists:append(L2, [Item1]),
  lists:append(L3, get_swap_tail(L, Index2)).

get_swap_head(L, I) ->
  lists:sublist(L, I -1).
get_swap_middle(L, I1, I2) ->
  lists:sublist(L, I1 + 1, I2 - I1 - 1).
get_swap_tail(L, I) ->
  lists:sublist(L, I + 1, length(L) - I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
swap_test() ->
  % head and tail
  L1Expect = [3, 2, 1],
  L1Result = swap([1, 2, 3], 1, 3),
  show_swap_test_result("case 001 - head and tail", L1Expect, L1Result),
  % 1st and 2nd
  L2Expect = [2, 1, 3],
  L2Result = swap([1, 2, 3], 1, 2),
  show_swap_test_result("case 002 - 1st and 2nd", L2Expect, L2Result),
  % 1st index out of range 01
  L3Expect = [1, 2, 3],
  L3Result = swap([1, 2, 3], 0, 2),
  show_swap_test_result("case 003 - 1st index out of range 01", L3Expect, L3Result),
  % 1st index out of range 02
  L4Expect = [1, 2, 3],
  L4Result = swap([1, 2, 3], 4, 2),
  show_swap_test_result("case 004 - 1st index out of range 02", L4Expect, L4Result),
  % 2nd index out of range 01
  L5Expect = [1, 2, 3],
  L5Result = swap([1, 2, 3], 1, 0),
  show_swap_test_result("case 005 - 2nd index out of range 01", L5Expect, L5Result),
  % 2nd index out of range 02
  L6Expect = [1, 2, 3],
  L6Result = swap([1, 2, 3], 1, 4),
  show_swap_test_result("case 006 - 2nd index out of range 02", L6Expect, L6Result),
  % invalid all index 01
  L7Expect = [1, 2, 3],
  L7Result = swap([1, 2, 3], 0, 0),
  show_swap_test_result("case 007 - all index out of range 02", L7Expect, L7Result),
  % invalid all index 02
  L8Expect = [1, 2, 3],
  L8Result = swap([1, 2, 3], 4, 4),
  show_swap_test_result("case 008 - all index out of range 02", L8Expect, L8Result),
  % 1st index is null
  L9Expect = [1, 2, 3],
  L9Result = swap([1, 2, 3], null, 2),
  show_swap_test_result("case 009 - 1st index is null", L9Expect, L9Result),
  % 2nd index is null
  L10Expect = [1, 2, 3],
  L10Result = swap([1, 2, 3], 1, null),
  show_swap_test_result("case 010 - 1st index is null", L10Expect, L10Result),
  % all index is null
  L11Expect = [1, 2, 3],
  L11Result = swap([1, 2, 3], null, null),
  show_swap_test_result("case 011 - all index is null", L11Expect, L11Result),
  % head, middle, tail exists.
  L12Expect = [1, 4, 3, 2, 5],
  L12Result = swap([1, 2, 3, 4, 5], 2, 4),
  show_swap_test_result("case 012 - swap 2 to 4", L12Expect, L12Result),
  % middle, tail exists.
  L13Expect = [3, 2, 1, 4, 5],
  L13Result = swap([1, 2, 3, 4, 5], 1, 3),
  show_swap_test_result("case 013 - swap 1 to 3", L13Expect, L13Result),
  % head, tail exists.
  L14Expect = [1, 2, 4, 3, 5],
  L14Result = swap([1, 2, 3, 4, 5], 3, 4),
  show_swap_test_result("case 014 - swap 3 to 4", L14Expect, L14Result),
  % head, middle exists.
  L15Expect = [1, 2, 5, 4, 3],
  L15Result = swap([1, 2, 3, 4, 5], 3, 5),
  show_swap_test_result("case 015 - swap 3 to 5", L15Expect, L15Result),
  % head exists.
  L16Expect = [1, 2, 3, 5, 4],
  L16Result = swap([1, 2, 3, 4, 5], 4, 5),
  show_swap_test_result("case 016 - swap 4 to 5", L16Expect, L16Result),
  % middle exists.
  L17Expect = [5, 2, 3, 4, 1],
  L17Result = swap([1, 2, 3, 4, 5], 1, 5),
  show_swap_test_result("case 017 - swap 1 to 5", L17Expect, L17Result),
  % tail exists.
  L18Expect = [2, 1, 3, 4, 5],
  L18Result = swap([1, 2, 3, 4, 5], 1, 2),
  show_swap_test_result("case 018 - swap 1 to 2", L18Expect, L18Result),
  ok.

show_swap_test_result(Title, Expect, Result) ->
  case Result of
    Expect ->
      T1 = Title ++ ": ~w",
      ?INFO(T1, [true]);
    _ ->
      T1 = Title ++ ": ~w~nExpect: ~w~nResult: ~w",
      ?INFO(T1, [false, Expect, Result])
  end.
