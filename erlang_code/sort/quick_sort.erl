-module(quick_sort).

-export([start/0,
         start/1,
         test/0]).

-import(sort_funcs, [
  get_sort_list/1,
  show_list/1,
  compare/2,
  sort_test/2,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-record(item, {sort_value}).

-define(OUTPUT_DEBUG(S, Args), output_debug("quick_sort", S, Args)).
-define(OUTPUT_DEBUG(S), output_debug("quick_sort", S)).
-define(OUTPUT_INFO(S, Args), output_info("quick_sort", S, Args)).
-define(OUTPUT_INFO(S), output_info("quick_sort", S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  partition_test(),
  sort_test(
    quick_sort, 
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
sort(L) ->
  quick_sort(L).

-spec quick_sort(list(T)) -> list(T).
quick_sort([]) -> [];
quick_sort(L) when length(L) =:= 1 -> L;
quick_sort(L) when length(L) < 100 -> heap_sort:sort(L);
quick_sort(L) ->
  quick_sort(L, get_pivot_index(L)).
quick_sort(L, PivotIndex) when length(L) < PivotIndex ->
  heap_sort:sort(L);
quick_sort(L, PivotIndex) ->
  {L1, L2} = partition(L, PivotIndex),
  case L1 of
    L -> quick_sort(L, PivotIndex + 1);
    _ ->
      lists:append(
        quick_sort(L1),
        quick_sort(L2))
  end.

-spec partition(list(T), _) -> {list(T), list(T)}.
partition(L, PivotIndex) when PivotIndex < 1 -> {L, []};
partition(L, PivotIndex) when PivotIndex > length(L) -> {L, []};
partition(L, PivotIndex) -> 
  #item{sort_value=BaseValue} = lists:nth(PivotIndex, L),
  {
    [#item{sort_value=Value} || #item{sort_value=Value} <- L, Value =< BaseValue],
    [#item{sort_value=Value} || #item{sort_value=Value} <- L, Value > BaseValue]
  }.

-spec get_pivot_index(list()) -> integer().
get_pivot_index(L) ->
  trunc(length(L) / 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
partition_test() ->
  % empty
  L1FrontExpect = [],
  L1RearExpect = [],
  {L1FrontResult, L1RearResult} = partition([], 1),
  show_partition_test_result(
    "case 001 - etmpty: ~w",
    L1FrontExpect, 
    L1RearExpect, 
    L1FrontResult, 
    L1RearResult),
  % front is one
  L2FrontExpect = [#item{sort_value=3}],
  L2RearExpect = [
    #item{sort_value=4},
    #item{sort_value=5},
    #item{sort_value=6},
    #item{sort_value=7}],
  {L2FrontResult, L2RearResult} = partition(
    [
      #item{sort_value=3},
      #item{sort_value=4},
      #item{sort_value=5},
      #item{sort_value=6},
      #item{sort_value=7}
    ],
    1),
  show_partition_test_result(
    "case 002 - front one factor: ~w",
    L2FrontExpect, 
    L2RearExpect, 
    L2FrontResult, 
    L2RearResult),
  % rear is empty
  L3FrontExpect = [
    #item{sort_value=3},
    #item{sort_value=4},
    #item{sort_value=5},
    #item{sort_value=6},
    #item{sort_value=7}
  ],
  L3RearExpect = [],
  {L3FrontResult, L3RearResult} = partition(
    [
      #item{sort_value=3},
      #item{sort_value=4},
      #item{sort_value=5},
      #item{sort_value=6},
      #item{sort_value=7}
    ],
    5),
  show_partition_test_result(
    "case 003 - rear is empty: ~w",
    L3FrontExpect, 
    L3RearExpect, 
    L3FrontResult, 
    L3RearResult),
  % center separate
  L4FrontExpect = [
    #item{sort_value=3},
    #item{sort_value=4},
    #item{sort_value=5}
  ],
  L4RearExpect = [
    #item{sort_value=6},
    #item{sort_value=7}
  ],
  {L4FrontResult, L4RearResult} = partition(
    [
      #item{sort_value=3},
      #item{sort_value=4},
      #item{sort_value=5},
      #item{sort_value=6},
      #item{sort_value=7}
    ],
    3),
  show_partition_test_result(
    "case 004 - separate center: ~w",
    L4FrontExpect, 
    L4RearExpect, 
    L4FrontResult, 
    L4RearResult),
  % index out of order 01
  L5FrontExpect = [
    #item{sort_value=3},
    #item{sort_value=4},
    #item{sort_value=5},
    #item{sort_value=6},
    #item{sort_value=7}
  ],
  L5RearExpect = [],
  {L5FrontResult, L5RearResult} = partition(
    [
      #item{sort_value=3},
      #item{sort_value=4},
      #item{sort_value=5},
      #item{sort_value=6},
      #item{sort_value=7}
    ],
    0),
  show_partition_test_result(
    "case 005 - out of order 01: ~w",
    L5FrontExpect, 
    L5RearExpect, 
    L5FrontResult, 
    L5RearResult),
  % index out of order 01
  L6FrontExpect = [
    #item{sort_value=3},
    #item{sort_value=4},
    #item{sort_value=5},
    #item{sort_value=6},
    #item{sort_value=7}
  ],
  L6RearExpect = [],
  {L6FrontResult, L6RearResult} = partition(
    [
      #item{sort_value=3},
      #item{sort_value=4},
      #item{sort_value=5},
      #item{sort_value=6},
      #item{sort_value=7}
    ],
    6),
  show_partition_test_result(
    "case 006 - out of order 02: ~w",
    L6FrontExpect, 
    L6RearExpect, 
    L6FrontResult, 
    L6RearResult),
  ok.

show_partition_test_result(Title, FrontExpect, RearExpect, FrontResult, RearResult) ->
  FrontCheck = case FrontResult of
    FrontExpect -> true;
    _ -> false
  end,
  RearCheck = case RearResult of
    RearExpect -> true;
    _ -> false
  end,
  case {FrontCheck, RearCheck} of
    {true, true} ->
      ?OUTPUT_INFO(Title, [true]);
    _ ->
      ?OUTPUT_INFO(Title, [false]),
      ?OUTPUT_INFO("Front~n  Expect: ~w~n  Result: ~w", [FrontExpect, FrontResult]),
      ?OUTPUT_INFO("Rear~n  Expect: ~w~n  Result: ~w", [RearExpect, RearResult])
  end.
