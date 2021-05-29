-module(cocktail_shaker_sort).

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

-define(OUTPUT_DEBUG(S, Args), output_debug(cocktail_shaker_sort, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(cocktail_shaker_sort, S)).
-define(OUTPUT_INFO(S, Args), output_info(cocktail_shaker_sort, S, Args)).
-define(OUTPUT_INFO(S), output_info(cocktail_shaker_sort, S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  sort_test(
    cocktail_shaker_sort, 
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
sort(L) ->
  sort(L, min, [], []).
sort([], _, Asc, Desc) ->
  lists:append(Asc, lists:reverse(Desc));
sort(L, Direction, Asc, Desc) ->
  case Direction of
    min ->
      {MinValue, L1} = choice_min_value(L),
      Asc1 = lists:append(Asc, [MinValue]),
      sort(L1, max, Asc1, Desc);
    max ->
      {MaxValue, L1} = choice_max_value(L),
      Desc1 = lists:append(Desc, [MaxValue]),
      sort(L1, min, Asc, Desc1)
  end.

-spec choice_min_value(list(T)) -> {T, list(T)}.
choice_min_value([]) -> {null, []};
choice_min_value(L) ->
  choice_min_value(L, 2, 1).
choice_min_value(L, I, MinIndex) when I > length(L) ->
  {
    lists:nth(MinIndex, L), 
    lists:append(
      lists:sublist(L, MinIndex - 1),
      lists:nthtail(MinIndex, L))
  };
choice_min_value(L, I, MinIndex) ->
  MinValue = lists:nth(MinIndex, L),
  TestValue = lists:nth(I, L),
  MinIndex1 = case compare(MinValue, TestValue) of
    greater_than -> I;
    _ -> MinIndex
  end,
  choice_min_value(L, I + 1, MinIndex1).

-spec choice_max_value(list(T)) -> {T, list(T)}.
choice_max_value([]) -> {null, []};
choice_max_value(L) ->
  choice_max_value(L, 2, 1).
choice_max_value(L, I, MaxIndex) when I > length(L) ->
  {
    lists:nth(MaxIndex, L), 
    lists:append(
      lists:sublist(L, MaxIndex - 1),
      lists:nthtail(MaxIndex, L))
  };
choice_max_value(L, I, MaxIndex) ->
  MaxValue = lists:nth(MaxIndex, L),
  TestValue = lists:nth(I, L),
  MaxIndex1 = case compare(MaxValue, TestValue) of
    lower_than -> I;
    _ -> MaxIndex
  end,
  choice_max_value(L, I + 1, MaxIndex1).
