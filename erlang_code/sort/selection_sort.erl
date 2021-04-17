% c(sort_funcs).
% c(selection_sort).
% selection_sort:test().
% selection_sort:start(1000).
-module(selection_sort).

-export([start/0,
         start/1,
         test/0]).

-import(sort_funcs, [
  get_sort_list/1,
  show_list/1,
  compare/2,
  sort_test/2]).

-include("sort_record.hrl").
-define(DEBUG(S), io:fwrite("[DEBUG] insertion_sort: " ++ S ++ "~n")).
% -define(DEBUG(S), io:fwrite("")).
-define(DEBUG(S, Args), io:fwrite("[DEBUG] insertion_sort: " ++ S ++ "~n", Args)).
% -define(DEBUG(S, Args), io:fwrite("")).

test() ->
  sort_test(
    selection_sort,
    fun(L) -> sort(L) end).

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

-spec sort(list(T)) -> list(T).
sort(L) when 1 >= length(L) ->
  L;
sort(L) ->
  sort(
    L, 
    fun(A, B) -> compare(A, B) end,
    length(L)).

-spec sort(
  list(T), 
  fun((T, T) -> greater_than | equal_to | lower_than),
  integer()) -> list(T).
sort(L, _, 0) ->
  L;
sort(L, F, N) ->
  {MaxValue, L1} = get_max_value(L, F, []),
  L2 = lists:append(L1, [MaxValue]),
  sort(L2, F, N-1).

-spec get_max_value(
  list(T),
  fun((T, T) -> greater_than | equal_to | lower_than),
  list(T)) -> {T, list(T)}.
get_max_value(L, _, Retain) when 1 >= length(L) ->
  [H1|_] = L,
  {H1, Retain};
get_max_value(L, F, Retain) ->
  [H1, H2|T] = L,
  case F(H1, H2) of
    greater_than ->
      Retain1 = lists:append(Retain, [H2]),
      L1 = [H1 | T],
      get_max_value(L1, F, Retain1);
    _ ->
      Retain1 = lists:append(Retain, [H1]),
      L1 = [H2 | T],
      get_max_value(L1, F, Retain1)
  end.
