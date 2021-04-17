% c(sort_funcs).
% c(insertion_sort).
% insertion_sort:test().
% insertion_sort:start(1000).
-module(insertion_sort).

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
    insertion_sort,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sort(L) when 1 >= length(L) -> 
  L;
sort(L) ->
  sort(L, fun(A, B) -> compare(A, B) end).
sort(L, F) ->
  % when occured insertion then retry
  case sort(L, F, false) of
    {L1, true} -> sort(L1, F);
    {L1, false} -> L1
  end.
sort(L, F, Insertion) ->
  sort([], L, F, Insertion).
sort(Front, Rear, _, Insertion) when 1 >= length(Rear) ->
  {lists:append(Front, Rear), Insertion};
sort(Front, Rear, F, Insertion) ->
  [H1, H2 | T] = Rear,
  case F(H1, H2) of
    greater_than -> 
      insertion(H1, Front, [H2 | T], F);
    _ ->
      Front1 = lists:append(Front, [H1]),
      Rear1 = [H2 | T],
      sort(Front1, Rear1, F, Insertion)
  end.

-spec insertion(
  T, 
  list(T), 
  list(T), 
  fun((T, T) -> greater_than | equal_to | lower_than)) -> {list(T), true | false}.
insertion(Value, Front, Rear, _) when 0 >= length(Rear) ->
  {lists:append(Front, [Value]), true};
insertion(Value, Front, Rear, F) ->
  [H1 | T] = Rear,
  case F(Value, H1) of
    greater_than ->
      Front1 = lists:append(Front, [H1]),
      insertion(Value, Front1, T, F);
    _ ->
      Front1 = lists:append(Front, [Value]),
      L = lists:append(Front1, Rear),
      {L, true}
  end.
