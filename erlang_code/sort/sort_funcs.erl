% c(sort_funcs).
-module(sort_funcs).

-export([get_sort_list/1,
  show_list/1,
  compare/2,
  sort_test/1]).

-record(item, {sort_value}).

-define(DEBUG(S), io:fwrite("[DEBUG] sort_funcs: " ++ S ++ "~n")).
-define(DEBUG(S, Args), io:fwrite("[DEBUG] sort_funcs: " ++ S ++ "~n", Args)).
-define(INFO(S), io:fwrite("[INFO] sort_funcs: " ++ S ++ "~n")).
-define(INFO(S, Args), io:fwrite("[INFO] sort_funcs: " ++ S ++ "~n", Args)).

get_sort_list(3) ->
  [
    #item{sort_value=3} ,
    #item{sort_value=2} ,
    #item{sort_value=1}
  ];
get_sort_list(N) ->
  get_sort_list([], N).
get_sort_list(L, 0) ->
  L;
get_sort_list(L, N) ->
  L1 = [#item{sort_value=rand:uniform(100)} | L],
  get_sort_list(L1, N - 1).

show_list(L) ->
  io:fwrite("list :"),
  [H|T] = L,
  show_list(H, T).
show_list(H, []) ->
  #item{sort_value=Value} = H,
  io:fwrite("~w~n", [Value]),
  ok;
show_list(H, T) ->
  #item{sort_value=Value} = H,
  io:fwrite("~w, ", [Value]),
  [H2|T2] = T,
  show_list(H2, T2).

compare(#item{sort_value=Avalue}, #item{sort_value=Bvalue}) when Avalue > Bvalue ->
  greater_than;
compare(#item{sort_value=Avalue}, #item{sort_value=Bvalue}) when Avalue =:= Bvalue ->
  equal_to;
compare(_, _) ->
  lower_than.

-spec sort_test(fun((list(T)) -> list(T))) -> true | false.
sort_test(F) ->
  L1 = generate_test_value([1, 2, 3, 4, 5]),
  L1Expect = generate_test_value([1, 2, 3, 4, 5]),
  L1SortResult = F(L1),
  L1TestResult = check_result(L1Expect, L1SortResult),
  show_check_result("sort_test - case 001 sorted list: ~w", L1TestResult, L1Expect, L1SortResult),
  L2 = generate_test_value([5, 1, 2, 3, 4]),
  L2Expect = generate_test_value([1, 2, 3, 4, 5]),
  L2SortResult = F(L2),
  L2TestResult = check_result(L2Expect, L2SortResult),
  show_check_result("sort_test - case 002 first to last: ~w", L2TestResult, L2Expect, L2SortResult),
  L3 = generate_test_value([1, 5, 3, 4, 2]),
  L3Expect = generate_test_value([1, 2, 3, 4, 5]),
  L3SortResult = F(L3),
  L3TestResult = check_result(L3Expect, L3SortResult),
  show_check_result("sort_test - case 003 rough: ~w", L3TestResult, L3Expect, L3SortResult),
  L4 = generate_test_value([5, 1, 3, 2, 4]),
  L4Expect = generate_test_value([1, 2, 3, 4, 5]),
  L4SortResult = F(L4),
  L4TestResult = check_result(L4Expect, L4SortResult),
  show_check_result("sort_test - case 004 reverse rough: ~w", L4TestResult, L4Expect, L4SortResult),
  L5 = generate_test_value([5, 4, 3, 2, 1]),
  L5Expect = generate_test_value([1, 2, 3, 4, 5]),
  L5SortResult = F(L5),
  L5TestResult = check_result(L5Expect, L5SortResult),
  show_check_result("sort_test - case 005 desc: ~w", L5TestResult, L5Expect, L5SortResult),
  L6 = generate_test_value([]),
  L6Expect = generate_test_value([]),
  L6SortResult = F(L6),
  L6TestResult = check_result(L6Expect, L6SortResult),
  show_check_result("sort_test - case 006 empty list: ~w", L6TestResult, L6Expect, L6SortResult),
  L7 = generate_test_value([1]),
  L7Expect = generate_test_value([1]),
  L7SortResult = F(L7),
  L7TestResult = check_result(L7Expect, L7SortResult),
  show_check_result("sort_test - case 007 one factor: ~w", L7TestResult, L7Expect, L7SortResult),
  L8 = generate_test_value([1, 2]),
  L8Expect = generate_test_value([1, 2]),
  L8SortResult = F(L8),
  L8TestResult = check_result(L8Expect, L8SortResult),
  show_check_result("sort_test - case 008 two factor: ~w", L8TestResult, L8Expect, L8SortResult),
  L9 = generate_test_value([2, 1]),
  L9Expect = generate_test_value([1, 2]),
  L9SortResult = F(L9),
  L9TestResult = check_result(L9Expect, L9SortResult),
  show_check_result("sort_test - case 009 two factor reverse: ~w", L9TestResult, L9Expect, L9SortResult),
  ok.

generate_test_value([]) ->
  [];
generate_test_value(L) ->
  [H|T] = L,
  [ #item{sort_value=H} |
    generate_test_value(T)].

-spec check_result(list(T), list(T)) -> true | false.
check_result(Expect, Result) ->
  ExpectLength = length(Expect),
  ResultLength = length(Result),
  case ExpectLength =:= ResultLength of
    true -> check_result_items(Expect, Result);
    false -> 
      ?DEBUG("length error"),
      false
  end.

-spec check_result_items(list(T), list(T)) -> true | false.
check_result_items([], []) ->
  true;
check_result_items(Expect, Result) ->
  [HExpect | TExpect] = Expect,
  [HResult | TResult] = Result,
  case compare(HExpect, HResult) of
    equal_to -> 
      check_result_items(TExpect, TResult);
    _ -> 
      false
  end.

show_check_result(Title, true, _, _) ->
  ?INFO(Title, [true]);
show_check_result(Title, false, Expect, Result) ->
  ?INFO(Title, [false]),
  ?INFO("Expect: ~w", [Expect]),
  ?INFO("Result: ~w", [Result]).
