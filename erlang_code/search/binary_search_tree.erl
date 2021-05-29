-module(binary_search_tree).

-export([test/0]).

-import(search_funcs, [
  get_search_list/1,
  compare/2,
  search_test/2,
  output_debug/2,
  output_debug/3,
  output_info/2,
  output_info/3]).

-define(OUTPUT_DEBUG(S, Args), output_debug(binary_search_tree, S, Args)).
-define(OUTPUT_DEBUG(S), output_debug(binary_search_tree, S)).
-define(OUTPUT_INFO(S, Args), output_info(binary_search_tree, S, Args)).
-define(OUTPUT_INFO(S), output_info(binary_search_tree, S)).

-include("search_record.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  bucket_sort_test(),
  create_tree_test(),
  search_test(
    binary_search_tree, 
    fun(T, L) -> search(T, L) end),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(T, list(T)) -> not_found | T.
search(_, []) -> not_found;
search(Value, L) ->
  ?OUTPUT_DEBUG("search/2: ~w", [start]),
  Tree = create_tree(L),
  ?OUTPUT_DEBUG("search/2: ~w", [create_tree_finish]),
  search_value(Value, Tree).

-spec search_value(T, list(T)) -> not_found | T.
search_value(Value, Tree) ->
  search_value(Value, Tree, 1).
search_value(_, Tree, Index) when length(Tree) < Index -> not_found;
search_value(Value, Tree, Index) ->
  [_, Node2Index, Node3Index] = get_node_indexes(Tree, Index),
  ?OUTPUT_DEBUG("search_value/3: comapre ~w, ~w", [Value, list_nth(Index, Tree)]),
  case compare(Value, list_nth(Index, Tree)) of
    greater_than ->
      search_value(Value, Tree, Node3Index);
    lower_than ->
      search_value(Value, Tree, Node2Index);
    equal_to -> Value
  end.

-spec bucket_sort(list(T)) -> list(T).
bucket_sort(L) ->
  bucket_sort(L, maps:new()).
bucket_sort([], Map) ->
  generate_list_from_map(Map, [], 1);
bucket_sort(L, Map) ->
  [#item{value=Value}|T] = L,
  Map1 = case maps:is_key(Value, Map) of
    false ->
      maps:put(Value, [#item{value=Value}], Map);
    true ->
      Map
  end,
  bucket_sort(T, Map1).

-spec generate_list_from_map(map(), list(T), integer()) -> list(T).
generate_list_from_map(Map, L, Value) ->
  {L1, Map1} = case maps:is_key(Value, Map) of
    true -> 
      RemovedMap = maps:remove(Value, Map),
      {[#item{value=Value}], RemovedMap};
    false ->  {[], Map}
  end,
  L2 = lists:append(L, L1),
  case maps:size(Map1) =< 0 of
    true -> L2;
    false -> generate_list_from_map(Map1, L2, Value + 1)
  end.

% create_tree
-spec create_tree(list(T)) -> list(T).
create_tree([]) -> [];
create_tree(L) ->
  L1 = bucket_sort(L),
  MaxDepthIndex = get_max_depth_index(L1),
  ?OUTPUT_DEBUG("create_tree/1 - length: ~w, max depth index: ~w", [length(L1), MaxDepthIndex]),
  create_tree(
    L1, 
    [], 
    0,
    MaxDepthIndex ).

-spec create_tree(list(T), list(T), integer(), integer()) -> list(T).
create_tree([], DistList, _, _)  -> DistList;
create_tree(SrcList, _, _, _) when length(SrcList) =< 1 -> SrcList;
create_tree(_, DistList, DepthIndex, MaxDepthIndex) when DepthIndex >= MaxDepthIndex -> 
  ?OUTPUT_DEBUG(
    "create_tree/4 - depth index: ~w, max depth index: ~w", 
    [DepthIndex, MaxDepthIndex]),
  DistList;
create_tree(SrcList, [], DepthIndex, MaxDepthIndex) ->
  ?OUTPUT_DEBUG(
    "create_tree/4 - depth index: ~w, max depth index: ~w", 
    [DepthIndex, MaxDepthIndex]),
  DistList = [get_center_node(SrcList)],
  create_tree(SrcList, DistList, DepthIndex + 1, MaxDepthIndex);
create_tree(SrcList, DistList, DepthIndex, MaxDepthIndex)  ->
  % left node
  DistList1 = lists:append(
    DistList,
    get_child_nodes(SrcList, DepthIndex)),
  create_tree(SrcList, DistList1, DepthIndex + 1, MaxDepthIndex).

get_child_nodes(L, DepthIndex) ->
  SplitedList = split_depth_list(L, DepthIndex),
  ?OUTPUT_DEBUG("get_child_nodes/2 - splited list: ~w", [SplitedList]),
  get_splited_lists_center_nodes(SplitedList).

split_depth_list(L, 0) -> [L];
split_depth_list(L, _) when length(L) =< 1 -> [[null], [null]];
split_depth_list(L, _) when length(L) =< 2 -> [[list_nth(1, L)], [null]];
split_depth_list(L, DepthIndex) ->
  CenterIndex = get_center_index(L),
  LeftLists = split_depth_list(
    lists:sublist(
      L, 
      CenterIndex - 1), 
    DepthIndex - 1),
  lists:append(
    LeftLists,
    split_depth_list(
      lists:sublist(
        L, 
        CenterIndex + 1, length(L) - CenterIndex), 
      DepthIndex - 1)).

-spec get_splited_lists_center_nodes(list(list(T))) -> list(T).
get_splited_lists_center_nodes([]) -> [];
get_splited_lists_center_nodes(SplitedLists) ->
  [H|T] = SplitedLists,
  Value = get_center_node(H),
  lists:append([Value], get_splited_lists_center_nodes(T)).

-spec get_max_depth_index(list()) -> integer().
get_max_depth_index(L) ->
  get_max_depth_index(L, 1).
get_max_depth_index(L, Index) ->
  case length(L) < math:pow(2, Index) of
    true -> Index;
    false -> get_max_depth_index(L, Index + 1) 
  end.
  
-spec get_center_node(list(T)) -> T.
get_center_node(L) ->
  list_nth(get_center_index(L), L).

get_center_index(L) ->
  trunc(math:floor(length(L) / 2) + 1).

get_node_indexes(L, I) when I == 1 ->
  RootNodeIndex = 1,
  LeftNodeIndex = 2,
  RightNodeIndex = 3,
  LeftNodeIndex1 = case length(L) < LeftNodeIndex of
    true -> null;
      _ -> LeftNodeIndex 
    end,
  RightNodeIndex1 = case length(L) < RightNodeIndex of
    true -> null;
      _ -> RightNodeIndex 
    end,
  [
    RootNodeIndex,
    LeftNodeIndex1,
    RightNodeIndex1
  ];
get_node_indexes(L, I) -> 
  RootNodeIndex = I,
  LeftNodeIndex = I * 2,
  RightNodeIndex = I * 2 + 1,
  LeftNodeIndex1 = case length(L) < LeftNodeIndex of
    true -> null;
      _ -> LeftNodeIndex 
    end,
  RightNodeIndex1 = case length(L) < RightNodeIndex of
    true -> null;
      _ -> RightNodeIndex 
    end,
  [
    RootNodeIndex,
    LeftNodeIndex1,
    RightNodeIndex1
  ].

-spec list_nth(integer(), list(T)) -> T | null.
list_nth(I, L) when length(L) < I -> null;
list_nth(I, L) -> lists:nth(I, L).

-spec list_equal(list(T), list(T)) -> false | true.
list_equal(L1, L2) when length(L1) /= length(L2) -> false;
list_equal([], []) -> true;
list_equal(L1, L2) ->
  [V1|T1]   = L1,
  [V2|T2]   = L2,
  case value_equal(V1, V2) of
    true -> list_equal(T1, T2);
    _ -> false
  end.

value_equal(null, null) -> true;
value_equal(_, null) -> false;
value_equal(null, _) -> false;
value_equal(V1, V2) ->
  #item{value=Value1} = V1,
  #item{value=Value2} = V2,
  case Value1 of
    Value2 -> true;
    _ -> false
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bucket_sort_test() ->
  % empty
  Expect1 = [],
  Result1 = bucket_sort([]),
  show_list_test_result(
    "bucket_sort_test() case 001 - empty list: ~w",
    Expect1,
    Result1),
  % one factor
  Expect2 = [
    #item{value=100}],
  Result2 = bucket_sort([
    #item{value=100}]),
  show_list_test_result(
    "bucket_sort_test() case 002 - one factor: ~w",
    Expect2,
    Result2),
  % two factor
  Expect3 = [
    #item{value=100},
    #item{value=101}],
  Result3 = bucket_sort([
    #item{value=100},
    #item{value=101}]),
  show_list_test_result(
    "bucket_sort_test() case 003 - two factor: ~w",
    Expect3,
    Result3),
  % two factor reverse
  Expect4 = [
    #item{value=100},
    #item{value=101}],
  Result4 = bucket_sort([
    #item{value=101},
    #item{value=100}]),
  show_list_test_result(
    "bucket_sort_test() case 004 - two factor reverse: ~w",
    Expect4,
    Result4),
  % tree factor
  Expect5 = [
    #item{value=100},
    #item{value=101},
    #item{value=102}],
  Result5 = bucket_sort([
    #item{value=100},
    #item{value=101},
    #item{value=102}]),
  show_list_test_result(
    "bucket_sort_test() case 005 - three factor: ~w",
    Expect5,
    Result5),
  % tree factor reverse
  Expect6 = [
    #item{value=100},
    #item{value=101},
    #item{value=102}],
  Result6 = bucket_sort([
    #item{value=102},
    #item{value=101},
    #item{value=100}]),
  show_list_test_result(
    "bucket_sort_test() case 006 - three factor reverse: ~w",
    Expect6,
    Result6),
  % duplicate
  Expect7 = [
    #item{value=100},
    #item{value=101},
    #item{value=102}],
  Result7 = bucket_sort([
    #item{value=102},
    #item{value=101},
    #item{value=100},
    #item{value=102},
    #item{value=101},
    #item{value=100}]),
  show_list_test_result(
    "bucket_sort_test() case 007 - duplicate: ~w",
    Expect7,
    Result7),
  ok.

create_tree_test() ->
  % empty
  Expect1 = [],
  Result1 = create_tree([]),
  show_list_test_result("create_tree_test() case 001 - empty list: ~w", Expect1, Result1),
  % one factor
  Expect2 = [
    #item{value=100}],
  Result2 = create_tree([
    #item{value=100}]),
  show_list_test_result("create_tree_test() case 002 - one factor: ~w", Expect2, Result2),
  % two factor
  Expect3 = [
    #item{value=101}, 
    #item{value=100},
    null],
  Result3 = create_tree([
    #item{value=100}, 
    #item{value=101}]),
  show_list_test_result("create_tree_test() case 003 - two factor: ~w", Expect3, Result3),
  % tree factor
  Expect4 = [
    #item{value=102}, 
    #item{value=101},
    #item{value=103}],
  Result4 = create_tree([
    #item{value=103}, 
    #item{value=102}, 
    #item{value=101}]),
  show_list_test_result("create_tree_test() case 004 - three factor: ~w", Expect4, Result4),
  % duplicate factor
  Expect5 = [
    #item{value=101}, 
    #item{value=100}, 
    #item{value=102}],
  Result5 = create_tree([
    #item{value=100}, 
    #item{value=100}, 
    #item{value=100}, 
    #item{value=100}, 
    #item{value=102}, 
    #item{value=101}]),
  show_list_test_result("create_tree_test() case 005 - duplicate factor: ~w", Expect5, Result5),
  % 5 factor
  Expect6 = [
    #item{value=102}, 
    #item{value=101}, 
    #item{value=104}, 
    #item{value=100}, 
    null,
    #item{value=103},
    null],
  Result6 = create_tree([
    #item{value=100}, 
    #item{value=101}, 
    #item{value=102}, 
    #item{value=103}, 
    #item{value=104}]),
  show_list_test_result("create_tree_test() case 006 - 5 factor: ~w", Expect6, Result6),
  ok.

show_list_test_result(Title, Expect, Result) ->
  case list_equal(Expect, Result) of
    true ->
      ?OUTPUT_INFO(Title, [true]);
    _ ->
      ?OUTPUT_INFO(Title, [false]),
      ?OUTPUT_INFO("~nExpect: ~w~nResult: ~w", [Expect, Result])
  end.
