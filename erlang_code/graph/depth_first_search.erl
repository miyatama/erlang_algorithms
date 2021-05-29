-module(depth_first_search).

-export([test/0]).

% rf(vertex_record).
% rd(vertex_record, {vertex,edges,status,period}).
-record(vertex_record, {vertex,edges,status,period}).

-define(OUTPUT_DEBUG(S), io:fwrite("[DEBUG] depth_first_search: " ++ S ++ "~n")).
-define(OUTPUT_DEBUG(S, Args), io:fwrite("[DEBUG] depth_first_search: " ++ S ++ "~n", Args)).
-define(OUTPUT_INFO(S), io:fwrite("[INFO] depth_first_search: " ++ S ++ "~n")).
-define(OUTPUT_INFO(S, Args), io:fwrite("[INFO] depth_first_search: " ++ S ++ "~n", Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
  search(get_model_data(1)),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search(L) ->
  Route = dfs_visit(L, start),
  ?OUTPUT_DEBUG("create_route/2 - vertex: ~w", [Route]),
  show_routes(Route),
  ok.

-spec dfs_visit(list(T), integer() | start | stop) -> list(T).
dfs_visit(Nodes, TargetVertex) ->
  Nodes1 = set_node_status(Nodes, TargetVertex, gray),
  Edges = get_node_edges(Nodes, TargetVertex),
  Edges1 = choose_white_nodes(Nodes, Edges),
  dfs_visit(Nodes1, TargetVertex, Edges1).

-spec dfs_visit(list(T), integer() | start | stop, list(integer() | start | stop)) -> list(T).
dfs_visit(Nodes, TargetVertex, []) -> 
  set_node_status(Nodes, TargetVertex, black);
dfs_visit(Nodes, TargetVertex, Edges) ->
  [H|T] = Edges,
  Status = get_node_status(Nodes, H),
  ?OUTPUT_DEBUG("dfs_visit/3 - vertex: ~w, status; ~w", [H, Status]),
  Nodes1 = case Status of
    white ->
      set_node_period(Nodes, H, TargetVertex);
    _ -> Nodes
  end,
  Nodes2 = dfs_visit(Nodes1, H),
  dfs_visit(Nodes2, TargetVertex, T).

choose_white_nodes(_, []) -> [];
choose_white_nodes(Nodes, Edges) ->
  [H|T] = Edges,
  Targets = case get_node_status(Nodes, H) of
    white -> [H];
    _ -> []
  end,
  lists:append(Targets, choose_white_nodes(Nodes, T)).


get_node_edges(Nodes, TargetVertex) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  TargetNode#vertex_record.edges.

get_node_status(Nodes, TargetVertex) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  TargetNode#vertex_record.status.

set_node_status(Nodes, TargetVertex, Status) ->
  ?OUTPUT_DEBUG("set_node_status/3 - target vertex: ~w", [TargetVertex]),
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{status=Status} | 
    OtherNodes
  ].

set_node_period(Nodes, TargetVertex, Period) ->
  ?OUTPUT_DEBUG("set_node_period/3 - vertex: ~w, replaced: ~w", [TargetVertex, Period]),
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{period=Period} | 
    OtherNodes
  ].

get_model_data(1) ->
  [
    #vertex_record{vertex=start, edges=[1, 6, 8]      ,status=white, period=null},
    #vertex_record{vertex=1,     edges=[start, 2, 3]  ,status=white, period=null},
    #vertex_record{vertex=6,     edges=[start, 5, 7]  ,status=white, period=null},
    #vertex_record{vertex=8,     edges=[start, 7, 14] ,status=white, period=null},
    #vertex_record{vertex=2,     edges=[1, 11, 10]    ,status=white, period=null},
    #vertex_record{vertex=3,     edges=[1, 4, 12]     ,status=white, period=null},
    #vertex_record{vertex=5,     edges=[4, 6, 9]      ,status=white, period=null},
    #vertex_record{vertex=4,     edges=[3, 5, 13]     ,status=white, period=null},
    #vertex_record{vertex=7,     edges=[6, 8, 9]      ,status=white, period=null},
    #vertex_record{vertex=9,     edges=[5, 7, stop]   ,status=white, period=null},
    #vertex_record{vertex=10,    edges=[2]            ,status=white, period=null},
    #vertex_record{vertex=11,    edges=[2]            ,status=white, period=null},
    #vertex_record{vertex=12,    edges=[3]            ,status=white, period=null},
    #vertex_record{vertex=13,    edges=[4]            ,status=white, period=null},
    #vertex_record{vertex=14,    edges=[8]            ,status=white, period=null},
    #vertex_record{vertex=stop,  edges=[9]            ,status=white, period=null}
  ].

show_routes(Nodes) ->
  Route = create_route(Nodes, stop),
  Route1 = lists:reverse(Route),
  ?OUTPUT_INFO("show_routes: ~w", [Route1]).

create_route(_, start) -> [start];
create_route(Nodes, TargetVertex) ->
  ?OUTPUT_DEBUG("create_route/2 - vertex: ~w", [TargetVertex]),
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  NextTarget = TargetNode#vertex_record.period,
  lists:append([TargetVertex], create_route(Nodes, NextTarget)).
