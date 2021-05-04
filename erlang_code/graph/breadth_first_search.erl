-module(breadth_first_search).


-export([test/0]).

% rf(vertex_record).
% rd(vertex_record, {vertex,edges,status,period,dist}).
-record(vertex_record, {vertex,edges,status,period,dist}).

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
  Route = bfs_visit(L, start),
  show_routes(Route),
  ok.

bfs_visit(Nodes, Target) ->
  Queue = queue:new(),
  Queue1 = queue:in(Target, Queue),
  bfs_visit(Nodes, Target, Queue1).

bfs_visit(Nodes, Target, Queue) ->
  Node = queue:head(Queue),
  Edges = get_endges(Nodes, Node),
  {Nodes1, Queue1} = bfs_visit(Nodes, Node, Queue, Edges),
  case queue:len(Queue1) of
    0 ->  Nodes1;
    _  -> bfs_visit(Nodes1, Target, Queue1)
  end.

-spec bfs_visit(list(), integer() | stop | start, list(), list()) -> {list(), list()}.
bfs_visit(Nodes, Target, Queue, []) -> 
  {_, Queue1} = queue:out(Queue),
  Nodes1 = set_status(Nodes, Target, black),
  {Nodes1, Queue1};
bfs_visit(Nodes, Target, Queue, Edges) when is_list(Edges) ->
  [H|T]  = Edges,
  {Nodes1, Queue1} = bfs_visit(Nodes, Target, Queue, H),
  bfs_visit(Nodes1, Target, Queue1, T);
bfs_visit(Nodes, Target, Queue, Edge) ->
  case get_status(Nodes, Edge) of
    white ->
      Dist = get_dist(Nodes, Target),
      Nodes1 = set_dist(Nodes, Edge, Dist + 1),
      Nodes2 = set_period(Nodes1, Edge, Target),
      Nodes3 = set_status(Nodes2, Edge, gray),
      Queue1 = queue:in(Edge, Queue),
      {Nodes3, Queue1};
    _ -> 
      {Nodes, Queue}
  end.

get_dist(Nodes, TargetVertex) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  TargetNode#vertex_record.dist.

get_status(Nodes, TargetVertex) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  TargetNode#vertex_record.status.

get_endges(Nodes, TargetVertex) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  TargetNode#vertex_record.edges.

set_dist(Nodes, TargetVertex, Dist) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{dist=Dist} | 
    OtherNodes
  ].

set_status(Nodes, TargetVertex, Status) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{status=Status} | 
    OtherNodes
  ].

set_period(Nodes, TargetVertex, Period) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{period=Period} | 
    OtherNodes
  ].

get_model_data(1) ->
  [
    #vertex_record{vertex=start, edges=[1, 6, 8]      ,status=white, period=null, dist=0},
    #vertex_record{vertex=1,     edges=[start, 2, 3]  ,status=white, period=null, dist=0},
    #vertex_record{vertex=6,     edges=[start, 5, 7]  ,status=white, period=null, dist=0},
    #vertex_record{vertex=8,     edges=[start, 7, 14] ,status=white, period=null, dist=0},
    #vertex_record{vertex=2,     edges=[1, 11, 10]    ,status=white, period=null, dist=0},
    #vertex_record{vertex=3,     edges=[1, 4, 12]     ,status=white, period=null, dist=0},
    #vertex_record{vertex=5,     edges=[4, 6, 9]      ,status=white, period=null, dist=0},
    #vertex_record{vertex=4,     edges=[3, 5, 13]     ,status=white, period=null, dist=0},
    #vertex_record{vertex=7,     edges=[6, 8, 9]      ,status=white, period=null, dist=0},
    #vertex_record{vertex=9,     edges=[5, 7, stop]   ,status=white, period=null, dist=0},
    #vertex_record{vertex=10,    edges=[2]            ,status=white, period=null, dist=0},
    #vertex_record{vertex=11,    edges=[2]            ,status=white, period=null, dist=0},
    #vertex_record{vertex=12,    edges=[3]            ,status=white, period=null, dist=0},
    #vertex_record{vertex=13,    edges=[4]            ,status=white, period=null, dist=0},
    #vertex_record{vertex=14,    edges=[8]            ,status=white, period=null, dist=0},
    #vertex_record{vertex=stop,  edges=[9]            ,status=white, period=null, dist=0}
  ].


show_routes(Nodes)  ->
  Route = build_routes(Nodes, stop, []),
  ?OUTPUT_INFO("show_routes/1 - route: ~w", [Route]).
build_routes(_, start, Dist)  -> [start | Dist];
build_routes(Nodes, TargetVertex, Dist)  ->
  Dist1 = [TargetVertex | Dist],
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  build_routes(Nodes, TargetNode#vertex_record.period, Dist1).
