-module(dijkstras_algorithm).

-export([test/0]).

% dist is priority
% rf(vertex_record).
% rd(vertex_record, {vertex,edges,status,period,dist}).
-record(vertex_record, {vertex,edges,period,dist}).
% rf(edge_record).
% rd(edge_record, {vertex,edges,status,period,dist}).
-record(edge_record, {vertex,weight}).
% rf(priority_record).
% rd(priority_record, {vertex,edges,status,period,dist}).
-record(priority_record, {vertex, dist}).

-define(OUTPUT_DEBUG(S), io:fwrite("[DEBUG] dijkstras_algorithm: " ++ S ++ "~n")).
-define(OUTPUT_DEBUG(S, Args), io:fwrite("[DEBUG] dijkstras_algorithm: " ++ S ++ "~n", Args)).
-define(OUTPUT_INFO(S), io:fwrite("[INFO] dijkstras_algorithm: " ++ S ++ "~n")).
-define(OUTPUT_INFO(S, Args), io:fwrite("[INFO] dijkstras_algorithm: " ++ S ++ "~n", Args)).
-define(MAX_DIST, 9999).

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
  Route = single_source_shortest(L, start),
  show_routes(Route),
  ok.

single_source_shortest(Nodes, StartVertex) -> 
  Nodes1 = set_dist(Nodes, StartVertex, 0),
  PQ = create_fist_priority_list(Nodes1),
  show_pq(PQ),
  {Nodes2, _} = calc_shortest_path(Nodes1, PQ),
  Nodes2.

calc_shortest_path(Nodes, []) -> {Nodes, []};
calc_shortest_path(Nodes, PQ) ->
  {Nodes1, PQ1} = case length(PQ) of
    0 -> {Nodes, PQ};
    _ ->
      {Vertex, RetainPQ} = get_min(PQ),
      ?OUTPUT_DEBUG("calc_shortest_path/2 - vertex: ~w", [Vertex]),
      Edges = get_edges(Nodes, Vertex),
      calc_shortest_path(Nodes, RetainPQ, Vertex, Edges)
  end,
  calc_shortest_path(Nodes1, PQ1).
calc_shortest_path(Nodes, PQ, _, [])  -> {Nodes, PQ};
calc_shortest_path(Nodes, PQ, MinVertex, Edges) ->
  [Edge|RetainEdges] = Edges,
  Weight = get_weight(Nodes, MinVertex, Edge#edge_record.vertex),
  MinVertexDist = get_dist(Nodes, MinVertex),
  EdgeDist = get_dist(Nodes, Edge#edge_record.vertex),
  Length = MinVertexDist + Weight,
  {Nodes1, PQ1} = case Length < EdgeDist of
    true ->
      DecreasePQ = decrease_priority(PQ, Edge#edge_record.vertex, Length),
      SetDistNodes = set_dist(Nodes, Edge#edge_record.vertex, Length),
      SetPeriodNodes = set_period(SetDistNodes, Edge#edge_record.vertex, MinVertex),
      {SetPeriodNodes, DecreasePQ};
    false -> {Nodes, PQ}
  end,
  calc_shortest_path(Nodes1, PQ1, MinVertex, RetainEdges).

create_fist_priority_list(Nodes) ->
  create_fist_priority_list(Nodes, []).
create_fist_priority_list([], PQ) ->
  PQ;
create_fist_priority_list(Nodes, PQ) ->
  [Rec|Retain] = Nodes,
  PQ1 = [ 
    #priority_record{
       vertex=Rec#vertex_record.vertex, 
       dist=Rec#vertex_record.dist} | PQ],
  create_fist_priority_list(Retain, PQ1).

-spec set_period(list(), integer() | start | stop, integer() | start | stop) -> list().
set_period(Nodes, TargetVertex, Period) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{period=Period} | 
    OtherNodes
  ].

-spec set_dist(list(), integer() | stop | start, integer()) -> list().
set_dist(Nodes, TargetVertex, Dist) ->
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  OtherNodes = lists:keydelete(TargetVertex, #vertex_record.vertex, Nodes),
  [ 
    TargetNode#vertex_record{dist=Dist} | 
    OtherNodes
  ].

get_dist(Nodes, Vertex) ->
  Node = lists:keyfind(Vertex, #vertex_record.vertex, Nodes),
  Node#vertex_record.dist.

get_edges(Nodes, Vertex) ->
  ?OUTPUT_DEBUG("get_edges: vertex: ~w", [Vertex]),
  Node = lists:keyfind(Vertex, #vertex_record.vertex, Nodes),
  Node#vertex_record.edges.

-spec get_weight(list(), integer() | start | stop, integer() | start | stop) -> integer().
get_weight(Nodes, TargetVertex, TargetEdge) ->
  Node = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  Edge = lists:keyfind(TargetEdge, #edge_record.vertex, Node#vertex_record.edges),
  Edge#edge_record.weight.

-spec get_min(list()) -> {integer() | start | stop, list()}.
get_min(PQ) -> 
  [Rec|_] = PQ,
  get_min(PQ, Rec).
get_min(PQ, MinVertex) ->
  MinVertexDist = MinVertex#priority_record.dist,
  Lowers = [
    #priority_record{vertex=Vertex, dist=Dist} || 
    #priority_record{vertex=Vertex, dist=Dist} <- PQ , 
      Dist < MinVertexDist],
  case length(Lowers) of
    0 -> 
      show_pq(PQ),
      PQ1 = lists:keydelete(
        MinVertex#priority_record.vertex,
        #priority_record.vertex,
        PQ),
      ?OUTPUT_DEBUG("get_min/2 - min vertex: ~w", [MinVertex#priority_record.vertex]),
      show_pq(PQ1),
      {MinVertex#priority_record.vertex, PQ1};
    _ -> 
      [Lower|_] = Lowers,
      get_min(PQ, Lower)
  end. 

decrease_priority(PQ, Vertex, Priority) ->
  ?OUTPUT_DEBUG("decrease_priority/3 - vertex: ~w, priority: ~w", [Vertex, Priority]),
  TargetNode = lists:keyfind(Vertex, #priority_record.vertex, PQ),
  ?OUTPUT_DEBUG("decrease_priority/3 - node: ~w", [TargetNode]),
  OtherNodes = lists:keydelete(Vertex, #priority_record.vertex, PQ),
  [ 
    TargetNode#priority_record{dist=Priority} | 
    OtherNodes
  ].

get_model_data(1) ->
  [
    #vertex_record{
       vertex=start,
       edges=[
         #edge_record{vertex=1, weight=6},
         #edge_record{vertex=2, weight=8},
         #edge_record{vertex=stop, weight=18}],
       period=null,
       dist=0},
    #vertex_record{
       vertex=1,
       edges=[
         #edge_record{vertex=4, weight=11}],
       period=null,
       dist=?MAX_DIST},
    #vertex_record{
       vertex=2,
       edges=[
         #edge_record{vertex=stop, weight=9}],
       period=null,
       dist=?MAX_DIST},
    #vertex_record{
       vertex=stop,
       edges=[],
       period=null,
       dist=?MAX_DIST},
    #vertex_record{
       vertex=4,
       edges=[
         #edge_record{vertex=5, weight=3}],
       period=null,
       dist=?MAX_DIST},
    #vertex_record{
       vertex=5,
       edges=[
         #edge_record{vertex=stop, weight=4},
         #edge_record{vertex=2, weight=7}],
       period=null,
       dist=?MAX_DIST}
  ].

show_routes(Nodes)  ->
  Route = build_routes(Nodes, stop, []),
  ?OUTPUT_INFO("show_routes/1 - route: ~w", [Route]).
build_routes(_, start, Dist)  -> [start | Dist];
build_routes(Nodes, TargetVertex, Dist)  ->
  Dist1 = [TargetVertex | Dist],
  TargetNode = lists:keyfind(TargetVertex, #vertex_record.vertex, Nodes),
  build_routes(Nodes, TargetNode#vertex_record.period, Dist1).
  
show_pq([]) ->
  ?OUTPUT_DEBUG("priority queue: empty");
show_pq(PQ) ->
  [H|T] = PQ,
  show_pq(T, [H#priority_record.vertex]).
show_pq([], Vertexes) ->
  ?OUTPUT_DEBUG("priority queue: ~w", [Vertexes]);
show_pq(PQ, Vertexes) ->
  [H|T] = PQ,
  show_pq(T, [H#priority_record.vertex|Vertexes]).
