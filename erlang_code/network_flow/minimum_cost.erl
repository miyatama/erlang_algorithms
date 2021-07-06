-module(minimum_cost).

-export([test/0]).

-import(ford_fullkerson, 
  [process_argpath/2,
    equal_edges/2,
    find_edge/3,
    add_argumenting_path/4]).

-include("network_flow.hrl").

-record(priority_queue_record, {vertex, dist}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
  case (?LOG_LEVEL) =< 1 of
    true -> io:fwrite("[DEBUG] minimum_cost: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] minimum_cost: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] minimum_cost: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] minimum_cost: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] minimum_cost: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] minimum_cost: " ++ S ++ "~n", Args);
    _ -> ok
  end).


-define(MAX_DIST,  9999999).
-define(MAX_DELTA, 9999999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  test(1),
  test(2),
  test(3),
  test(4),
  test(5),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(TestCase) ->
  Graph = generate_initial_graph(TestCase),
  ResultGraph = compute(Graph),
  ?OUTPUT_INFO(
    "test ~w",
    [TestCase]),
  ExpectGraph = generate_expect_graph(TestCase),
  show_result("test case ~w: ~w", TestCase, ExpectGraph, ResultGraph).

-spec compute(list(edge)) -> list(edge).
compute(Graph) -> 
  show_edges(Graph),
  ArguPath = generate_argumenting_path(Graph),
  ?OUTPUT_DEBUG("compute/1 - arg paths: ~w", [ArguPath]),
  compute(Graph, ArguPath).


-spec compute(list(edge), list(argpath)) -> list(edge).
compute(Graph, []) ->  
  ?OUTPUT_DEBUG("compute/2 - complete"),
  Graph;
compute(Graph, ArgPaths) ->  
  NewGraph = process_argpath(Graph, ArgPaths),
  compute(NewGraph).

-spec generate_argumenting_path(
    list(edge)
  ) -> list(argpath).
generate_argumenting_path(Edges) -> 
  PriorityQueue = [
    #priority_queue_record{vertex=source, dist=0}],
  Dists = generate_initial_dist(Edges),
  SetSourceDists = maps:put(source, 0, Dists), 
  ArgPaths = [],
  generate_argumenting_path(
    Edges, 
    PriorityQueue,
    SetSourceDists, 
    ArgPaths).

% 3: map() -> [{atom(), integer()}]
-spec generate_argumenting_path(
    list(edge),
    list(priority_queue_record),
    map(),
    list(argumenting_record)
  ) -> list(argumenting_record).
generate_argumenting_path(_, [], Dists, ArgPaths) ->
  Dist = maps:get(sink, Dists),
  case Dist == ?MAX_DIST of
    true -> [];
    false -> ArgPaths
  end;
generate_argumenting_path(Edges, PriorityQueue, Dists, ArgPaths) ->
  {Smallest, PriorityQueue2} = get_smallest_id_priority_queue(PriorityQueue),
  ?OUTPUT_DEBUG("generate_argumenting_path/5 - smallest vertex: ~w", [Smallest#priority_queue_record.vertex]),
  generate_argumenting_path(Edges, PriorityQueue2, Dists, 
    ArgPaths,Smallest#priority_queue_record.vertex).

-spec generate_argumenting_path(
      list(edge),
    list(priority_queue_record),
    map(),
    list(argumenting_record),
    atom()
  ) -> list(argumenting_record).
generate_argumenting_path(Edges, _, Dists, ArgPaths, sink) ->
  generate_argumenting_path(Edges, [], Dists, ArgPaths);
generate_argumenting_path(Edges, PriorityQueue, Dists, 
  ArgPaths, PriorityVertex) ->
  Vertexes = generate_vertex_from_edges(Edges),
  RemovedTargetVertexes = remove_vertexes([source, PriorityVertex], Vertexes),
  generate_argumenting_path(Edges, PriorityQueue, Dists,  
    ArgPaths, PriorityVertex,RemovedTargetVertexes).

-spec generate_argumenting_path(
      list(edge),
    list(priority_queue_record),
    map(),
    list(argumenting_record),
    atom(),
    list(atom())
  ) -> list(argumenting_record).
generate_argumenting_path(Edges, PriorityQueue, Dists, ArgPaths, _, []) ->
  generate_argumenting_path( Edges, PriorityQueue, Dists, ArgPaths);
generate_argumenting_path(Edges, PriorityQueue, Dists, 
    ArgPaths, CurrentVertex, Vertexes) ->
  [NextVertex | VertexesRetain] = Vertexes,

  ?OUTPUT_DEBUG("generate_argumenting_path/6 - next vertex: ~w", [NextVertex]),
  % forward
  {PriorityQueue2, 
    Dists2, 
    ArgPaths2} = 
    generate_argumenting_path_forward(Edges, PriorityQueue, Dists, 
      ArgPaths, CurrentVertex,NextVertex),
  ?OUTPUT_DEBUG("generate_argumenting_path/6 - ~w", [after_forward]),
  show_dists(Dists2),
  % backward
  {PriorityQueue3, 
  Dists3, 
  ArgPaths3} = 
    generate_argumenting_path_backward(Edges, PriorityQueue2, Dists2, 
      ArgPaths2, CurrentVertex, NextVertex),
  ?OUTPUT_DEBUG("generate_argumenting_path/6 - ~w", [after_backward]),
  show_dists(Dists3),
  generate_argumenting_path(Edges, PriorityQueue3, Dists3, 
    ArgPaths3,CurrentVertex,VertexesRetain).

-spec generate_argumenting_path_forward(
    list(edge),
    list(priority_queue_record),
    map(),
    list(argpath),
    atom(),
    atom()
  ) ->  {
        list(priority_queue_record),
        map(),
        list(argpath)}.
generate_argumenting_path_forward(
      Edges, PriorityQueue, Dists, 
      ArgPaths, CurrentVertex, NextVertex) ->
  Edge = find_edge(CurrentVertex, NextVertex, Edges),
  {ExistsEdge, NewDist, NotFull} = case Edge of
      null -> 
        {false, -1, false};
      _ -> 
        {
          true,
          maps:get(CurrentVertex, Dists) + Edge#edge.cost, 
          (Edge#edge.flow < Edge#edge.capacity)
        }
    end,
  NextDist = maps:get(NextVertex, Dists),
  UpdateDist = (NewDist >= 0) and (NewDist < NextDist),
  case {ExistsEdge, NotFull, UpdateDist} of
    {true, true, true} ->
      % new argumenting path
      NewArgPaths = add_argumenting_path(NextVertex, CurrentVertex,forward, ArgPaths),
      % new dists
      NewDists = maps:put(NextVertex, NewDist, Dists),
      % new priority queue
      NewPriorityQueue = insert_priority_queue(NextVertex, NewDist, PriorityQueue),
      {
        NewPriorityQueue,
        NewDists,
        NewArgPaths
      };
    _ ->
      % no change
      {
        PriorityQueue, 
        Dists, 
        ArgPaths
      }
  end.
  
-spec generate_argumenting_path_backward(
    list(edge),
    list(priority_queue_record),
    map(),
    list(argpath),
    atom(),
    atom()
  ) ->  {
        list(priority_queue_record),
        map(),
        list(inqueue_record),
        list(argpath)}.
generate_argumenting_path_backward(
      Edges, PriorityQueue, Dists, 
      ArgPaths, CurrentVertex, NextVertex) ->
  Edge = find_edge(NextVertex, CurrentVertex, Edges),
  {ExistsEdge, NewDist, NonEmpty} = case Edge of
      null -> {false, -1, false};
      _ ->
        {
          true, 
          maps:get(CurrentVertex, Dists) - Edge#edge.cost, 
          (Edge#edge.flow > 0)
        }
    end,
  UpdateDist = (NewDist >= 0) and (NewDist < maps:get(NextVertex, Dists)),
  case {ExistsEdge, NonEmpty, UpdateDist} of
    {true, true, true} ->
      % new argmenting path
      NewArgPaths = add_argumenting_path(NextVertex, CurrentVertex, backward, ArgPaths),
      % new dists
      NewDists = maps:put(NextVertex, NewDist, Dists),
      % new primary queue
      NewPriorityQueue = insert_priority_queue(NextVertex, NewDist, PriorityQueue),
      {
        NewPriorityQueue,
        NewDists,
        NewArgPaths
      };
    _ ->
      {
        PriorityQueue,
        Dists,
        ArgPaths
      }
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% graph functions
generate_vertex_from_edges(Edges) ->
  generate_vertex_from_edges(Edges, []).
generate_vertex_from_edges([], ResultVertexes) -> ResultVertexes;
generate_vertex_from_edges(Edges, ResultVertexes) ->
  [Edge | Retain] = Edges,
  Result2 = case lists:any(fun(Vertex) -> Vertex == Edge#edge.from end, ResultVertexes) of
    true -> ResultVertexes;
    false -> [Edge#edge.from] ++ ResultVertexes
  end,
  Result3 = case lists:any(fun(Vertex) -> Vertex == Edge#edge.to end, Result2) of
    true -> Result2;
    false -> [Edge#edge.to] ++ Result2
  end,
  generate_vertex_from_edges(Retain, Result3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dists function
% return map() -> {atom(), integer()}
-spec generate_initial_dist(list(edge)) -> map().
generate_initial_dist(Edges) -> 
  Map = maps:new(),
  generate_initial_dist(Edges, Map).

-spec generate_initial_dist(
    list(edge),
    map()
  ) -> map().
generate_initial_dist([], Map) ->  Map;
generate_initial_dist(Edges, Map) -> 
  [Edge | Retain] = Edges,
  #edge{from=FromVertex, to=ToVertex} = Edge,
  Map2 = case maps:is_key(FromVertex, Map) of
    true -> Map;
    _ -> 
      maps:put(FromVertex, ?MAX_DIST, Map)
  end,
  Map3 = case maps:is_key(ToVertex, Map) of
    true -> Map2;
    _ -> 
      maps:put(ToVertex, ?MAX_DIST, Map)
  end,
  generate_initial_dist(Retain, Map3).  

-spec show_dists(map()) -> ok.
show_dists(Maps) -> 
  Keys = maps:keys(Maps),
  show_dists(Keys, Maps),
  ok.
show_dists([], _) -> ok;
show_dists(Keys, Maps) ->
  [Key | Retain] = Keys,
  Value = maps:get(Key, Maps),
  ?OUTPUT_DEBUG("show_dists/2 - key: ~w, Value: ~w", [Key, Value]),
  show_dists(Retain, Maps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% priority queue function
remove_priority_queue(Rec, List) ->
  lists:keydelete(
    Rec#priority_queue_record.vertex,
    #priority_queue_record.vertex,
    List).

-spec get_smallest_id_priority_queue(
    list(priority_queue_record)
  ) -> {priority_queue_record, list(priority_queue_record)}.
get_smallest_id_priority_queue(List) -> 
  [Rec | Retain] = List,
  Smallest = get_smallest_id_priority_queue(Rec, Retain),
  RemovedList = remove_priority_queue(Smallest, List),
  {Smallest, RemovedList}.

get_smallest_id_priority_queue(Rec, []) -> Rec;
get_smallest_id_priority_queue(Rec, List) -> 
  [Rec2 | Retain] = List,
  Lower = case Rec#priority_queue_record.dist > Rec2#priority_queue_record.dist of
    true -> Rec2;
    false -> Rec
  end,
  get_smallest_id_priority_queue(Lower, Retain).

-spec exists_priority_queue(atom(), list(priority_queue_record)) -> true | false.
exists_priority_queue(_, []) -> false;
exists_priority_queue(Vertex, PriorityQueue) ->
  [Rec | Retain] = PriorityQueue,
  case Rec#priority_queue_record.vertex of
    Vertex -> true;
    _ ->
      exists_priority_queue(Vertex, Retain)
  end.

-spec decrease_priority_queue(
    atom(), 
    integer(), 
    list(priority_queue_record)
  ) -> list(priority_queue_record).
decrease_priority_queue(Vertex, Dist, PriorityQueue) ->
  [Rec | Retain] = PriorityQueue,
  case Rec#priority_queue_record.vertex of
    Vertex ->
      [
        #priority_queue_record{
           vertex=Vertex,
           dist=Dist
        }
      ] ++ Retain;
    _ -> 
      [Rec] ++ decrease_priority_queue(Vertex, Dist, Retain)
  end.

insert_priority_queue(Vertex, Dist, PriorityQueue) ->
  case exists_priority_queue(Vertex, PriorityQueue) of
    true ->
      decrease_priority_queue(Vertex, Dist, PriorityQueue);
    false ->
      [
        #priority_queue_record{
          vertex=Vertex,
          dist=Dist
        }
      ] ++ PriorityQueue
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test function
generate_initial_graph(1) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10, cost=1},
   #edge{from=v1, to=sink, flow=0, capacity=10, cost=1}
  ];

generate_initial_graph(2) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=5, cost=1},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge{from=v1, to=sink, flow=0, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=0, capacity=5, cost=1}
  ];

generate_initial_graph(3) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10, cost=30},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=11},
   #edge{from=v1, to=sink, flow=0, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=0, capacity=3, cost=1}
  ];

generate_initial_graph(4) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10, cost=5},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge{from=source, to=v3, flow=0, capacity=4, cost=1},
   #edge{from=v1, to=v4, flow=0, capacity=5, cost=5},
   #edge{from=v1, to=v2, flow=0, capacity=10, cost=5},
   #edge{from=v2, to=v4, flow=0, capacity=2, cost=1},
   #edge{from=v2, to=v3, flow=0, capacity=10, cost=1},
   #edge{from=v3, to=v5, flow=0, capacity=11, cost=1},
   #edge{from=v4, to=sink, flow=0, capacity=7, cost=1},
   #edge{from=v5, to=sink, flow=0, capacity=11, cost=1}
  ];

generate_initial_graph(5) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=5, cost=4},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=2},
   #edge{from=source, to=v3, flow=0, capacity=5, cost=1},
   #edge{from=v1, to=v4, flow=0, capacity=5, cost=3},
   #edge{from=v2, to=v4, flow=0, capacity=5, cost=2},
   #edge{from=v3, to=v4, flow=0, capacity=5, cost=1},
   #edge{from=v4, to=sink, flow=0, capacity=11, cost=1}
  ];

generate_initial_graph(_) ->
  [].

generate_expect_graph(1) ->
  [
   #edge{from=source, to=v1, flow=10, capacity=10, cost=1},
   #edge{from=v1, to=sink, flow=10, capacity=10, cost=1}
  ];

generate_expect_graph(2) ->
  [
   #edge{from=source, to=v1, flow=5, capacity=5, cost=1},
   #edge{from=source, to=v2, flow=5, capacity=5, cost=1},
   #edge{from=v1, to=sink, flow=10, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=5, capacity=5, cost=1}
  ];

generate_expect_graph(3) ->
  [
   #edge{from=source, to=v1, flow=7, capacity=10, cost=30},
   #edge{from=source, to=v2, flow=3, capacity=5, cost=11},
   #edge{from=v1, to=sink, flow=10, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=3, capacity=3, cost=1}
  ];

generate_expect_graph(4) ->
  [
   #edge{from=source, to=v1, flow=9, capacity=10, cost=5},
   #edge{from=source, to=v2, flow=5, capacity=5, cost=1},
   #edge{from=source, to=v3, flow=4, capacity=4, cost=1},
   #edge{from=v1, to=v4, flow=5, capacity=5, cost=5},
   #edge{from=v1, to=v2, flow=4, capacity=10, cost=5},
   #edge{from=v2, to=v4, flow=2, capacity=2, cost=1},
   #edge{from=v2, to=v3, flow=7, capacity=10, cost=1},
   #edge{from=v3, to=v5, flow=11, capacity=11, cost=1},
   #edge{from=v4, to=sink, flow=7, capacity=7, cost=1},
   #edge{from=v5, to=sink, flow=11, capacity=11, cost=1}
  ];

generate_expect_graph(5) ->
  [
   #edge{from=source, to=v1, flow=1, capacity=5, cost=4},
   #edge{from=source, to=v2, flow=5, capacity=5, cost=2},
   #edge{from=source, to=v3, flow=5, capacity=5, cost=1},
   #edge{from=v1, to=v4, flow=1, capacity=5, cost=3},
   #edge{from=v2, to=v4, flow=5, capacity=5, cost=2},
   #edge{from=v3, to=v4, flow=5, capacity=5, cost=1},
   #edge{from=v4, to=sink, flow=11, capacity=11, cost=1}
  ];

generate_expect_graph(_) -> [].

remove_vertexes([], Vertexes) -> Vertexes;
remove_vertexes(RemoveVertexes, Vertexes) ->
  [RemoveVertex | Retain] = RemoveVertexes,
  NewVertexes = lists:delete(RemoveVertex, Vertexes),
  remove_vertexes(Retain, NewVertexes).

show_result(Text, CaseNo ,Expect, Result) ->
  case equal_edges(Expect, Result) of
    true ->
      ?OUTPUT_INFO(Text, [CaseNo, true]);
    false ->
      ?OUTPUT_ERROR(Text, [CaseNo, false]),
      ?OUTPUT_ERROR("Expect:"),
      show_edges(false, Expect),
      ?OUTPUT_ERROR("Result"),
      show_edges(false, Result)
  end.

show_edges(Edges) -> show_edges(true, Edges).
show_edges(_, []) -> ok;
show_edges(Debug, Edges) -> 
  [Edge | Retain] = Edges,
  show_edge(Debug, Edge),
  show_edges(Debug, Retain).
show_edge(Debug, Edge) ->
  #edge{
    from=FromVertex,
    to=ToVertex,
    flow=Flow,
    capacity=Capacity,
    cost=Cost} = Edge,
  Text = "~w to ~w: ~w/~w, cost is ~w(par unit ~w)",
  Param = [
     FromVertex,
     ToVertex,
     Flow,
     Capacity,
     Cost * Flow,
     Cost
    ],
  case Debug of
    true ->
      ?OUTPUT_DEBUG(Text, Param);
    false -> 
      ?OUTPUT_INFO(Text, Param)
  end.