-module(minimum_cost).

-export([test/0]).

% from -> atom()
% to -> atom()
% flow -> integer()
% capacity -> integer()
% cost -> integer()
-record(edge_record, {from, to, flow, capacity, cost}).

% direction -> atom: forward | backward
-record(argumenting_path_record, {vertex, previous, direction}).

-record(priority_queue_record, {vertex, dist}).

-record(inqueue_record, {vertex, arrival}).


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


-define(MAX_DIST, 99999999).
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
  show_result(ResultGraph).

-spec compute(list(edge_record)) -> list(edge_record).
compute(Graph) -> 
  ArguPath = generate_argumenting_path(Graph),
  ?OUTPUT_DEBUG("compute/1 - arg paths: ~w", [ArguPath]),
  compute(Graph, ArguPath).


-spec compute(list(edge_record), list(argumenting_path_record)) -> list(edge_record).
compute(Graph, []) ->  
  ?OUTPUT_DEBUG("compute/2 - complete"),
  Graph;
compute(Graph, ArgPaths) ->  
  NewGraph = process_path(Graph, ArgPaths),
  compute(NewGraph).

-spec process_path(list(edge_record), list(argumenting_path_record)) -> list(edge_record).
process_path(Edges, ArgPaths) ->
  Vertex = sink,
  Delta = calculate_delta(Edges, ArgPaths, Vertex, ?MAX_DELTA),
  ?OUTPUT_DEBUG(
    "process_path/2 - delta: ~w",
    [Delta]),
  reflect_delta(Edges, ArgPaths, Delta).

-spec calculate_delta(
    list(edge_record), 
    list(argumenting_path_record),
    atom(),
    integer()
  ) -> integer().
calculate_delta(_, _, source, Delta) -> Delta;
calculate_delta(Edges, ArgPaths, Vertex, Delta) ->
  ?OUTPUT_DEBUG(
    "calculate_delta/4 - vertex: ~w",
    [Vertex]),
  ArgPath = find_argumenting_path(Vertex, ArgPaths),
  {TryDelta, NextVertex}  = case ArgPath#argumenting_path_record.direction of
    forward ->
      ?OUTPUT_DEBUG(
        "calculate_delta/4 - forward(vertex: ~w, previous: ~w)",
        [
          ArgPath#argumenting_path_record.vertex,
          ArgPath#argumenting_path_record.previous
        ]),
      Edge = find_edge(
        ArgPath#argumenting_path_record.previous,
        ArgPath#argumenting_path_record.vertex,
        Edges),
      {
        Edge#edge_record.capacity - Edge#edge_record.flow, 
        Edge#edge_record.from
      };
    backward ->
      ?OUTPUT_DEBUG(
        "calculate_delta/4 - backward(vertex: ~w, previous: ~w)",
        [
          ArgPath#argumenting_path_record.vertex,
          ArgPath#argumenting_path_record.previous
        ]),
      Edge = find_edge(
        ArgPath#argumenting_path_record.vertex,
        ArgPath#argumenting_path_record.previous,
        Edges),
      {
        Edge#edge_record.flow,
        Edge#edge_record.to
      }
  end,
  NewDelta = erlang:min(Delta, TryDelta),
  ?OUTPUT_DEBUG(
    "calculate_delta/4 - delta ~w , try delta: ~w",
    [Delta, TryDelta]),
  calculate_delta(Edges, ArgPaths, NextVertex, NewDelta).

-spec reflect_delta(
    list(edge_record), 
    list(argumenting_path_record), 
    integer()
  ) -> list(edge_record).
reflect_delta(Edges, ArgPaths, Delta) ->
  Vertex = sink,
  reflect_delta(Edges, ArgPaths, Delta, Vertex).

-spec reflect_delta(
    list(edge_record), 
    list(argumenting_path_record), 
    integer(),
    atom() 
  ) -> list(edge_record).
reflect_delta(Edges, _, _, source) -> Edges;
reflect_delta(Edges, ArgPaths, Delta, Vertex) -> 
  ?OUTPUT_DEBUG(
     "reflect_delta/4 - edge length: ~w",
     [length(Edges)]),
  ?OUTPUT_DEBUG(
     "reflect_delta/4 - vertex: ~w in ~w",
     [Vertex, ArgPaths]),
  ArgPath = find_argumenting_path(Vertex, ArgPaths),

  ?OUTPUT_DEBUG(
     "reflect_delta/4 - founded arg path: ~w",
     [ArgPath]),

  {Edges2, NextVertex} = case ArgPath#argumenting_path_record.direction of
    forward ->
      NewEdge = add_flow(
        Edges, 
        ArgPath#argumenting_path_record.previous, 
        Vertex, 
        Delta),
      {NewEdge, ArgPath#argumenting_path_record.previous};
    backward ->
      NewEdge = sub_flow(
        Edges, 
        Vertex,
        ArgPath#argumenting_path_record.previous,
        Delta),
      {NewEdge, ArgPath#argumenting_path_record.previous}
  end,
  reflect_delta(Edges2, ArgPaths, Delta, NextVertex).

-spec generate_argumenting_path(
    list(edge_record)
  ) -> list(argumenting_path_record).
generate_argumenting_path(Edges) -> 
  PriorityQueue = [
    #priority_queue_record{vertex=source, dist=0}],
  Dists = generate_initial_dist(Edges),
  SetSourceDists = maps:put(source, 0, Dists), 
  Inqueue = generate_initial_inqueue(Edges),
  SetSourceInqueue = add_inqueue(source, true, Inqueue),
  ArgPaths = [],
  generate_argumenting_path(
    Edges, 
    PriorityQueue,
    SetSourceDists, 
    SetSourceInqueue,
    ArgPaths).

% 3: map() -> [{atom(), integer()}]
-spec generate_argumenting_path(
    list(edge_record),
    list(priority_queue_record),
    map(),
    list(inqueue_record),
    list(argumenting_record)
  ) -> list(argumenting_record).
generate_argumenting_path(_, [], Dists, _, ArgPaths) ->
  Dist = maps:get(sink, Dists),
  case Dist == ?MAX_DIST of
    true -> [];
    false -> ArgPaths
  end;
generate_argumenting_path(Edges, PriorityQueue, Dists, Inqueue, ArgPaths) ->
  {Smallest, PriorityQueue2} = get_smallest_id_priority_queue(PriorityQueue),
  Inqueue2 = set_inqueue(
    Smallest#priority_queue_record.vertex,
    false,
    Inqueue),
  generate_argumenting_path(
    Edges, 
    PriorityQueue2, 
    Dists, 
    Inqueue2, 
    ArgPaths,
    Smallest).

generate_argumenting_path(
  Edges, 
  _, 
  Dists, 
  Inqueue, 
  ArgPaths, 
  #priority_queue_record{vertex=sink}) ->
  generate_argumenting_path(
    Edges, 
    [], 
    Dists, 
    Inqueue, 
    ArgPaths);
generate_argumenting_path(
  Edges, 
  PriorityQueue, 
  Dists, 
  Inqueue, 
  ArgPaths, 
  PriorityVertex) ->
  Vertexes = generate_vertex_from_edges(Edges),
  RemovedTargetVertexes = remove_vertexes(
    [source, PriorityVertex#priority_queue_record.vertex],
    Vertexes),
  generate_argumenting_path(
    Edges, 
    PriorityQueue, 
    Dists, 
    Inqueue, 
    ArgPaths, 
    PriorityVertex,
    RemovedTargetVertexes).


generate_argumenting_path(
    Edges, PriorityQueue, Dists, 
    Inqueue, ArgPaths, _, []) ->
  generate_argumenting_path( Edges, PriorityQueue, Dists, Inqueue, ArgPaths);
generate_argumenting_path(
    Edges, PriorityQueue, Dists, Inqueue, 
    ArgPaths, PriorityQueueRec, Vertexes) ->
  [NextVertex | VertexesRetain] = Vertexes,

  % forward
  {PriorityQueue2, 
    Dists2, 
    Inqueue2, 
    ArgPaths2} = 
    generate_argumenting_path_forward(
      Edges, 
      PriorityQueue, 
      Dists, 
      Inqueue, 
      ArgPaths, 
      PriorityQueueRec#priority_queue_record.vertex,
      NextVertex),

  % backward
  {PriorityQueue3, 
  Dists3, 
  Inqueue3, 
  ArgPaths3} = 
    generate_argumenting_path_backward(
      Edges, 
      PriorityQueue2, 
      Dists2, 
      Inqueue2, 
      ArgPaths2, 
      PriorityQueueRec#priority_queue_record.vertex,
      NextVertex),
  generate_argumenting_path(
    Edges, 
    PriorityQueue3, 
    Dists3, 
    Inqueue3, 
    ArgPaths3,
    PriorityQueueRec,
    VertexesRetain).

-spec generate_argumenting_path_forward(
    list(edge_record),
    list(priority_queue_record),
    map(),
    list(inqueue_record),
    list(argumenting_path_record),
    atom(),
    atom()
  ) ->  {
        list(priority_queue_record),
        map(),
        list(inqueue_record),
        list(argumenting_path_record)}.
generate_argumenting_path_forward(
      Edges, PriorityQueue, Dists, 
      Inqueue, ArgPaths, CurrentVertex, NextVertex) ->
  ?OUTPUT_DEBUG("generate_argumenting_path_forward/7 - ~w", [start]),
  Edge = find_edge(CurrentVertex, NextVertex, Edges),
  {ExistsEdge, NewDist, ExistsMargin} = case Edge of
      null -> 
        {false, -1, false};
      _ -> 
        Dist = maps:get(CurrentVertex, Dists) + Edge#edge_record.cost,
        Margin = (Edge#edge_record.flow < Edge#edge_record.capacity),
        {true, Dist, Margin}
    end,
  NextDist = maps:get(NextVertex, Dists),
  UpdateDist = (NewDist >= 0) and (NewDist < NextDist),
  Arrival = get_inqueue(NextVertex, Inqueue),
  ?OUTPUT_DEBUG(
    "generate_argumenting_path_forward/7 - exists edge: ~w, update dist: ~w, arrival: ~w",
    [ExistsEdge, UpdateDist, Arrival]),
  case {ExistsEdge, ExistsMargin, UpdateDist, Arrival} of
    {true, true, true, true} ->
      % new argumenting path
      NewArgPaths = add_argumenting_path(
        NextVertex, 
        CurrentVertex,
        forward,
        ArgPaths),
      % new dists
      NewDists = maps:put(
        NextVertex,
        NewDist,
        Dists),
      % new priority queue
      NewPriorityQueue = decrease_priority_queue(NextVertex, NewDist, PriorityQueue),
      {
        NewPriorityQueue,
        NewDists,
        Inqueue,
        NewArgPaths
      };
    {true, true, true, false} ->
      % new argumenting path
      NewArgPaths = add_argumenting_path(
        NextVertex, 
        CurrentVertex,
        forward,
        ArgPaths),
      % new dists
      NewDists = maps:put(
        NextVertex,
        NewDist,
        Dists),
      % new priority queue
      NewPriorityQueue = insert_priority_queue(NextVertex, NewDist, PriorityQueue),
      % new inqueue
      NewInqueue = set_inqueue(NextVertex, true, Inqueue),
      {
        NewPriorityQueue,
        NewDists,
        NewInqueue,
        NewArgPaths
      };
    _ ->
      % no change
      {
        PriorityQueue, 
        Dists, 
        Inqueue, 
        ArgPaths
      }
  end.

  
-spec generate_argumenting_path_backward(
    list(edge_record),
    list(priority_queue_record),
    map(),
    list(inqueue_record),
    list(argumenting_path_record),
    atom(),
    atom()
  ) ->  {
        list(priority_queue_record),
        map(),
        list(inqueue_record),
        list(argumenting_path_record)}.
generate_argumenting_path_backward(
      Edges, PriorityQueue, Dists, 
      Inqueue, ArgPaths, CurrentVertex, NextVertex) ->
  ?OUTPUT_DEBUG("generate_argumenting_path_backward/7 - ~w", [start]),
  Edge = find_edge(NextVertex, CurrentVertex, Edges),
  {ExistsEdge, NewDist, ExistsFlow} = case Edge of
      null -> {false, -1, false};
      _ ->
        Dist = maps:get(CurrentVertex, Dists) - Edge#edge_record.cost,
        Retain = (Edge#edge_record.flow > 0),
        {true, Dist, Retain}
    end,
  UpdateDist = (NewDist >= 0) and (NewDist < maps:get(NextVertex, Dists)),
  Arrival = get_inqueue(NextVertex, Inqueue),
  case {ExistsEdge, ExistsFlow, UpdateDist, Arrival} of
    {true, true, true, true} ->
      % new argmenting path
      NewArgPaths = add_argumenting_path(NextVertex, CurrentVertex, backward, ArgPaths),
      % new dists
      NewDists = maps:put(NextVertex, NewDist, Dists),
      % new primary queue
      NewPriorityQueue = decrease_priority_queue(NextVertex, NewDist, PriorityQueue),
      {
        NewPriorityQueue,
        NewDists,
        Inqueue,
        NewArgPaths
      };
    {true, true, true, false} ->
      % new argmenting path
      NewArgPaths = add_argumenting_path(NextVertex, CurrentVertex, backward, ArgPaths),
      % new dists
      NewDists = maps:put(NextVertex, NewDist, Dists),
      % new primary queue
      NewPriorityQueue = insert_priority_queue(NextVertex, NewDist, PriorityQueue),
      % new inqueue
      NewInqueue = set_inqueue(NextVertex, true, Inqueue),
      {
        NewPriorityQueue,
        NewDists,
        NewInqueue,
        NewArgPaths
      };
    _ ->
      {
        PriorityQueue,
        Dists,
        Inqueue,
        ArgPaths
      }
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% graph functions
-spec find_edge(
    atom(), 
    atom(), 
    list(edge_record)
  ) -> {edge_record | null}.
find_edge(_, _, []) -> null;
find_edge(FromVertex, ToVertex, Edges) -> 
  ?OUTPUT_DEBUG("find_edge/3 - from: ~w, to: ~w", [FromVertex, ToVertex]),
  [Head | Retain] = Edges,
  case {Head#edge_record.from, Head#edge_record.to} of
    {FromVertex, ToVertex} -> Head;
    _ ->
     find_edge(FromVertex, ToVertex, Retain)
  end.

-spec remove_edge(
    atom(), 
    atom(), 
    list(edge_record)
  ) -> list(edge_record).
remove_edge(_, _, []) -> [];
remove_edge(FromVertex, ToVertex, Edges) -> 
  [Head | Retain] = Edges,
  case {Head#edge_record.from, Head#edge_record.to} of
    {FromVertex, ToVertex} -> Retain;
    _ ->
     [Head] ++ remove_edge(FromVertex, ToVertex, Retain)
  end.

-spec add_flow(
    list(edge_record),
    atom(), 
    atom(),
    integer()
  ) -> list(edge_record).
add_flow(Edges, FromVertex, ToVertex, Delta) ->
  ?OUTPUT_DEBUG(
    "add_flow/4 - from: ~w, to: ~w, delta: ~w",
    [FromVertex, ToVertex, Delta]),
  Edge = find_edge(FromVertex, ToVertex, Edges), 
  #edge_record{flow=Flow} = Edge, 
  [
   Edge#edge_record{flow=Flow + Delta}
  ] ++ remove_edge(FromVertex, ToVertex, Edges).

-spec sub_flow(
    list(edge_record),
    atom(), 
    atom(),
    integer()
  ) -> list(edge_record).
sub_flow(Edges, FromVertex, ToVertex, Delta) ->
  ?OUTPUT_DEBUG(
    "sub_flow/4 - from: ~w, to: ~w, delta: ~w",
    [FromVertex, ToVertex, Delta]),
  Edge = find_edge(FromVertex, ToVertex, Edges), 
  #edge_record{flow=Flow} = Edge, 
  [
   Edge#edge_record{flow=Flow - Delta}
  ] ++ remove_edge(FromVertex, ToVertex, Edges).

generate_vertex_from_edges(Edges) ->
  generate_vertex_from_edges(Edges, []).
generate_vertex_from_edges([], ResultVertexes) -> ResultVertexes;
generate_vertex_from_edges(Edges, ResultVertexes) ->
  [Edge | Retain] = Edges,
  Result2 = case lists:any(fun(Vertex) -> Vertex == Edge#edge_record.from end, ResultVertexes) of
    true -> ResultVertexes;
    false -> [Edge#edge_record.from] ++ ResultVertexes
  end,
  Result3 = case lists:any(fun(Vertex) -> Vertex == Edge#edge_record.to end, Result2) of
    true -> Result2;
    false -> [Edge#edge_record.to] ++ Result2
  end,
  generate_vertex_from_edges(Retain, Result3).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% argumenting path functions
-spec find_argumenting_path(
  atom(), 
  list(argumenting_path_record)) -> {argumenting_path_record | null}.
find_argumenting_path(_, []) -> 
  null;
find_argumenting_path(Vertex, ArgPaths) ->
  [ArgPath | Retain] = ArgPaths,
  find_argumenting_path(
    Vertex, 
    ArgPath#argumenting_path_record.vertex, 
    ArgPath, 
    Retain).

-spec find_argumenting_path(
  atom(), 
  atom(), 
  argumenting_path_record,
  list(argumenting_path_record)) -> {argumenting_path_record | null}.
find_argumenting_path(Vertex, Vertex, ArgPath, _) -> ArgPath;
find_argumenting_path(Vertex, _, _, ArgPaths) -> 
  find_argumenting_path(Vertex, ArgPaths).

-spec add_argumenting_path(
  atom(),
  atom(),
  forward | backward,
  list(argumenting_path_record)
  ) -> list(argumenting_path_record).
add_argumenting_path(Vertex, PreviousVertex, Direction, ArgPaths) ->
  ArgPaths ++ [
    #argumenting_path_record{
      vertex=Vertex, 
      previous=PreviousVertex,
      direction=Direction}
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dists function
% return map() -> {atom(), integer()}
-spec generate_initial_dist(list(edge_record)) -> map().
generate_initial_dist(Edges) -> 
  Map = maps:new(),
  generate_initial_dist(Edges, Map).

-spec generate_initial_dist(
    list(edge_record),
    map()
  ) -> map().
generate_initial_dist([], Map) ->  Map;
generate_initial_dist(Edges, Map) -> 
  [Edge | Retain] = Edges,
  #edge_record{from=FromVertex, to=ToVertex} = Edge,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inqueue function
-spec generate_initial_inqueue(list(edge_record)) -> list(inqueue_record).
generate_initial_inqueue(Edges) ->
  List = [],
  generate_initial_inqueue(Edges, List).

-spec generate_initial_inqueue(
    list(edge_record),
    list(inqueue_record)
  ) -> list(inqueue_record).
generate_initial_inqueue([], List) ->  List;
generate_initial_inqueue(Edges, List) -> 
  [Edge | Retain] = Edges,
  #edge_record{from=FromVertex, to=ToVertex} = Edge,
  List2 = case exists_inqueue(FromVertex, List) of
    true -> List;
    _ -> 
      add_inqueue(FromVertex, false, List)
  end,
  List3 = case exists_inqueue(ToVertex, List2) of
    true -> List2;
    _ -> 
      add_inqueue(ToVertex, false, List2)
  end,
  generate_initial_inqueue(Retain, List3).

-spec exists_inqueue(atom(), list(inqueue_record)) -> true | false.
exists_inqueue(Vertex, Inqueue) -> 
  case lists:keyfind(
    Vertex,
    #inqueue_record.vertex,
    Inqueue) of
    false -> false;
    _ -> true
  end.

add_inqueue(Vertex, Arrival, List) -> 
  [
    #inqueue_record{
      vertex=Vertex,
      arrival=Arrival
    }
  ] ++ List.

set_inqueue(Vertex, Arrival, List) ->
  Record = lists:keyfind(
    Vertex,
    #inqueue_record.vertex,
    List),
  UpdatedRecord = Record#inqueue_record{arrival=Arrival},
  lists:keydelete(
    Vertex,
    #inqueue_record.vertex,
    List) ++ [UpdatedRecord].

get_inqueue(Vertex, List) ->
  Rec = lists:keyfind(
    Vertex,
    #inqueue_record.vertex,
    List),
  Rec#inqueue_record.arrival.
  

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
  [
    #priority_queue_record{
       vertex=Vertex,
       dist=Dist
    }
  ] ++ PriorityQueue.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utility functions
generate_initial_graph(1) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=10, cost=1},
   #edge_record{from=v1, to=sink, flow=0, capacity=10, cost=1}
  ];

generate_initial_graph(2) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=5, cost=1},
   #edge_record{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge_record{from=v1, to=sink, flow=0, capacity=10, cost=1},
   #edge_record{from=v2, to=v1, flow=0, capacity=5, cost=1}
  ];

generate_initial_graph(3) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=5, cost=1},
   #edge_record{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge_record{from=v1, to=sink, flow=0, capacity=10, cost=1},
   #edge_record{from=v2, to=v1, flow=0, capacity=3, cost=1}
  ];

generate_initial_graph(4) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=10, cost=5},
   #edge_record{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge_record{from=source, to=v3, flow=0, capacity=4, cost=1},
   #edge_record{from=v1, to=v4, flow=0, capacity=5, cost=5},
   #edge_record{from=v1, to=v2, flow=0, capacity=3, cost=5},
   #edge_record{from=v2, to=v4, flow=0, capacity=2, cost=1},
   #edge_record{from=v2, to=v3, flow=0, capacity=5, cost=1},
   #edge_record{from=v3, to=v5, flow=0, capacity=8, cost=1},
   #edge_record{from=v4, to=sink, flow=0, capacity=7, cost=1},
   #edge_record{from=v5, to=sink, flow=0, capacity=11, cost=1}
  ];

generate_initial_graph(5) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=5, cost=5},
   #edge_record{from=source, to=v2, flow=0, capacity=5, cost=3},
   #edge_record{from=source, to=v3, flow=0, capacity=5, cost=1},
   #edge_record{from=v1, to=v4, flow=0, capacity=5, cost=5},
   #edge_record{from=v2, to=v4, flow=0, capacity=5, cost=1},
   #edge_record{from=v3, to=v4, flow=0, capacity=5, cost=1},
   #edge_record{from=v4, to=sink, flow=0, capacity=10, cost=1}
  ];


generate_initial_graph(_) ->
  [].


remove_vertexes([], Vertexes) -> Vertexes;
remove_vertexes(RemoveVertexes, Vertexes) ->
  [RemoveVertex | Retain] = RemoveVertexes,
  NewVertexes = lists:delete(RemoveVertex, Vertexes),
  remove_vertexes(Retain, NewVertexes).

show_result([]) -> ok;
show_result(Edges) ->
  [Edge | Retain] = Edges,
  #edge_record{
    from=FromVertex,
    to=ToVertex,
    flow=Flow,
    capacity=Capacity,
    cost=Cost} = Edge,
  ?OUTPUT_INFO(
    "~w to ~w: ~w/~w, cost is ~w(par unit ~w)", 
    [
     FromVertex,
     ToVertex,
     Flow,
     Capacity,
     Cost * Flow,
     Cost
    ]),
  show_result(Retain).
