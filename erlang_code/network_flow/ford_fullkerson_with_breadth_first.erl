-module(ford_fullkerson_with_breadth_first).

-export([test/0]).

-import(ford_fullkerson, 
  [process_argpath/2,
    find_edge_forward/2,
    find_edge_backward/2,
    find_argumenting_path/2,
    add_argumenting_path/4,
    exists_argumenting_path/2]).

-include("network_flow.hrl").

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
  case (?LOG_LEVEL) =< 1 of
    true -> io:fwrite("[DEBUG] ford_fullkerson_with_breadth_first: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] ford_fullkerson_with_breadth_first: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] ford_fullkerson_with_breadth_first: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] ford_fullkerson_with_breadth_first: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] ford_fullkerson_with_breadth_first: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] ford_fullkerson_with_breadth_first: " ++ S ++ "~n", Args);
    _ -> ok
  end).


-define(MAX_DELTA, 9999999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  test(1),
  test(2),
  test(3),
  test(4),
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

-spec compute(list(edge)) -> list(edge).
compute(Graph) -> 
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

-spec generate_argumenting_path(list(edge)) -> list(argpath).
generate_argumenting_path(Graph) ->
  Queue = [source],
  ArgPaths = [
    #argpath{
      vertex=source,
      previous=null,
      direction=none}],
  generate_argumenting_path(Graph, Queue, ArgPaths).

-spec generate_argumenting_path(
    list(edge), 
    list(atom()), 
    list(argpath)
  ) -> list(argpath).
generate_argumenting_path(_, [], _) -> 
  [];
generate_argumenting_path(Edges, PathQueue, ArgPaths) -> 
  {Vertex, PathQueueRetain} = remove_first_vertex_queue(PathQueue),
  {ExistsArgumentingPath, PathQueue2, ArgPaths2} = 
    generate_argumenting_path_forward(Edges, PathQueueRetain, ArgPaths, Vertex),
  case ExistsArgumentingPath of
    true -> 
      ArgPaths2;
    false -> 
      {PathQueue3, ArgPaths3} = 
        generate_argumenting_path_backward(
          Edges, 
          PathQueue2, 
          ArgPaths2, 
          Vertex),
      generate_argumenting_path(Edges, PathQueue3, ArgPaths3)
  end.

-spec generate_argumenting_path_forward(
    list(edge),
    list(atom()), 
    list(argpath),
    atom()) ->  
  {
    true | false,
    list(atom()), 
    list(argpath)
  }.
generate_argumenting_path_forward(Edges, PathQueue, ArgPaths, Vertex) ->
  ForwardEdges = find_edge_forward(Vertex, Edges),
  generate_argumenting_path_forward(Edges, PathQueue, ArgPaths, Vertex, ForwardEdges).

-spec generate_argumenting_path_forward(
    list(edge),
    list(atom()), 
    list(argpath),
    atom(),
    list(edge)) -> 
  {
    true | false,
    list(atom()), 
    list(argpath)
  }.
generate_argumenting_path_forward(_, PathQueue, ArgPaths, _, []) -> 
  {false, PathQueue, ArgPaths};
generate_argumenting_path_forward(Edges, PathQueue, ArgPaths, Vertex, ForwardEdges) -> 
  [ForwardEdge | ForwardEdgesRetain] = ForwardEdges,
  Exists = exists_argumenting_path(
    ForwardEdge#edge.to, 
    ArgPaths),
  FullTank = (ForwardEdge#edge.capacity > ForwardEdge#edge.flow),
  ArrivalSink = (ForwardEdge#edge.to  ==  sink),
  ?OUTPUT_DEBUG(
     "generate_argumenting_path_forward/5 - ~w in arg path exists: ~w, full tank: ~w, arrival sink: ~w",
    [ForwardEdge#edge.to, Exists, FullTank , ArrivalSink ]),
  {Return, PathQueue2, ArgPaths2} = case {Exists, FullTank, ArrivalSink} of
    % not arrival,not limit and not arrival sink
    {false, true, false} -> 
      ?OUTPUT_DEBUG("generate_argumenting_path_forward - ~w", [not_arrival_sink]),
      NewArgPaths = add_argumenting_path(
        ForwardEdge#edge.to, 
        Vertex, 
        forward, 
        ArgPaths),
      NewPathQueue = insert_vertex_queue(
        ForwardEdge#edge.to, 
        PathQueue),
      {false, NewPathQueue, NewArgPaths};
    % not arrival,not limit and arrival sink
    {false, true, true} ->
      ?OUTPUT_DEBUG("generate_argumenting_path_forward - ~w", [arrival_sink]),
      NewArgPaths = add_argumenting_path(
        ForwardEdge#edge.to, 
        Vertex, 
        forward, 
        ArgPaths),
      {true, PathQueue, NewArgPaths};
    _ -> 
      ?OUTPUT_DEBUG("generate_argumenting_path_forward - ~w", [any]),
      {false, PathQueue, ArgPaths}
  end,
  case Return of
    true ->
      {true, PathQueue2, ArgPaths2};
    false ->
      generate_argumenting_path_forward(
        Edges, 
        PathQueue2, 
        ArgPaths2, 
        Vertex, 
        ForwardEdgesRetain)
  end.

-spec generate_argumenting_path_backward(
    list(edge),
    list(atom()),
    list(argpath),
    atom() 
  ) -> {list(atom()), list(argpath)}.
generate_argumenting_path_backward(Edges, PathQueue, ArgPaths, Vertex) ->
  ?OUTPUT_DEBUG(
    "generate_argumenting_path_backward/4 - vertex: ~w",
    [Vertex]),
  BackwardEdges = find_edge_backward(Vertex, Edges),
  ?OUTPUT_DEBUG(
    "generate_argumenting_path_backward/4 - edges: ~w",
    [BackwardEdges]),
  generate_argumenting_path_backward(Edges, PathQueue, ArgPaths, Vertex, BackwardEdges).

-spec generate_argumenting_path_backward(
    list(edge),
    list(atom()),
    list(argpath),
    atom(),
    list(edge)
  ) -> {list(atom()), list(argpath)}.
generate_argumenting_path_backward(_, PathQueue, ArgPaths, _, []) ->
  {PathQueue, ArgPaths};
generate_argumenting_path_backward(Edges, PathQueue, ArgPaths, Vertex, BackwardEdges) ->
  [BackwardEdge | BackwardEdgesRetain] = BackwardEdges,
  ExistsArgPath = exists_argumenting_path(
    BackwardEdge#edge.from,
    ArgPaths),
  ExistsFlow = BackwardEdge#edge.flow > 0,
  ?OUTPUT_DEBUG(
    "generate_argumenting_path_backward/5 - exists arg path: ~w, exists flow: ~w",
    [ExistsArgPath, ExistsFlow]),
  {PathQueue2, ArgPaths2} = case {ExistsArgPath, ExistsFlow} of
    % not arrival and exists flow
    {false, true} ->
      NewArgPaths = add_argumenting_path(
        BackwardEdge#edge.from,
        Vertex,
        backward,
        ArgPaths),
      NewPathQueue = insert_vertex_queue(
        Vertex, 
        PathQueue),
      {NewPathQueue, NewArgPaths};
    _ ->
      {PathQueue, ArgPaths}
  end,
  generate_argumenting_path_backward(
    Edges, 
    PathQueue2, 
    ArgPaths2, 
    Vertex, 
    BackwardEdgesRetain).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vertex Queue functions
-spec insert_vertex_queue(atom(), list(atom())) -> 
  list(atom()).
insert_vertex_queue(Vertex, Queue) ->
  [Vertex] ++ Queue.

-spec remove_first_vertex_queue(list(atom())) -> 
  {atom(), list(atom())}.
remove_first_vertex_queue(Queue) ->
  [Head | Retain] = Queue,
  {Head, Retain}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utility functions
generate_initial_graph(1) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10},
   #edge{from=v1, to=sink, flow=0, capacity=10}
  ];

generate_initial_graph(2) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=5},
   #edge{from=source, to=v2, flow=0, capacity=5},
   #edge{from=v1, to=sink, flow=0, capacity=10},
   #edge{from=v2, to=v1, flow=0, capacity=5}
  ];

generate_initial_graph(3) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=5},
   #edge{from=source, to=v2, flow=0, capacity=5},
   #edge{from=v1, to=sink, flow=0, capacity=10},
   #edge{from=v2, to=v1, flow=0, capacity=3}
  ];

generate_initial_graph(4) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10},
   #edge{from=source, to=v2, flow=0, capacity=5},
   #edge{from=source, to=v3, flow=0, capacity=4},
   #edge{from=v1, to=v4, flow=0, capacity=5},
   #edge{from=v1, to=v2, flow=0, capacity=3},
   #edge{from=v2, to=v4, flow=0, capacity=2},
   #edge{from=v2, to=v3, flow=0, capacity=5},
   #edge{from=v3, to=v5, flow=0, capacity=8},
   #edge{from=v4, to=sink, flow=0, capacity=7},
   #edge{from=v5, to=sink, flow=0, capacity=11}
  ];

generate_initial_graph(_) ->
  [].

show_result([]) -> ok;
show_result(Edges) ->
  [Edge | Retain] = Edges,
  #edge{
    from=FromVertex,
    to=ToVertex,
    flow=Flow,
    capacity=Capacity} = Edge,
  ?OUTPUT_INFO(
    "~w to ~w: ~w/~w", 
    [
     FromVertex,
     ToVertex,
     Flow,
     Capacity
    ]),
  show_result(Retain).

