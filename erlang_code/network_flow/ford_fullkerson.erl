-module(ford_fullkerson).

-export([
  test/0,
  process_argpath/2,
  equal_edges/2,
  equal_edge/2,
  find_edge/3,
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
    true -> io:fwrite("[DEBUG] ford_fullkerson: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] ford_fullkerson: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] ford_fullkerson: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] ford_fullkerson: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] ford_fullkerson: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] ford_fullkerson: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(MAX_DELTA, 9999999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  find_edge_forward_test(),
  find_edge_backward_test(),
  process_argpath_test(),
  test(1),
  test(2),
  test(3),
  test(4),
  ok.

-spec process_argpath(list(edge), list(argpath)) -> list(edge).
process_argpath(Edges, ArgPaths) ->
  Vertex = sink,
  Delta = calculate_delta(Edges, ArgPaths, Vertex, ?MAX_DELTA),
  ?OUTPUT_DEBUG(
    "process_argpath/2 - delta: ~w",
    [Delta]),
  reflect_delta(Edges, ArgPaths, Delta, sink).

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

-spec calculate_delta(
    list(edge), 
    list(argpath),
    atom(),
    integer()
  ) -> integer().
calculate_delta(_, _, source, Delta) -> Delta;
calculate_delta(Edges, ArgPaths, Vertex, Delta) ->
  ArgPath = find_argumenting_path(Vertex, ArgPaths),
  Edge = case ArgPath#argpath.direction of
    forward ->
      find_edge(
        ArgPath#argpath.previous,
        ArgPath#argpath.vertex,
        Edges);
    backward ->
      find_edge(
        ArgPath#argpath.vertex,
        ArgPath#argpath.previous,
        Edges)
  end,
  TryDelta = case {Edge, ArgPath#argpath.direction} of
    {null, _} ->
      ?OUTPUT_ERROR(
        "calculate_delta/4 - edge not found: ~w",
        [ArgPath]),
      Delta;
    {Edge, forward} ->
      Edge#edge.capacity - Edge#edge.flow;
    {Edge, backward} ->
        Edge#edge.flow
  end,
  NewDelta = erlang:min(Delta, TryDelta),
  calculate_delta(Edges, ArgPaths, ArgPath#argpath.previous , NewDelta).

-spec reflect_delta(
    list(edge), 
    list(argpath), 
    integer(),
    atom() 
  ) -> list(edge).
reflect_delta(Edges, _, _, source) -> Edges;
reflect_delta(Edges, ArgPaths, Delta, Vertex) -> 
  ArgPath = find_argumenting_path(Vertex, ArgPaths),
  {Edges2, PrevVertex} = case ArgPath#argpath.direction of
    forward ->
      NewEdge = add_flow(
        Edges, 
        ArgPath#argpath.previous, 
        Vertex, 
        Delta),
      {NewEdge, ArgPath#argpath.previous};
    backward ->
      NewEdge = sub_flow(
        Edges, 
        Vertex,
        ArgPath#argpath.previous,
        Delta),
      {NewEdge, ArgPath#argpath.previous}
  end,
  reflect_delta(Edges2, ArgPaths, Delta, PrevVertex).

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
  {Vertex, PathQueueRetain} = pop_vertex_queue(PathQueue),
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
      NewPathQueue = push_vertex_queue(
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
      NewPathQueue = push_vertex_queue(
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
% graph functions

% return forward edge for specify vertex
% ex) soruce -> v1, v2 -> v3 -> sink
% v3 forward edge
% 0: v3 -> sink 
-spec find_edge_forward(atom(), list(edge)) -> list(edge).
find_edge_forward(_, []) ->  [];
find_edge_forward(Vertex, Graph) -> 
  ?OUTPUT_DEBUG(
     "find_edge_forward/2 - vertex: ~w",
     [Vertex]),
  [Edge | GraphRetain] = Graph,
  case Edge#edge.from of
    Vertex ->
      [Edge];
    _ ->
      []
  end ++ find_edge_forward(Vertex, GraphRetain).


% return backward edge for specify vertex
% ex) soruce -> v1, v2 -> v3 -> sink
% v3 backward edge
% 0: v1 -> v3
% 1: v2 -> v3
-spec find_edge_backward(atom(), list(edge)) -> list(edge).
find_edge_backward(_, []) ->  [];
find_edge_backward(Vertex, Graph) -> 
  [Edge | GraphRetain] = Graph,
  case Edge#edge.to of
    Vertex ->
      [Edge];
    _ ->
      []
  end ++ find_edge_backward(Vertex, GraphRetain).

-spec find_edge(
    atom(), 
    atom(), 
    list(edge)
  ) -> {edge | null}.
find_edge(_, _, []) -> null;
find_edge(FromVertex, ToVertex, Edges) -> 
  [Head | Retain] = Edges,
  case {Head#edge.from, Head#edge.to} of
    {FromVertex, ToVertex} -> Head;
    _ ->
     find_edge(FromVertex, ToVertex, Retain)
  end.

-spec remove_edge(
    atom(), 
    atom(), 
    list(edge)
  ) -> list(edge).
remove_edge(_, _, []) -> [];
remove_edge(FromVertex, ToVertex, Edges) -> 
  [Head | Retain] = Edges,
  case {Head#edge.from, Head#edge.to} of
    {FromVertex, ToVertex} -> Retain;
    _ ->
     [Head] ++ remove_edge(FromVertex, ToVertex, Retain)
  end.

-spec add_flow(
    list(edge),
    atom(), 
    atom(),
    integer()
  ) -> list(edge).
add_flow(Edges, FromVertex, ToVertex, Delta) ->
  case find_edge(FromVertex, ToVertex, Edges) of
    null ->
      ?OUTPUT_ERROR(
        "add_flow/4 - edge not found: e(~w, ~w)",
        [FromVertex, ToVertex]),
      Edges;
    Edge ->
      #edge{flow=Flow} = Edge, 
      [
        Edge#edge{flow=Flow + Delta}
      ] ++ remove_edge(FromVertex, ToVertex, Edges)
  end.


-spec sub_flow(
    list(edge),
    atom(), 
    atom(),
    integer()
  ) -> list(edge).
sub_flow(Edges, FromVertex, ToVertex, Delta) ->
  case find_edge(FromVertex, ToVertex, Edges) of
    null ->
      ?OUTPUT_ERROR(
        "sub_flow/4 - edge not found: e(~w, ~w)", 
        [FromVertex, ToVertex]),
      Edges;
    Edge ->
      #edge{flow=Flow} = Edge, 
      [
        Edge#edge{flow=Flow - Delta}
      ] ++ remove_edge(FromVertex, ToVertex, Edges)
  end.


-spec equal_edges(list(edge),list(edge)) -> {true | false}.
equal_edges([], []) -> true;
equal_edges(Edges01, Edges02) when length(Edges01) /= length(Edges02) ->  false;
equal_edges(Edges01, Edges02) ->
  [Edge01 | Edges01Retain] = Edges01,
  Edge02 = find_edge(
    Edge01#edge.from,
    Edge01#edge.to,
    Edges02),
  case equal_edge(Edge01, Edge02) of
    true ->
      Edges02Retain = remove_edge(
        Edge01#edge.from,
        Edge01#edge.to,
        Edges02),
      equal_edges(Edges01Retain, Edges02Retain);
    false -> false
  end.

equal_edge(Edge, Edge) -> true;
equal_edge(_, _) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% argumenting path functions
-spec exists_argumenting_path(atom(), list(argpath)) -> {true | false}.
exists_argumenting_path(Vertex, ArgPaths) -> 
  case find_argumenting_path(Vertex, ArgPaths) of
     null -> false;
     _ -> true
  end.

-spec find_argumenting_path(
  atom(), 
  list(argpath)) -> {argpath | null}.
find_argumenting_path(_, []) -> 
  null;
find_argumenting_path(Vertex, ArgPaths) ->
  [ArgPath | Retain] = ArgPaths,
  find_argumenting_path(
    Vertex, 
    ArgPath#argpath.vertex, 
    ArgPath, 
    Retain).

-spec find_argumenting_path(
  atom(), 
  atom(), 
  argpath,
  list(argpath)) -> {argpath | null}.
find_argumenting_path(Vertex, Vertex, ArgPath, _) -> ArgPath;
find_argumenting_path(Vertex, _, _, ArgPaths) -> 
  find_argumenting_path(Vertex, ArgPaths).

-spec add_argumenting_path(
  atom(),
  atom(),
  forward | backward,
  list(argpath)
  ) -> list(argpath).
add_argumenting_path(Vertex, PreviousVertex, Direction, ArgPaths) ->
  ArgPaths ++ [
    #argpath{
      vertex=Vertex, 
      previous=PreviousVertex,
      direction=Direction}
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vertex Queue functions
-spec push_vertex_queue(atom(), list(atom())) -> 
  list(atom()).
push_vertex_queue(Vertex, Queue) ->
  Queue ++ [Vertex].

-spec pop_vertex_queue(list(atom())) -> 
  {atom(), list(atom())}.
pop_vertex_queue(Queue) ->
   Vertex = lists:nth(length(Queue), Queue),
   RemoveAfterQueue= lists:droplast(Queue),
   {Vertex, RemoveAfterQueue}.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test functions
find_edge_forward_test() ->
  Edges01 = [],
  Vertex01 = source,
  Expect01 = [],
  Result01 = find_edge_forward(Vertex01, Edges01),
  show_find_edge_forward_test_result(
   "find_edge_forward_test case 01 - empty list: ~w", 
   Expect01,
   Result01),

  Edges02 = [
    #edge{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex02 = source,
  Expect02 = [],
  Result02 = find_edge_forward(Vertex02, Edges02),
  show_find_edge_forward_test_result(
   "find_edge_forward_test case 02 - not exits: ~w", 
   Expect02,
   Result02),

  Edges03 = [
    #edge{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex03 = v1,
  Expect03 = [
    #edge{from=v1, to=v2, flow=0, capacity=0}
  ],
  Result03 = find_edge_forward(Vertex03, Edges03),
  show_find_edge_forward_test_result(
   "find_edge_forward_test case 03 - one exists: ~w", 
   Expect03,
   Result03),

  Edges04 = [
    #edge{from=source, to=v2, flow=3, capacity=8},
    #edge{from=v1, to=v2, flow=4, capacity=8},
    #edge{from=source, to=v1, flow=3, capacity=3}
  ],
  Vertex04 = source,
  Expect04 = [
    #edge{from=source, to=v2, flow=3, capacity=8},
    #edge{from=source, to=v1, flow=3, capacity=3}
  ],
  Result04 = find_edge_forward(Vertex04, Edges04),
  show_find_edge_forward_test_result(
   "case 04 - two exists: ~w", 
   Expect04,
   Result04),
  ok.

show_find_edge_forward_test_result(Text, Expect, Result) ->
  Equals = equal_edges(Expect, Result),
  case Equals of
    true ->
      ?OUTPUT_INFO(Text, [true]);
    false ->
      ?OUTPUT_INFO(Text, [false]),
      ?OUTPUT_INFO("expect: ~w, result: ~w", [Expect, Result])
  end.


find_edge_backward_test() ->
  Edges01 = [],
  Vertex01 = source,
  Expect01 = [],
  Result01 = find_edge_backward(Vertex01, Edges01),
  show_find_edge_backward_test_result(
   "find_edge_backward_test case 01 - empty list: ~w", 
   Expect01,
   Result01),

  Edges02 = [
    #edge{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex02 = source,
  Expect02 = [],
  Result02 = find_edge_backward(Vertex02, Edges02),
  show_find_edge_backward_test_result(
   "find_edge_backward_test case 02 - not exits: ~w", 
   Expect02,
   Result02),

  Edges03 = [
    #edge{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex03 = v2,
  Expect03 = [
    #edge{from=v1, to=v2, flow=0, capacity=0}
  ],
  Result03 = find_edge_backward(Vertex03, Edges03),
  show_find_edge_backward_test_result(
   "find_edge_backward_test case 03 - one exists: ~w", 
   Expect03,
   Result03),

  Edges04 = [
    #edge{from=source, to=v2, flow=3, capacity=8},
    #edge{from=v1, to=v2, flow=4, capacity=8},
    #edge{from=source, to=v1, flow=3, capacity=3}
  ],
  Vertex04 = v2,
  Expect04 = [
    #edge{from=source, to=v2, flow=3, capacity=8},
    #edge{from=v1, to=v2, flow=4, capacity=8}
  ],
  Result04 = find_edge_backward(Vertex04, Edges04),
  show_find_edge_backward_test_result(
   "find_edge_backward_test case 04 - two exists: ~w", 
   Expect04,
   Result04),
  ok.

show_find_edge_backward_test_result(Text, Expect, Result) ->
  Equals = equal_edges(Expect, Result),
  case Equals of
    true ->
      ?OUTPUT_INFO(Text, [true]);
    false ->
      ?OUTPUT_INFO(Text, [false]),
      ?OUTPUT_INFO("expect: ~w, result: ~w", [Expect, Result])
  end.

process_argpath_test() ->
  Edges001 = [
    #edge{from=source, to=v1, flow=0, cost=0, capacity=80},
    #edge{from=v1, to=sink, flow=0, cost=0, capacity=100}],
  ArgPaths001 = [
    #argpath{vertex=sink, previous=v1, direction=forward},
    #argpath{vertex=v1, previous=source, direction=forward}],
  Expect001 = [
    #edge{from=source, to=v1, flow=80, cost=0, capacity=80},
    #edge{from=v1, to=sink, flow=80, cost=0, capacity=100}],
  Result001 = process_argpath(Edges001, ArgPaths001),
  show_process_argpath_test_result(
    "process_argpath_test case 001 - all forward: ~w",
    Expect001,
    Result001),
  %TODO backward id unknown case 
  Edges002 = [
    #edge{from=source, to=v1, flow=10, cost=0, capacity=80},
    #edge{from=v1, to=sink, flow=20, cost=0, capacity=100}],
  ArgPaths002 = [
    #argpath{vertex=sink, previous=v1, direction=backward},
    #argpath{vertex=v1, previous=source, direction=backward}],
  Expect002 = [
    #edge{from=source, to=v1, flow=10, cost=0, capacity=80},
    #edge{from=v1, to=sink, flow=20, cost=0, capacity=100}],
  Result002 = process_argpath(Edges002, ArgPaths002),
  show_process_argpath_test_result(
    "process_argpath_test case 002 - all backward: ~w",
    Expect002,
    Result002),
  ok.

show_process_argpath_test_result(Text, Expect, Result) ->
  case equal_edges(Expect, Result) of
    true ->
      ?OUTPUT_INFO(Text, [true]);
    false ->
      ?OUTPUT_ERROR(Text, [false]),
      ?OUTPUT_ERROR("Expect: ~w, Reuslt: ~w", [Expect, Result])
  end,
  ok.