-module(ford_fullkerson_with_breadth_first).

-export([test/0]).

% from -> atom()
% to -> atom()
% flow -> int
% capacity -> int
-record(edge_record, {from, to, flow, capacity}).

% direction -> atom: forward | backward
-record(argumenting_path_record, {vertex, previous, direction}).

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
  find_edge_forward_test(),
  find_edge_backward_test(),
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

-spec generate_argumenting_path(list(edge_record)) -> list(argumenting_path_record).
generate_argumenting_path(Graph) ->
  Queue = [source],
  ArgPaths = [
    #argumenting_path_record{
      vertex=source,
      previous=null,
      direction=none}],
  generate_argumenting_path(Graph, Queue, ArgPaths).

-spec generate_argumenting_path(
    list(edge_record), 
    list(atom()), 
    list(argumenting_path_record)
  ) -> list(argumenting_path_record).
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
    list(edge_record),
    list(atom()), 
    list(argumenting_path_record),
    atom()) ->  
  {
    true | false,
    list(atom()), 
    list(argumenting_path_record)
  }.
generate_argumenting_path_forward(Edges, PathQueue, ArgPaths, Vertex) ->
  ForwardEdges = find_edge_forward(Vertex, Edges),
  generate_argumenting_path_forward(Edges, PathQueue, ArgPaths, Vertex, ForwardEdges).

-spec generate_argumenting_path_forward(
    list(edge_record),
    list(atom()), 
    list(argumenting_path_record),
    atom(),
    list(edge_record)) -> 
  {
    true | false,
    list(atom()), 
    list(argumenting_path_record)
  }.
generate_argumenting_path_forward(_, PathQueue, ArgPaths, _, []) -> 
  {false, PathQueue, ArgPaths};
generate_argumenting_path_forward(Edges, PathQueue, ArgPaths, Vertex, ForwardEdges) -> 
  [ForwardEdge | ForwardEdgesRetain] = ForwardEdges,
  Exists = exists_argumenting_path(
    ForwardEdge#edge_record.to, 
    ArgPaths),
  FullTank = (ForwardEdge#edge_record.capacity > ForwardEdge#edge_record.flow),
  ArrivalSink = (ForwardEdge#edge_record.to  ==  sink),
  ?OUTPUT_DEBUG(
     "generate_argumenting_path_forward/5 - ~w in arg path exists: ~w, full tank: ~w, arrival sink: ~w",
    [ForwardEdge#edge_record.to, Exists, FullTank , ArrivalSink ]),
  {Return, PathQueue2, ArgPaths2} = case {Exists, FullTank, ArrivalSink} of
    % not arrival,not limit and not arrival sink
    {false, true, false} -> 
      ?OUTPUT_DEBUG("generate_argumenting_path_forward - ~w", [not_arrival_sink]),
      NewArgPaths = add_argumenting_path(
        ForwardEdge#edge_record.to, 
        Vertex, 
        forward, 
        ArgPaths),
      NewPathQueue = insert_vertex_queue(
        ForwardEdge#edge_record.to, 
        PathQueue),
      {false, NewPathQueue, NewArgPaths};
    % not arrival,not limit and arrival sink
    {false, true, true} ->
      ?OUTPUT_DEBUG("generate_argumenting_path_forward - ~w", [arrival_sink]),
      NewArgPaths = add_argumenting_path(
        ForwardEdge#edge_record.to, 
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
    list(edge_record),
    list(atom()),
    list(argumenting_path_record),
    atom() 
  ) -> {list(atom()), list(argumenting_path_record)}.
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
    list(edge_record),
    list(atom()),
    list(argumenting_path_record),
    atom(),
    list(edge_record)
  ) -> {list(atom()), list(argumenting_path_record)}.
generate_argumenting_path_backward(_, PathQueue, ArgPaths, _, []) ->
  {PathQueue, ArgPaths};
generate_argumenting_path_backward(Edges, PathQueue, ArgPaths, Vertex, BackwardEdges) ->
  [BackwardEdge | BackwardEdgesRetain] = BackwardEdges,
  ExistsArgPath = exists_argumenting_path(
    BackwardEdge#edge_record.from,
    ArgPaths),
  ExistsFlow = BackwardEdge#edge_record.flow > 0,
  ?OUTPUT_DEBUG(
    "generate_argumenting_path_backward/5 - exists arg path: ~w, exists flow: ~w",
    [ExistsArgPath, ExistsFlow]),
  {PathQueue2, ArgPaths2} = case {ExistsArgPath, ExistsFlow} of
    % not arrival and exists flow
    {false, true} ->
      NewArgPaths = add_argumenting_path(
        BackwardEdge#edge_record.from,
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
% graph functions

% return forward edge for specify vertex
% ex) soruce -> v1, v2 -> v3 -> sink
% v3 forward edge
% 0: v3 -> sink 
-spec find_edge_forward(atom(), list(edge_record)) -> list(edge_record).
find_edge_forward(_, []) ->  [];
find_edge_forward(Vertex, Graph) -> 
  ?OUTPUT_DEBUG(
     "find_edge_forward/2 - vertex: ~w",
     [Vertex]),
  [Edge | GraphRetain] = Graph,
  case Edge#edge_record.from of
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
-spec find_edge_backward(atom(), list(edge_record)) -> list(edge_record).
find_edge_backward(_, []) ->  [];
find_edge_backward(Vertex, Graph) -> 
  [Edge | GraphRetain] = Graph,
  case Edge#edge_record.to of
    Vertex ->
      [Edge];
    _ ->
      []
  end ++ find_edge_backward(Vertex, GraphRetain).

-spec find_edge(
    atom(), 
    atom(), 
    list(edge_record)
  ) -> {edge_record | null}.
find_edge(_, _, []) -> null;
find_edge(FromVertex, ToVertex, Edges) -> 
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

-spec equal_edges(list(edge_record),list(edge_record)) -> {true | false}.
equal_edges([], []) -> true;
equal_edges(Edges01, Edges02) when length(Edges01) /= length(Edges02) ->  false;
equal_edges(Edges01, Edges02) ->
  [Edge01 | Edges01Retain] = Edges01,
  Edge02 = find_edge(
    Edge01#edge_record.from,
    Edge01#edge_record.to,
    Edges02),
  case equal_edge(Edge01, Edge02) of
    true ->
      Edges02Retain = remove_edge(
        Edge01#edge_record.from,
        Edge01#edge_record.to,
        Edges02),
      equal_edges(Edges01Retain, Edges02Retain);
    false -> false
  end.

equal_edge(Edge, Edge) -> true;
equal_edge(_, _) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% argumenting path functions
-spec exists_argumenting_path(atom(), list(argumenting_path_record)) -> {true | false}.
exists_argumenting_path(Vertex, ArgPaths) -> 
  case find_argumenting_path(Vertex, ArgPaths) of
     null -> false;
     _ -> true
  end.

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
   #edge_record{from=source, to=v1, flow=0, capacity=10},
   #edge_record{from=v1, to=sink, flow=0, capacity=10}
  ];

generate_initial_graph(2) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=5},
   #edge_record{from=source, to=v2, flow=0, capacity=5},
   #edge_record{from=v1, to=sink, flow=0, capacity=10},
   #edge_record{from=v2, to=v1, flow=0, capacity=5}
  ];

generate_initial_graph(3) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=5},
   #edge_record{from=source, to=v2, flow=0, capacity=5},
   #edge_record{from=v1, to=sink, flow=0, capacity=10},
   #edge_record{from=v2, to=v1, flow=0, capacity=3}
  ];

generate_initial_graph(4) ->
  [
   #edge_record{from=source, to=v1, flow=0, capacity=10},
   #edge_record{from=source, to=v2, flow=0, capacity=5},
   #edge_record{from=source, to=v3, flow=0, capacity=4},
   #edge_record{from=v1, to=v4, flow=0, capacity=5},
   #edge_record{from=v1, to=v2, flow=0, capacity=3},
   #edge_record{from=v2, to=v4, flow=0, capacity=2},
   #edge_record{from=v2, to=v3, flow=0, capacity=5},
   #edge_record{from=v3, to=v5, flow=0, capacity=8},
   #edge_record{from=v4, to=sink, flow=0, capacity=7},
   #edge_record{from=v5, to=sink, flow=0, capacity=11}
  ];

generate_initial_graph(_) ->
  [].

show_result([]) -> ok;
show_result(Edges) ->
  [Edge | Retain] = Edges,
  #edge_record{
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
    #edge_record{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex02 = source,
  Expect02 = [],
  Result02 = find_edge_forward(Vertex02, Edges02),
  show_find_edge_forward_test_result(
   "find_edge_forward_test case 02 - not exits: ~w", 
   Expect02,
   Result02),

  Edges03 = [
    #edge_record{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex03 = v1,
  Expect03 = [
    #edge_record{from=v1, to=v2, flow=0, capacity=0}
  ],
  Result03 = find_edge_forward(Vertex03, Edges03),
  show_find_edge_forward_test_result(
   "find_edge_forward_test case 03 - one exists: ~w", 
   Expect03,
   Result03),

  Edges04 = [
    #edge_record{from=source, to=v2, flow=3, capacity=8},
    #edge_record{from=v1, to=v2, flow=4, capacity=8},
    #edge_record{from=source, to=v1, flow=3, capacity=3}
  ],
  Vertex04 = source,
  Expect04 = [
    #edge_record{from=source, to=v2, flow=3, capacity=8},
    #edge_record{from=source, to=v1, flow=3, capacity=3}
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
    #edge_record{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex02 = source,
  Expect02 = [],
  Result02 = find_edge_backward(Vertex02, Edges02),
  show_find_edge_backward_test_result(
   "find_edge_backward_test case 02 - not exits: ~w", 
   Expect02,
   Result02),

  Edges03 = [
    #edge_record{from=v1, to=v2, flow=0, capacity=0}
  ],
  Vertex03 = v2,
  Expect03 = [
    #edge_record{from=v1, to=v2, flow=0, capacity=0}
  ],
  Result03 = find_edge_backward(Vertex03, Edges03),
  show_find_edge_backward_test_result(
   "find_edge_backward_test case 03 - one exists: ~w", 
   Expect03,
   Result03),

  Edges04 = [
    #edge_record{from=source, to=v2, flow=3, capacity=8},
    #edge_record{from=v1, to=v2, flow=4, capacity=8},
    #edge_record{from=source, to=v1, flow=3, capacity=3}
  ],
  Vertex04 = v2,
  Expect04 = [
    #edge_record{from=source, to=v2, flow=3, capacity=8},
    #edge_record{from=v1, to=v2, flow=4, capacity=8}
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
