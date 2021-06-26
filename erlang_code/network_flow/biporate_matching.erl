-module(biporate_matching).

-export([test/0]).

% name -> atom()
% skills -> list(atom())
-record(people_record, {name, skills}).

% name -> atom()
% need_skills -> list(atom())
-record(job_record, {name, need_skills}).

% from -> atom() as job
% to -> atom() as people
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
    true -> io:fwrite("[DEBUG] biporate_matching: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] biporate_matching: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] biporate_matching: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] biporate_matching: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] biporate_matching: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] biporate_matching: " ++ S ++ "~n", Args);
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
      NewPathQueue = push_vertex_queue(
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
-spec find_edge_forward(atom(), list(edge_record)) -> list(edge_record).
find_edge_forward(_, []) ->  [];
find_edge_forward(Vertex, Graph) -> 
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
generate_initial_graph(TestCase) ->
  Jobs=generate_initial_jobs(TestCase),
  Peoples=generate_initial_peoples(TestCase),
  Edges = generate_graph_edges(Jobs, Peoples),
  ShapedEdges = remove_duplicate_edge(Edges),
  show_result(ShapedEdges),
  ShapedEdges.

-spec generate_graph_edges(
    list(job_record),
    list(people_record)
  ) -> list(edge_record).
generate_graph_edges([], _) -> [];
generate_graph_edges(Jobs, Peoples) ->
  [Job | JobsRetain] = Jobs,
  Edges = generate_graph_edges_per_job(Job, Peoples),
  Edges ++ generate_graph_edges(JobsRetain, Peoples).

-spec generate_graph_edges_per_job(
    job_record,
    list(people_record)
  ) -> list(edge_record).
generate_graph_edges_per_job(_, []) -> [];
generate_graph_edges_per_job(Job, Peoples) ->
  [People | PeoplesRetain] = Peoples,
  Edge = case match_job_and_people(Job, People) of
    true ->
      [
        #edge_record{
           from=source,
           to=Job#job_record.name,
           flow=0,
           capacity=1
        },
        #edge_record{
           from=Job#job_record.name,
           to=People#people_record.name,
           flow=0,
           capacity=1
        },
        #edge_record{
           from=People#people_record.name,
           to=sink,
           flow=0,
           capacity=1
        }
      ];
    false -> []
  end,
  Edge ++ generate_graph_edges_per_job(Job, PeoplesRetain).

-spec remove_duplicate_edge(list(edge_record)) -> list(edge_record).
remove_duplicate_edge([]) -> [];
remove_duplicate_edge(Edges) ->
  [Edge | Retain] = Edges,
  case find_edge(
        Edge#edge_record.from,
        Edge#edge_record.to,
        Retain) of
    null -> [Edge];
    _ -> []
  end ++ remove_duplicate_edge(Retain).

-spec match_job_and_people(job_record, people_record) -> true | false.
match_job_and_people(Job, People) ->
  NeedSkills = Job#job_record.need_skills,
  HasSkills = People#people_record.skills,
  in_skills(NeedSkills, HasSkills).

in_skills([], _) -> true;
in_skills(NeedSkills, HasSkills) ->
  [NeedSkill | NeedSkillsRetain] =  NeedSkills,
  case lists:any(fun(Skill) -> NeedSkill == Skill end, HasSkills) of
    false -> false;
    true -> 
      in_skills(NeedSkillsRetain, HasSkills)
  end.

-spec generate_initial_peoples(integer()) -> list(people_record).
generate_initial_peoples(1) -> 
  [
    #people_record{name=kenji, skills=[jump]},
    #people_record{name=satoru, skills=[jump, run]},
    #people_record{name=satomi, skills=[run]}
  ];
generate_initial_peoples(2) -> 
  [
    #people_record{name=aramaki, skills=[power, operation]},
    #people_record{name=kusanagi, skills=[plannning, hack, attack, operation]},
    #people_record{name=batoh, skills=[hack, attack, operation]},
    #people_record{name=togusa, skills=[pllanning, attack, operation]},
    #people_record{name=bommer, skills=[hack, attack, operation]},
    #people_record{name=pazzue, skills=[attack, operation]},
    #people_record{name=saitou, skills=[snipe, attack, operation]},
    #people_record{name=ishikawa, skills=[hack, attack, operation]}
  ].

-spec generate_initial_jobs(integer()) -> list(job_record).
generate_initial_jobs(1) -> 
  [
    #job_record{name=road, need_skills=[run]},
    #job_record{name=rock, need_skills=[jump]},
    #job_record{name=hole, need_skills=[jump, run]}
  ];
generate_initial_jobs(2) -> 
  [
    #job_record{name=manager01, need_skills=[power]},
    #job_record{name=major01, need_skills=[plannning]},
    #job_record{name=attacker01, need_skills=[attack]},
    #job_record{name=attacker02, need_skills=[attack]},
    #job_record{name=hacker01, need_skills=[hack]},
    #job_record{name=hacker02, need_skills=[hack]},
    #job_record{name=sniper01, need_skills=[snipe]},
    #job_record{name=operator01, need_skills=[operation]}
  ].

show_result([]) -> ok;
show_result(Edges) ->
  [Edge | Retain] = Edges,
  #edge_record{
    from=FromVertex,
    to=ToVertex,
    flow=Flow,
    capacity=Capacity} = Edge,
  SourceOrSink = (FromVertex == source) or 
    (ToVertex == sink),
  case {Flow > 0, SourceOrSink} of
    {true, false} ->
      ?OUTPUT_INFO(
        "~w to ~w: ~w/~w", 
        [
         FromVertex,
         ToVertex,
         Flow,
         Capacity
        ]);
    _ ->
      ?OUTPUT_DEBUG(
        "~w to ~w: ~w/~w", 
        [
         FromVertex,
         ToVertex,
         Flow,
         Capacity
        ]),
      ok
  end,
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
