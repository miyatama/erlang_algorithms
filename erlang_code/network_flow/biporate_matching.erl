-module(biporate_matching).

-export([test/0]).

-import(ford_fullkerson,
  [process_argpath/2,
    find_edge/3,
    find_edge_forward/2,
    find_edge_backward/2,
    exists_argumenting_path/2,
    add_argumenting_path/4]).

% name -> atom()
% skills -> list(atom())
-record(people_record, {name, skills}).

% name -> atom()
% need_skills -> list(atom())
-record(job_record, {name, need_skills}).

-include("network_flow.hrl").

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
  ) -> list(edge).
generate_graph_edges([], _) -> [];
generate_graph_edges(Jobs, Peoples) ->
  [Job | JobsRetain] = Jobs,
  Edges = generate_graph_edges_per_job(Job, Peoples),
  Edges ++ generate_graph_edges(JobsRetain, Peoples).

-spec generate_graph_edges_per_job(
    job_record,
    list(people_record)
  ) -> list(edge).
generate_graph_edges_per_job(_, []) -> [];
generate_graph_edges_per_job(Job, Peoples) ->
  [People | PeoplesRetain] = Peoples,
  Edge = case match_job_and_people(Job, People) of
    true ->
      [
        #edge{
           from=source,
           to=Job#job_record.name,
           flow=0,
           capacity=1
        },
        #edge{
           from=Job#job_record.name,
           to=People#people_record.name,
           flow=0,
           capacity=1
        },
        #edge{
           from=People#people_record.name,
           to=sink,
           flow=0,
           capacity=1
        }
      ];
    false -> []
  end,
  Edge ++ generate_graph_edges_per_job(Job, PeoplesRetain).

-spec remove_duplicate_edge(list(edge)) -> list(edge).
remove_duplicate_edge([]) -> [];
remove_duplicate_edge(Edges) ->
  [Edge | Retain] = Edges,
  case find_edge(
        Edge#edge.from,
        Edge#edge.to,
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
  #edge{
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