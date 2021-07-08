-module(augmenting_path).

-export([generate_argumenting_path/1]).

-import(ford_fullkerson, 
  [find_edge/3,
   add_argumenting_path/4]).

-include("network_flow.hrl").

-record(priority_queue_record, {vertex, dist}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
  case (?LOG_LEVEL) =< 1 of
    true -> io:fwrite("[DEBUG] augmenting_path: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] augmenting_path: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] augmenting_path: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] augmenting_path: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] augmenting_path: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] augmenting_path: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(MAX_DIST,  9999999).

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

remove_vertexes([], Vertexes) -> Vertexes;
remove_vertexes(RemoveVertexes, Vertexes) ->
  [RemoveVertex | Retain] = RemoveVertexes,
  NewVertexes = lists:delete(RemoveVertex, Vertexes),
  remove_vertexes(Retain, NewVertexes).

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
