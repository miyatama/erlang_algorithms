-module(prims_algorithm).

-export([test/0]).

% dist is priority
% rf(vertex_record).
% rd(vertex_record, {vertex,edges,period,dist}).
-record(vertex_record, {vertex, edges}).

% rf(edge_record).
% rd(edge_record, {from_vertex, to_vertex ,cost}).
-record(edge_record, {to_vertex, cost}).

% rf(priority_queue_record).
% rd(priority_queue_record, {vertex, cost}).
-record(priority_queue_record, {vertex, cost}).

-define(OUTPUT_DEBUG(S),
        io:fwrite("[DEBUG] prims_algorithm: " ++ S ++ "~n")).

-define(OUTPUT_DEBUG(S, Args),
        io:fwrite("[DEBUG] prims_algorithm: " ++ S ++ "~n",
                  Args)).

-define(OUTPUT_INFO(S),
        io:fwrite("[INFO] prims_algorithm: " ++ S ++ "~n")).

-define(OUTPUT_INFO(S, Args),
        io:fwrite("[INFO] prims_algorithm: " ++ S ++ "~n",
                  Args)).

-define(MAX_DIST, 9999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec test() -> ok.

test() ->
    Route = search(get_test_data(1)),
    show_routes(Route, 0, 3),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(list()) -> list().

search(Nodes) ->
    Keys = generate_initial_keys(Nodes),
    Periods = generate_initial_periods(Nodes),
    SetStartKeys = maps:put(0, 0, Keys),
    PriorityQueue = generate_priority_queue(Nodes,
                                            SetStartKeys),
    {_, _, Routes} = create_paths(Nodes,
                                  PriorityQueue,
                                  SetStartKeys,
                                  Periods),
    Routes.

create_paths(_, [], Keys, Periods) ->
    {[], Keys, Periods};
create_paths(Nodes, PQ, Keys, Periods)
    when is_list(Nodes) ->
    {PriorityRec, PopdPQ} = get_min(PQ),
    Node =
        lists:keyfind(PriorityRec#priority_queue_record.vertex,
                      #vertex_record.vertex,
                      Nodes),
    {NewPQ, NewKeys, NewPeriods} = create_paths(Node,
                                                PopdPQ,
                                                Keys,
                                                Periods),
    create_paths(Nodes, NewPQ, NewKeys, NewPeriods);
create_paths(Node, PQ, Keys, Periods) ->
    create_paths(Node,
                 Node#vertex_record.edges,
                 PQ,
                 Keys,
                 Periods).

create_paths(_, [], PQ, Keys, Periods) ->
    {PQ, Keys, Periods};
create_paths(Node, Edges, PQ, Keys, Periods)
    when is_list(Edges) ->
    [Edge | EdgeRetain] = Edges,
    {NewPQ, NewKeys, NewPeirods} = case
                                       exists_vertex_in_priority_queue(PQ,
                                                                       Edge#edge_record.to_vertex)
                                       of
                                       true ->
                                           calculate_cost(Node,
                                                          Edge,
                                                          PQ,
                                                          Keys,
                                                          Periods);
                                       false -> {PQ, Keys, Periods}
                                   end,
    create_paths(Node,
                 EdgeRetain,
                 NewPQ,
                 NewKeys,
                 NewPeirods).

calculate_cost(Node, Edge, PQ, Keys, Periods) ->
    EdgeCost = Edge#edge_record.cost,
    CurrentCost = maps:get(Edge#edge_record.to_vertex,
                           Keys),
    case EdgeCost < CurrentCost of
        true ->
            NewPeriods = maps:put(Edge#edge_record.to_vertex,
                                  Node#vertex_record.vertex,
                                  Periods),
            NewKeys = maps:put(Edge#edge_record.to_vertex,
                               EdgeCost,
                               Keys),
            NewPQ = decrease_priority(PQ,
                                      Edge#edge_record.to_vertex,
                                      EdgeCost),
            {NewPQ, NewKeys, NewPeriods};
        false -> {PQ, Keys, Periods}
    end.

generate_priority_queue([], _) -> [];
generate_priority_queue(Nodes, Keys) ->
    [Node | Retain] = Nodes,
    Cost = maps:get(Node#vertex_record.vertex, Keys),
    [#priority_queue_record{vertex =
                                Node#vertex_record.vertex,
                            cost = Cost}]
        ++ generate_priority_queue(Retain, Keys).

generate_initial_keys(Nodes) ->
    [Vertex | Retain] = [X
                         || #vertex_record{vertex = X} <- Nodes],
    Keys = maps:new(),
    AddVertexKeys = maps:put(Vertex, ?MAX_DIST, Keys),
    generate_initial_keys(AddVertexKeys, Retain).

generate_initial_keys(Keys, []) -> Keys;
generate_initial_keys(Keys, Vertexes) ->
    [Vertex | Retain] = Vertexes,
    AddVertexKeys = maps:put(Vertex, ?MAX_DIST, Keys),
    generate_initial_keys(AddVertexKeys, Retain).

generate_initial_periods(Nodes) ->
    [Vertex | Retain] = [X
                         || #vertex_record{vertex = X} <- Nodes],
    Periods = maps:new(),
    AddVertexPeriods = maps:put(Vertex, ?MAX_DIST, Periods),
    generate_initial_periods(AddVertexPeriods, Retain).

generate_initial_periods(Periods, []) -> Periods;
generate_initial_periods(Periods, Vertexes) ->
    [Vertex | Retain] = Vertexes,
    AddVertexPeriods = maps:put(Vertex, ?MAX_DIST, Periods),
    generate_initial_periods(AddVertexPeriods, Retain).

exists_vertex_in_priority_queue(PQ, Vertex) ->
    Rec = lists:keyfind(Vertex,
                        #priority_queue_record.vertex,
                        PQ),
    case Rec of
        false -> false;
        _ -> true
    end.

decrease_priority(PQ, Vertex, Priority) ->
    TargetNode = lists:keyfind(Vertex,
                               #priority_queue_record.vertex,
                               PQ),
    OtherNodes = lists:keydelete(Vertex,
                                 #priority_queue_record.vertex,
                                 PQ),
    case TargetNode of
        false ->
            [#priority_queue_record{vertex = Vertex,
                                    cost = Priority}
             | OtherNodes];
        _ ->
            [TargetNode#priority_queue_record{cost = Priority}
             | OtherNodes]
    end.

-spec get_min(list()) -> {map() | null, list()}.

get_min([]) -> {null, []};
get_min(PQ) ->
    [Rec | _] = PQ,
    get_min(PQ, Rec).

get_min(PQ, MinRec) ->
    MinCost = MinRec#priority_queue_record.cost,
    Lowers = [#priority_queue_record{vertex = Vertex,
                                     cost = Cost}
              || #priority_queue_record{vertex = Vertex, cost = Cost}
                     <- PQ,
                 Cost < MinCost],
    case length(Lowers) of
        0 ->
            PQ1 =
                lists:keydelete(MinRec#priority_queue_record.vertex,
                                #priority_queue_record.vertex,
                                PQ),
            {MinRec, PQ1};
        _ ->
            [Lower | _] = Lowers,
            get_min(PQ, Lower)
    end.

show_routes(RouteInfo, FromVertex, ToVertex) ->
    ?OUTPUT_INFO("show_routes - from: ~w, to: ~w",
                 [FromVertex, ToVertex]),
    Routes = build_route(RouteInfo, FromVertex, ToVertex),
    ?OUTPUT_INFO("show_routes: ~w",
                 [lists:reverse(Routes)]).

build_route(_, FromVertex, FromVertex) -> [FromVertex];
build_route(RouteInfo, FromVertex, ToVertex) ->
    NextVertex = maps:get(ToVertex, RouteInfo),
    [ToVertex] ++
        build_route(RouteInfo, FromVertex, NextVertex).

get_test_data(1) ->
    [#vertex_record{vertex = 0,
                    edges =
                        [#edge_record{to_vertex = 1, cost = 2},
                         #edge_record{to_vertex = 4, cost = 4},
                         #edge_record{to_vertex = 3, cost = 8}]},
     #vertex_record{vertex = 1,
                    edges =
                        [#edge_record{to_vertex = 0, cost = 2},
                         #edge_record{to_vertex = 2, cost = 3}]},
     #vertex_record{vertex = 2,
                    edges =
                        [#edge_record{to_vertex = 1, cost = 3},
                         #edge_record{to_vertex = 4, cost = 1},
                         #edge_record{to_vertex = 3, cost = 5}]},
     #vertex_record{vertex = 4,
                    edges =
                        [#edge_record{to_vertex = 0, cost = 4},
                         #edge_record{to_vertex = 2, cost = 1},
                         #edge_record{to_vertex = 3, cost = 7}]},
     #vertex_record{vertex = 3,
                    edges =
                        [#edge_record{to_vertex = 4, cost = 7},
                         #edge_record{to_vertex = 2, cost = 5},
                         #edge_record{to_vertex = 0, cost = 8}]}].
