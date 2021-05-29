-module(floyd_warshall).

-export([test/0]).

% dist is priority
% rf(vertex_record).
% rd(vertex_record, {vertex,edges,period,dist}).
-record(vertex_record, {vertex, edges}).

% rf(edge_record).
% rd(edge_record, {from_vertex, to_vertex ,weight}).
-record(edge_record, {from_vertex, to_vertex, weight}).

% rf(dist_record).
% rd(dist_record, {from_vertex, to_vertex, cost, period}).
-record(dist_record,
        {from_vertex, to_vertex, cost, period}).

-define(OUTPUT_DEBUG(S),
        io:fwrite("[DEBUG] floyd_warshall: " ++ S ++ "~n")).

-define(OUTPUT_DEBUG(S, Args),
        io:fwrite("[DEBUG] floyd_warshall: " ++ S ++ "~n",
                  Args)).

-define(OUTPUT_INFO(S),
        io:fwrite("[INFO] floyd_warshall: " ++ S ++ "~n")).

-define(OUTPUT_INFO(S, Args),
        io:fwrite("[INFO] floyd_warshall: " ++ S ++ "~n",
                  Args)).

-define(MAX_DIST, 9999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
    Route = search(get_test_data(1)),
    show_routes(Route, start, stop),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec search(list()) -> list().

search([]) -> [];
search(Nodes) ->
    Dists = generate_first_dists(Nodes),
    calculate_cost(Nodes, Dists).

-spec calculate_cost([map()], [map()]) -> [map()].

calculate_cost([], _) -> [];
calculate_cost(_, []) -> [];
calculate_cost(Nodes, Dists) ->
    Vertexes = [Vertex
                || #vertex_record{vertex = Vertex} <- Nodes],
    calculate_cost(Dists, Vertexes, Vertexes, Vertexes).

calculate_cost(Dists, [], _, _) -> Dists;
calculate_cost(Dists, _, [], _) -> Dists;
calculate_cost(Dists, _, _, []) -> Dists;
calculate_cost(Dists, Vertexes1, Vertexes2, Vertexes3)
    when is_list(Vertexes1) and is_list(Vertexes2) and
             is_list(Vertexes3) ->
    [Vertex1 | Vertexes1Retain] = Vertexes1,
    NewDists = calculate_cost(Dists,
                              Vertex1,
                              Vertexes2,
                              Vertexes3),
    calculate_cost(NewDists,
                   Vertexes1Retain,
                   Vertexes2,
                   Vertexes3);
calculate_cost(Dists, Vertex1, Vertexes2, Vertexes3)
    when not is_list(Vertex1) and is_list(Vertexes2) and
             is_list(Vertexes3) ->
    [Vertex2 | Vertexes2Retain] = Vertexes2,
    NewDists = calculate_cost(Dists,
                              Vertex1,
                              Vertex2,
                              Vertexes3),
    calculate_cost(NewDists,
                   Vertex1,
                   Vertexes2Retain,
                   Vertexes3);
calculate_cost(Dists, Vertex1, Vertex2, Vertexes3)
    when not is_list(Vertex1) and not is_list(Vertex2) and
             is_list(Vertexes3) ->
    [Vertex3 | Vertexes3Retain] = Vertexes3,
    NewDists = calculate_cost(Dists,
                              Vertex1,
                              Vertex2,
                              Vertex3),
    calculate_cost(NewDists,
                   Vertex1,
                   Vertex2,
                   Vertexes3Retain);
calculate_cost(Dists, Vertex1, Vertex2, Vertex3)
    when not is_list(Vertex1) and not is_list(Vertex2) and
             not is_list(Vertex3) ->
    Dist1 = get_dist(Dists, Vertex2, Vertex1),
    Dist2 = get_dist(Dists, Vertex1, Vertex3),
    Dist3 = get_dist(Dists, Vertex2, Vertex3),
    NewLen = Dist1#dist_record.cost +
                 Dist2#dist_record.cost,
    case NewLen < Dist3#dist_record.cost of
        true ->
            set_dist_and_period(Dists,
                                Vertex2,
                                Vertex3,
                                NewLen,
                                Dist2#dist_record.period);
        false -> Dists
    end.

% generate dist_records from vertex_records
-spec generate_first_dists(list()) -> [map()].

generate_first_dists([]) -> [];
generate_first_dists(Nodes) ->
    Vertexes = [Vertex
                || #vertex_record{vertex = Vertex} <- Nodes],
    Edges = convert_nodes_to_edges(Nodes),
    generate_first_dists(Vertexes, Vertexes, Edges).

generate_first_dists([], _, _) -> [];
generate_first_dists(_, [], _) -> [];
generate_first_dists(FromVertexes, ToVertexes, Edges)
    when is_list(FromVertexes) and is_list(ToVertexes) ->
    [FromVertex | FromVertexesRetain] = FromVertexes,
    generate_first_dists(FromVertex, ToVertexes, Edges) ++
        generate_first_dists(FromVertexesRetain,
                             ToVertexes,
                             Edges);
generate_first_dists(FromVertex, ToVertexes, Edges)
    when is_list(ToVertexes) ->
    [ToVertex | ToVertexesRetain] = ToVertexes,
    generate_first_dists(FromVertex, ToVertex, Edges) ++
        generate_first_dists(FromVertex,
                             ToVertexesRetain,
                             Edges);
generate_first_dists(FromVertex, FromVertex, _) ->
    [#dist_record{from_vertex = FromVertex,
                  to_vertex = FromVertex, cost = 0, period = null}];
generate_first_dists(FromVertex, ToVertex, Edges) ->
    Edge = get_edge(Edges, FromVertex, ToVertex),
    Cost = case Edge of
               null -> ?MAX_DIST;
               _ -> Edge#edge_record.weight
           end,
    [#dist_record{from_vertex = FromVertex,
                  to_vertex = ToVertex, cost = Cost, period = FromVertex}].

-spec show_routes(list(), integer() | start | stop,
                  integer() | start | stop) -> ok.

show_routes([], _, _) ->
    ?OUTPUT_INFO("show_routes/1 - route: ~w", [nothing]),
    ok;
show_routes(Dists, FromVertex, ToVertex) ->
    Route = lists:reverse(build_routes(Dists, FromVertex, ToVertex)),
    ?OUTPUT_INFO("show_routes/1 - route: ~w", [Route]),
    ok.

-spec build_routes(list(), integer() | start | stop,
                   integer() | start | stop) -> list().

build_routes(_, FromVertex, FromVertex) -> [FromVertex];
build_routes(Dists, FromVertex, ToVertex) ->
    ?OUTPUT_DEBUG("build_routes: from: ~w, to: ~w",
                  [FromVertex, ToVertex]),
    DistRec = get_dist(Dists, FromVertex, ToVertex),
    [ToVertex] ++
        build_routes(Dists,
                     FromVertex,
                     DistRec#dist_record.period).

-spec get_dist(list(), integer() | start | stop,
               integer() | start | stop) -> map() | null.

get_dist([], _, _) -> null;
get_dist(Dists, FromVertex, ToVertex) ->
    [Dist | DistsRetain] = Dists,
    case {Dist#dist_record.from_vertex,
          Dist#dist_record.to_vertex}
        of
        {FromVertex, ToVertex} -> Dist;
        _ -> get_dist(DistsRetain, FromVertex, ToVertex)
    end.

get_dist_exclusive([], _, _) -> [];
get_dist_exclusive(Dists, FromVertex, ToVertex) ->
    [Dist | DistsRetain] = Dists,
    case {Dist#dist_record.from_vertex,
          Dist#dist_record.to_vertex}
        of
        {FromVertex, ToVertex} -> DistsRetain;
        _ ->
            [Dist] ++
                get_dist_exclusive(DistsRetain, FromVertex, ToVertex)
    end.

set_dist_and_period(Dists, FromVertex, ToVertex,
                    NewCost, NewPeriod) ->
    ?OUTPUT_DEBUG("set_dist_and_period: edge(~w, ~w), new "
                  "cost: ~w, new period: ~w",
                  [FromVertex, ToVertex, NewCost, NewPeriod]),
    UpdateDist = get_dist(Dists, FromVertex, ToVertex),
    ExclusiveDist = get_dist_exclusive(Dists,
                                       FromVertex,
                                       ToVertex),
    [UpdateDist#dist_record{cost = NewCost,
                            period = NewPeriod}]
        ++ ExclusiveDist.

-spec convert_nodes_to_edges([map()]) -> [map()].

convert_nodes_to_edges([]) -> [];
convert_nodes_to_edges(Nodes) ->
    [Node | Retain] = Nodes,
    Node#vertex_record.edges ++
        convert_nodes_to_edges(Retain).

-spec get_edge([map()], integer() | start | stop,
               integer() | start | stop) -> map().

get_edge([], _, _) -> null;
get_edge(Edges, FromVertex, ToVertex) ->
    [Edge | Retain] = Edges,
    case {Edge#edge_record.from_vertex,
          Edge#edge_record.to_vertex}
        of
        {FromVertex, ToVertex} -> Edge;
        _ -> get_edge(Retain, FromVertex, ToVertex)
    end.

-spec get_test_data(integer()) -> list().

get_test_data(1) ->
    [#vertex_record{vertex = start,
                    edges =
                        [#edge_record{from_vertex = start, to_vertex = 1,
                                      weight = 2},
                         #edge_record{from_vertex = start, to_vertex = 4,
                                      weight = 4}]},
     #vertex_record{vertex = stop,
                    edges =
                        [#edge_record{from_vertex = stop, to_vertex = start,
                                      weight = 8}]},
     #vertex_record{vertex = 1,
                    edges =
                        [#edge_record{from_vertex = 1, to_vertex = 2,
                                      weight = 3}]},
     #vertex_record{vertex = 2,
                    edges =
                        [#edge_record{from_vertex = 2, to_vertex = 4,
                                      weight = 1},
                         #edge_record{from_vertex = 2, to_vertex = stop,
                                      weight = 5}]},
     #vertex_record{vertex = 4,
                    edges =
                        [#edge_record{from_vertex = 4, to_vertex = 3,
                                      weight = 7}]}].
