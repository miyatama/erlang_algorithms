-module(dijkstras_algorithm_dense_graph).

-export([test/0]).

% dist is priority
% rf(vertex_record).
% rd(vertex_record, {vertex,edges,visit,period,dist}).
-record(vertex_record,
        {vertex, edges, visit, period, dist}).

% rf(edge_record).
% rd(edge_record, {vertex,edges,status,period,dist}).
-record(edge_record, {vertex, weight}).

-define(OUTPUT_DEBUG(S),
        io:fwrite("[DEBUG] dijkstras_algorithm_dense_graph: " ++
                      S ++ "~n")).

-define(OUTPUT_DEBUG(S, Args),
        io:fwrite("[DEBUG] dijkstras_algorithm_dense_graph: " ++
                      S ++ "~n",
                  Args)).

-define(OUTPUT_INFO(S),
        io:fwrite("[INFO] dijkstras_algorithm_dense_graph: " ++
                      S ++ "~n")).

-define(OUTPUT_INFO(S, Args),
        io:fwrite("[INFO] dijkstras_algorithm_dense_graph: " ++
                      S ++ "~n",
                  Args)).

-define(MAX_DIST, 9999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
    Route = search(get_test_data(1)),
    show_routes(Route),
    Route2 = search(get_test_data(2)),
    show_routes(Route2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search(Nodes) ->
    Nodes1 = set_dist(Nodes, start, 0),
    calc_shortest_path(Nodes1).

calc_shortest_path(Nodes) ->
    TargetNode = get_non_visit_min_vertex(Nodes),
    case TargetNode of
        null -> Nodes;
        _ -> calc_shortest_path(Nodes, TargetNode)
    end.

calc_shortest_path(Nodes, TargetNode) ->
    case TargetNode#vertex_record.dist of
        ?MAX_DIST -> Nodes;
        _ ->
            Nodes1 = set_visit(Nodes,
                               TargetNode#vertex_record.vertex,
                               true),
            ResultNodes = calc_shortest_path(Nodes1,
                                             TargetNode,
                                             TargetNode#vertex_record.edges),
            calc_shortest_path(ResultNodes)
    end.

calc_shortest_path(Nodes, _, []) -> Nodes;
calc_shortest_path(Nodes, TargetNode, Edges) ->
    [Edge | Retain] = Edges,
    Weight = get_weight(Nodes,
                        TargetNode#vertex_record.vertex,
                        Edge#edge_record.vertex),
    Length = TargetNode#vertex_record.dist + Weight,
    EdgeDist = get_dist(Nodes, Edge#edge_record.vertex),
    Nodes1 = case Length < EdgeDist of
                 true ->
                     SetDistNodes = set_dist(Nodes,
                                             Edge#edge_record.vertex,
                                             Length),
                     SetPeriodNodes = set_period(SetDistNodes,
                                                 Edge#edge_record.vertex,
                                                 TargetNode#vertex_record.vertex),
                     SetPeriodNodes;
                 _ -> Nodes
             end,
    calc_shortest_path(Nodes1, TargetNode, Retain).

get_non_visit_min_vertex(Nodes) ->
    NonVisitNodes = get_non_vist_nodes(Nodes),
    get_min_dist_node(NonVisitNodes).

get_non_vist_nodes([]) -> [];
get_non_vist_nodes(Nodes) ->
    [Node | Retain] = Nodes,
    NonVisitNodes = case Node#vertex_record.visit of
                        true -> [];
                        false -> [Node]
                    end,
    NonVisitNodes ++ get_non_vist_nodes(Retain).

get_min_dist_node([]) -> null;
get_min_dist_node(Nodes) ->
    [Node | _] = Nodes,
    get_min_dist_node(Nodes, Node).

get_min_dist_node(Nodes, Node) ->
    Lowers = [#vertex_record{vertex = Vertex, edges = Edges,
                             visit = Visit, period = Period, dist = Dist}
              || #vertex_record{vertex = Vertex, edges = Edges,
                                visit = Visit, period = Period, dist = Dist}
                     <- Nodes,
                 Dist < Node#vertex_record.dist],
    case length(Lowers) of
        0 -> Node;
        _ ->
            [Lower | _] = Lowers,
            get_min_dist_node(Nodes, Lower)
    end.

get_weight(Nodes, TargetVertex, EdgeVertex) ->
    Node = lists:keyfind(TargetVertex,
                         #vertex_record.vertex,
                         Nodes),
    Edge = lists:keyfind(EdgeVertex,
                         #edge_record.vertex,
                         Node#vertex_record.edges),
    Edge#edge_record.weight.

set_visit(Nodes, Vertex, Visit) ->
    Node = lists:keyfind(Vertex,
                         #vertex_record.vertex,
                         Nodes),
    [Node#vertex_record{visit = Visit}
     | lists:keydelete(Vertex,
                       #vertex_record.vertex,
                       Nodes)].

get_dist(Nodes, Vertex) ->
    Node = lists:keyfind(Vertex,
                         #vertex_record.vertex,
                         Nodes),
    Node#vertex_record.dist.

set_dist(Nodes, Vertex, Dist) ->
    Node = lists:keyfind(Vertex,
                         #vertex_record.vertex,
                         Nodes),
    [Node#vertex_record{dist = Dist}
     | lists:keydelete(Vertex,
                       #vertex_record.vertex,
                       Nodes)].

set_period(Nodes, Vertex, Period) ->
    Node = lists:keyfind(Vertex,
                         #vertex_record.vertex,
                         Nodes),
    [Node#vertex_record{period = Period}
     | lists:keydelete(Vertex,
                       #vertex_record.vertex,
                       Nodes)].

get_test_data(1) ->
    [#vertex_record{vertex = start,
                    edges =
                        [#edge_record{vertex = 1, weight = 2},
                         #edge_record{vertex = stop, weight = 4}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 1,
                    edges = [#edge_record{vertex = 2, weight = 3}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 2,
                    edges =
                        [#edge_record{vertex = 3, weight = 5},
                         #edge_record{vertex = stop, weight = 1}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 3,
                    edges =
                        [#edge_record{vertex = start, weight = 8},
                         #edge_record{vertex = stop, weight = 7}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = stop,
                    edges = [#edge_record{vertex = 3, weight = 7}],
                    visit = false, period = null, dist = ?MAX_DIST}];
get_test_data(2) ->
    [#vertex_record{vertex = start,
                    edges =
                        [#edge_record{vertex = 1, weight = 6},
                         #edge_record{vertex = 2, weight = 8},
                         #edge_record{vertex = stop, weight = 18}],
                    visit = false, period = null, dist = 0},
     #vertex_record{vertex = 1,
                    edges = [#edge_record{vertex = 4, weight = 11}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 2,
                    edges = [#edge_record{vertex = stop, weight = 9}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = stop, edges = [], visit = false,
                    period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 4,
                    edges = [#edge_record{vertex = 5, weight = 3}],
                    visit = false, period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 5,
                    edges =
                        [#edge_record{vertex = stop, weight = 4},
                         #edge_record{vertex = 2, weight = 7}],
                    visit = false, period = null, dist = ?MAX_DIST}].

show_routes(Nodes) ->
    show_vertex_record(Nodes),
    Route = build_routes(Nodes, stop, []),
    ?OUTPUT_INFO("show_routes/1 - route: ~w", [Route]).

build_routes(_, start, Dist) -> [start | Dist];
build_routes(Nodes, TargetVertex, Dist) ->
    Dist1 = [TargetVertex | Dist],
    TargetNode = lists:keyfind(TargetVertex,
                               #vertex_record.vertex,
                               Nodes),
    build_routes(Nodes,
                 TargetNode#vertex_record.period,
                 Dist1).

show_vertex_record([]) -> ok;
show_vertex_record(Nodes) ->
    [Node | Retain] = Nodes,
    ?OUTPUT_DEBUG("show_vertex_record: vertex: ~w, period: "
                  "~w, dist: ~w",
                  [Node#vertex_record.vertex,
                   Node#vertex_record.period,
                   Node#vertex_record.dist]),
    show_vertex_record(Retain).
