-module(bellman_ford).

-export([test/0]).

% dist is priority
% rf(vertex_record).
% rd(vertex_record, {vertex,edges,period,dist}).
-record(vertex_record, {vertex, edges, period, dist}).

% rf(edge_record).
% rd(edge_record, {from_vertex, to_vertex ,weight}).
-record(edge_record, {from_vertex, to_vertex, weight}).

-define(OUTPUT_DEBUG(S),
        io:fwrite("[DEBUG] bellman_ford: " ++ S ++ "~n")).

-define(OUTPUT_DEBUG(S, Args),
        io:fwrite("[DEBUG] bellman_ford: " ++ S ++ "~n", Args)).

-define(OUTPUT_INFO(S),
        io:fwrite("[INFO] bellman_ford: " ++ S ++ "~n")).

-define(OUTPUT_INFO(S, Args),
        io:fwrite("[INFO] bellman_ford: " ++ S ++ "~n", Args)).

-define(MAX_DIST, 9999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
    Route = search(get_test_data(1)),
    show_routes(Route),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(list()) -> list().

search([]) -> [];
search(Nodes) ->
    Nodes1 = set_dist(Nodes, start, 1),
    search(Nodes1, 1).

-spec search(list(), integer()) -> list().

search(Nodes, Count) when Count > length(Nodes) ->
    Nodes;
search(Nodes, Count) ->
    {Nodes1, Result} = single_source_shortest(Nodes,
                                              get_edge_records(Nodes),
                                              Count),
    case Result of
        true -> search(Nodes1, Count + 1);
        false -> []
    end.

-spec single_source_shortest(list(), list(),
                             integer()) -> {list(), true | false}.

single_source_shortest(Nodes, [], _) -> {Nodes, true};
single_source_shortest(Nodes, Edges, Count) ->
    [Edge | EdgeRetain] = Edges,
    ToDist = get_dist(Nodes, Edge#edge_record.to_vertex),
    NewCost = get_dist(Nodes, Edge#edge_record.from_vertex)
                  + Edge#edge_record.weight,
    {ChangedDistNodes, NegativeLoopBreak} =
        update_node_cost(Nodes, Edge, ToDist, NewCost, Count),
    ?OUTPUT_DEBUG("single_source_shortest/3 - nggative "
                  "loop: ~w",
                  [NegativeLoopBreak]),
    case NegativeLoopBreak of
        true -> {[], false};
        false ->
            single_source_shortest(ChangedDistNodes,
                                   EdgeRetain,
                                   Count)
    end.

-spec update_node_cost(list(), list(), integer(),
                       integer(), integer()) -> {list(), true | false}.

update_node_cost(Nodes, Edge, Cost, NewCost,
                 VertexCount)
    when Cost > NewCost ->
    ?OUTPUT_DEBUG("update_node_cost/5 - vertex: ~w, new "
                  "cost: ~w, vertex count: ~w",
                  [Edge#edge_record.to_vertex, NewCost, VertexCount]),
    SetDistNodes = set_dist(Nodes,
                            Edge#edge_record.to_vertex,
                            NewCost),
    SetPeriodNodes = set_period(SetDistNodes,
                                Edge#edge_record.to_vertex,
                                Edge#edge_record.from_vertex),
    NegativeLoopBreak = VertexCount >= length(Nodes),
    {SetPeriodNodes, NegativeLoopBreak};
update_node_cost(Nodes, _, _, _, _) -> {Nodes, false}.

get_edge_records([]) -> [];
get_edge_records(Nodes) ->
    [Node | Retain] = Nodes,
    Node#vertex_record.edges ++ get_edge_records(Retain).

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

show_routes([]) ->
    ?OUTPUT_INFO("show_routes/1 - route: ~w", [nothing]);
show_routes(Nodes) ->
    ?OUTPUT_DEBUG("show_routes/1 - nodes: ~w", [Nodes]),
    show_vertex_record(Nodes),
    Route = build_routes(Nodes, stop, []),
    ?OUTPUT_INFO("show_routes/1 - route: ~w", [Route]).

build_routes(_, start, Dist) -> [start | Dist];
build_routes(Nodes, TargetVertex, Dist) ->
    Dist1 = [TargetVertex | Dist],
    TargetNode = lists:keyfind(TargetVertex,
                               #vertex_record.vertex,
                               Nodes),
    ?OUTPUT_DEBUG("build_routes/3 node: ~w", [TargetNode]),
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

get_test_data(1) ->
    [#vertex_record{vertex = start,
                    edges =
                        [#edge_record{from_vertex = start, to_vertex = 4,
                                      weight = 2}],
                    period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 4,
                    edges =
                        [#edge_record{from_vertex = 4, to_vertex = 1,
                                      weight = 5},
                         #edge_record{from_vertex = 4, to_vertex = 3,
                                      weight = 4}],
                    period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 1,
                    edges =
                        [#edge_record{from_vertex = 1, to_vertex = 3,
                                      weight = -2}],
                    period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = 3,
                    edges =
                        [#edge_record{from_vertex = 3, to_vertex = stop,
                                      weight = 6}],
                    period = null, dist = ?MAX_DIST},
     #vertex_record{vertex = stop,
                    edges =
                        [#edge_record{from_vertex = stop, to_vertex = 1,
                                      weight = -3}],
                    period = null, dist = ?MAX_DIST}].
