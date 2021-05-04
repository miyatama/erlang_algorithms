-module(brute_force).

-export([test/0]).

-record(vertex_record, {vertex, edges}).

-define(OUTPUT_DEBUG(S), io:fwrite("[DEBUG] brute_force: " ++ S ++ "~n")).
-define(OUTPUT_DEBUG(S, Args), io:fwrite("[DEBUG] brute_force: " ++ S ++ "~n", Args)).
-define(OUTPUT_INFO(S), io:fwrite("[INFO] brute_force: " ++ S ++ "~n")).
-define(OUTPUT_INFO(S, Args), io:fwrite("[INFO] brute_force: " ++ S ++ "~n", Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec test() -> ok.
test() -> 
  search(get_model_data(1)),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(list(T)) -> list(T).
search(L) ->
  % [[start, 1, 8, ..], [start, 2, 8, ...]]
  Routes = dfs_first_visit(start, L),
  FilteredRoutes = filter_route(Routes),
  show_routes(FilteredRoutes),
  ok.

dfs_first_visit(Route, L) ->
  Edges = get_vertex_edges(Route, L),
  dfs_first_visit(Route, L, Edges, []).
dfs_first_visit(_, _, [], Dist) -> Dist;
dfs_first_visit(Route, L, Edges, Dist) when is_list(Edges) ->
  [H|T] = Edges,
  % [[start, 1, 8, ...], [start, 1, 6, ...]]
  ResultRoute = dfs_visit([Route], H, L),
  Dist1 = lists:append(Dist, ResultRoute),
  % [[start, 1, 8, ...], [start, 1, 6, ...], [start, 2, 3, ...]]
  dfs_first_visit(Route, L, T, Dist1).

-spec dfs_visit(list(), integer() | stop | start, list()) -> list().
dfs_visit(Route, _, L) when length(Route) > length(L) -> Route;
dfs_visit(Route, Vertex, L) ->
  Route1 = lists:append(Route, [Vertex]),
  Edges = get_vertex_edges(Vertex, L),
  ExcludeVertexEdges = eliminate_vertex(Edges, Route),
  dfs_visit(Route1, ExcludeVertexEdges, L, []).

-spec dfs_visit(list(), list(), list(), list()) -> list().
dfs_visit(Route, [], _, Dist) -> 
  [Route | Dist];
dfs_visit(Route, Edges, L, Dist) ->
  [H|T] = Edges,
  ResultRoute = dfs_visit(Route, H, L),
  Dist1 = lists:append(Dist, ResultRoute),
  dfs_visit(Route, T, L, Dist1).

get_vertex_edges(Target, L) ->
  VertexEdges = [ Edges || #vertex_record{vertex=Vertex, edges=Edges} <- L, Vertex == Target],
  [H|_] = VertexEdges,
  H.

eliminate_vertex(Edges, Vertexes) ->
  lists:subtract(Edges, Vertexes).

filter_route(Routes) -> 
  filter_route(Routes, []).
filter_route([], Dist) -> Dist;
filter_route(Routes, Dist) -> 
  [Route|T] = Routes,
  Dist1 = case lists:nth(length(Route), Route) of
    stop -> lists:append(Dist, [Route]);
    _ -> Dist
  end,
  filter_route(T, Dist1).

show_routes([]) -> 
  ?OUTPUT_INFO("finish"),
  ok;
show_routes(Routes) ->
  [H|T] = Routes,
  ?OUTPUT_INFO("Route: ~w", [H]),
  show_routes(T).


get_model_data(1) ->
  [
    #vertex_record{vertex=start, edges=[1, 6, 8]},
    #vertex_record{vertex=1,     edges=[start, 2, 3]},
    #vertex_record{vertex=6,     edges=[start, 5, 7]},
    #vertex_record{vertex=8,     edges=[start, 7, 14]},
    #vertex_record{vertex=2,     edges=[1, 11, 10]},
    #vertex_record{vertex=3,     edges=[1, 4, 12]},
    #vertex_record{vertex=5,     edges=[4, 6, 9]},
    #vertex_record{vertex=4,     edges=[3, 5, 13]},
    #vertex_record{vertex=7,     edges=[6, 8, 9]},
    #vertex_record{vertex=9,     edges=[5, 7, stop]},
    #vertex_record{vertex=10,    edges=[2]},
    #vertex_record{vertex=11,    edges=[2]},
    #vertex_record{vertex=12,    edges=[3]},
    #vertex_record{vertex=13,    edges=[4]},
    #vertex_record{vertex=14,    edges=[8]},
    #vertex_record{vertex=stop,  edges=[]}
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
