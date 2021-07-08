-module(allocation).

-export([test/0]).

-import(ford_fullkerson, 
  [process_argpath/2,
    equal_edges/2,
    find_edge/3,
    add_argumenting_path/4]).

-import(augmenting_path,
  [generate_argumenting_path/1]).

-include("network_flow.hrl").

-record(supplyer, {no, supply_unit_count, allocation_cost_par_warehouse, costs_for_demand}).

-record(demand, {no, demand_unit_count}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
  case (?LOG_LEVEL) =< 1 of
    true -> io:fwrite("[DEBUG] allocation: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] allocation: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] allocation: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] allocation: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] allocation: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] allocation: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(MAX_CAPACITY, 9999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  generate_edges_test(),
  test(1),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(TestCase) ->
  Graph = generate_graph(TestCase),
  ResultGraph = compute(Graph),
  ?OUTPUT_INFO(
    "test ~w",
    [TestCase]),
  ExpectGraph = generate_expect_graph(TestCase),
  show_result("test case ~w: ~w", TestCase, ExpectGraph, ResultGraph).

-spec compute(list(edge)) -> list(edge).
compute(Graph) -> 
  show_edges(Graph),
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

-spec generate_graph(integer()) -> list(edge).
generate_graph(TestCase) ->
    Supplyers = generate_supplyers(TestCase),
    Demands = generate_demands(TestCase),
    generate_edges(Supplyers, Demands).

-spec generate_supplyers(integer()) -> list(supplyer).
generate_supplyers(1) ->
    [
        #supplyer{
            no=1,
            supply_unit_count=60, 
            costs_for_demand=[
              {1, 500},
              {2, 300},
              {3, 250}]},
        #supplyer{
            no=2,
            supply_unit_count=20, 
            costs_for_demand=[
              {1, 100},
              {2, 200},
              {3, 200}]},
        #supplyer{
            no=3,
            supply_unit_count=60, 
            costs_for_demand=[
              {1, 300},
              {2, 210},
              {3, 230}]}
    ].

-spec generate_demands(integer()) -> list(demand).
generate_demands(1) ->
    [
        #demand{no=1, demand_unit_count=100},
        #demand{no=2, demand_unit_count=30},
        #demand{no=3, demand_unit_count=10}
    ].

-spec generate_edges(
    list(supplyer),
    list(demand)) -> list(edge).
generate_edges(Supplyers, Demands) ->
    generate_supplyer_edges(Supplyers, Demands)
     ++ generate_demands_edges(Demands).

-spec generate_supplyer_edges(
  list(suppliyer),
  list(demand)) -> list(edge).
generate_supplyer_edges(Supplyers, Demands) ->
    SourceEdges = generate_source_to_supplyer_edges(Supplyers),
    AddDemandEdges = SourceEdges ++
        generate_supplyer_to_demand_edges(Supplyers, Demands),
    AddDemandEdges.

generate_source_to_supplyer_edges([]) -> [];
generate_source_to_supplyer_edges(Supplyers) ->
  [Supplyer | Retain]  = Supplyers,
  [create_edge(
      source,
      generate_supplyer_vertex_name(Supplyer),
      0,
      1)] ++
    generate_source_to_supplyer_edges(Retain).

generate_supplyer_to_demand_edges([], _) -> [];
generate_supplyer_to_demand_edges(Supplyers, Demands) ->
    [Supplyer | SupplyersRetain] = Supplyers,
    generate_supplyer_to_demand_edges(Supplyers, Demands, Supplyer) ++
      generate_supplyer_to_demand_edges(SupplyersRetain, Demands).
generate_supplyer_to_demand_edges(_, [], _) -> [];
generate_supplyer_to_demand_edges(Supplyers, Demands, Supplyer) ->
    [Demand | DemandsRetain] = Demands,
    [create_edge(
        generate_supplyer_vertex_name(Supplyer),
        generate_demand_vertex_name(Demand),
        get_supplyer_cost_for_demand(Supplyer, Demand),
        ?MAX_CAPACITY)] ++
        generate_supplyer_to_demand_edges(Supplyers, DemandsRetain, Supplyer).

-spec generate_demands_edges(list(demand)) -> list(edge).
generate_demands_edges([]) -> [];
generate_demands_edges(Demands) ->
    [Demand | DemandsRetain] = Demands,
    [create_edge(
        generate_demand_vertex_name(Demand), 
        sink,
        0,
        1)] ++
    generate_demands_edges(DemandsRetain).

generate_supplyer_vertex_name(Supplyer) ->
    list_to_atom(
          lists:flatten(io_lib:format("supplyer_~B", [Supplyer#supplyer.no]))).

generate_demand_vertex_name(Demand) ->
    list_to_atom(
          lists:flatten(io_lib:format("demand_~B", [Demand#demand.no]))).

create_edge(FromVertex, ToVertex, Cost, Capacity) ->
    #edge{
        from=FromVertex,
        to=ToVertex,
        cost=Cost,
        flow=0,
        capacity=Capacity}.

get_supplyer_cost_for_demand(Supplyer, Demand) ->
    get_supplyer_cost_for_demand(
        Supplyer, 
        Supplyer#supplyer.costs_for_demand, 
        Demand).
get_supplyer_cost_for_demand(_, [], _) -> 0;
get_supplyer_cost_for_demand(Supplyer, Costs, Demand) ->
    [Cost | CostsRetain] = Costs,
    DemandNo = Demand#demand.no,
    case Cost of
        {DemandNo, Value} ->
            Value;
        _ ->
            get_supplyer_cost_for_demand(Supplyer, CostsRetain, Demand)
    end.

show_result(Text, CaseNo ,Expect, Result) ->
  case equal_edges(Expect, Result) of
    true ->
      ?OUTPUT_INFO(Text, [CaseNo, true]);
    false ->
      ?OUTPUT_ERROR(Text, [CaseNo, false]),
      ?OUTPUT_ERROR("Expect:"),
      show_edges(false, Expect),
      ?OUTPUT_ERROR("Result"),
      show_edges(false, Result)
  end.

show_edges(Edges) -> show_edges(true, Edges).
show_edges(_, []) -> ok;
show_edges(Debug, Edges) -> 
  [Edge | Retain] = Edges,
  show_edge(Debug, Edge),
  show_edges(Debug, Retain).
show_edge(Debug, Edge) ->
  #edge{
    from=FromVertex,
    to=ToVertex,
    flow=Flow,
    capacity=Capacity,
    cost=Cost} = Edge,
  Text = "~w to ~w: ~w/~w, cost is ~w(par unit ~w)",
  Param = [
     FromVertex,
     ToVertex,
     Flow,
     Capacity,
     Cost * Flow,
     Cost
    ],
  case Debug of
    true ->
      ?OUTPUT_DEBUG(Text, Param);
    false -> 
      ?OUTPUT_INFO(Text, Param)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test functions
generate_expect_graph(1) -> [
    #edge{from=source, to=supplyer_1, flow=1, capacity=1, cost=0},
    #edge{from=source, to=supplyer_2, flow=1, capacity=1, cost=0},
    #edge{from=source, to=supplyer_3, flow=1, capacity=1, cost=0},
    #edge{from=supplyer_1, to=demand_1, flow=0, capacity=9999, cost=500},
    #edge{from=supplyer_1, to=demand_2, flow=0, capacity=9999, cost=300},
    #edge{from=supplyer_1, to=demand_3, flow=1, capacity=9999, cost=250},
    #edge{from=supplyer_2, to=demand_1, flow=1, capacity=9999, cost=100},
    #edge{from=supplyer_2, to=demand_2, flow=0, capacity=9999, cost=200},
    #edge{from=supplyer_2, to=demand_3, flow=0, capacity=9999, cost=200},
    #edge{from=supplyer_3, to=demand_1, flow=0, capacity=9999, cost=300},
    #edge{from=supplyer_3, to=demand_2, flow=1, capacity=9999, cost=210},
    #edge{from=supplyer_3, to=demand_3, flow=0, capacity=9999, cost=230},
    #edge{from=demand_1, to=sink, flow=1, capacity=1, cost=0},
    #edge{from=demand_2, to=sink, flow=1, capacity=1, cost=0},
    #edge{from=demand_3, to=sink, flow=1, capacity=1, cost=0}
  ];

generate_expect_graph(_) -> [].

generate_edges_test() ->
    % case 001: suplyer, warehouse, demand.1:1:1
    Supplyers001 = [
        #supplyer{
            no=1,
            supply_unit_count = 10,
            costs_for_demand=[
                {1, 11}]}],
    Demands001 = [
        #demand{no=1, demand_unit_count=1000}],
    Edges001 = generate_edges(Supplyers001, Demands001),
    Expect001 = [
        #edge{from=source, to=supplyer_1, flow=0, cost=0, capacity=1},
        #edge{from=supplyer_1, to=demand_1, flow=0, cost=11, capacity=?MAX_CAPACITY},
        #edge{from=demand_1, to=sink, flow=0, cost=0, capacity=1}],
    show_generate_edges_test(
        "case001 - supplyer, warehouse, demand. 1:1:1. : ~w", 
        Expect001,
        Edges001),
    % case 002: suplyer, warehouse, demand.2:1:1
    Supplyers002 = [
        #supplyer{
            no=1,
            supply_unit_count = 10,
            costs_for_demand=[
                {1, 11}]},
        #supplyer{
            no=2,
            supply_unit_count = 15,
            costs_for_demand=[
                {1, 15}]}],
    Demands002 = [
        #demand{no=1, demand_unit_count=1000}],
    Edges002 = generate_edges(Supplyers002, Demands002),
    Expect002 = [
        #edge{from=source, to=supplyer_1, flow=0, cost=0, capacity=1},
        #edge{from=source, to=supplyer_2, flow=0, cost=0, capacity=1},
        #edge{from=supplyer_1, to=demand_1, flow=0, cost=11, capacity=?MAX_CAPACITY},
        #edge{from=supplyer_2, to=demand_1, flow=0, cost=15, capacity=?MAX_CAPACITY},
        #edge{from=demand_1, to=sink, flow=0, cost=0, capacity=1}],
    show_generate_edges_test(
        "case002 - supplyer, warehouse, demand. 2:1:1. : ~w", 
        Expect002,
        Edges002),
    ok.

show_generate_edges_test(CaseText, Expect, Result) ->
    Equals = equal_edges(Expect, Result),
    case Equals of
        true ->
            ?OUTPUT_INFO(CaseText, [Equals]);
        _ ->
            ?OUTPUT_ERROR(CaseText, [Equals]),
            ?OUTPUT_DEBUG("expect: ~w, result: ~w", [Expect, Result])
    end.
