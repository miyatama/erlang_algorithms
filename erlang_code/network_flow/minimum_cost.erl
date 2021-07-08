-module(minimum_cost).

-export([test/0]).

-import(ford_fullkerson, 
  [process_argpath/2,
    equal_edges/2,
    find_edge/3,
    add_argumenting_path/4]).

-import(augmenting_path,
  [generate_argumenting_path/1]).

-include("network_flow.hrl").

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
  case (?LOG_LEVEL) =< 1 of
    true -> io:fwrite("[DEBUG] minimum_cost: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] minimum_cost: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] minimum_cost: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] minimum_cost: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] minimum_cost: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] minimum_cost: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(MAX_DELTA, 9999999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  test(1),
  test(2),
  test(3),
  test(4),
  test(5),
  test(6),
  test(7),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(TestCase) ->
  Graph = generate_initial_graph(TestCase),
  ResultGraph = compute(Graph),
  ?OUTPUT_INFO("test ~w", [TestCase]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test function
generate_initial_graph(1) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10, cost=1},
   #edge{from=v1, to=sink, flow=0, capacity=10, cost=1}
  ];

generate_initial_graph(2) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=5, cost=1},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge{from=v1, to=sink, flow=0, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=0, capacity=5, cost=1}
  ];

generate_initial_graph(3) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10, cost=30},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=11},
   #edge{from=v1, to=sink, flow=0, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=0, capacity=3, cost=1}
  ];

generate_initial_graph(4) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=10, cost=5},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=1},
   #edge{from=source, to=v3, flow=0, capacity=4, cost=1},
   #edge{from=v1, to=v4, flow=0, capacity=5, cost=5},
   #edge{from=v1, to=v2, flow=0, capacity=10, cost=5},
   #edge{from=v2, to=v4, flow=0, capacity=2, cost=1},
   #edge{from=v2, to=v3, flow=0, capacity=10, cost=1},
   #edge{from=v3, to=v5, flow=0, capacity=11, cost=1},
   #edge{from=v4, to=sink, flow=0, capacity=7, cost=1},
   #edge{from=v5, to=sink, flow=0, capacity=11, cost=1}
  ];

generate_initial_graph(5) ->
  [
   #edge{from=source, to=v1, flow=0, capacity=5, cost=4},
   #edge{from=source, to=v2, flow=0, capacity=5, cost=2},
   #edge{from=source, to=v3, flow=0, capacity=5, cost=1},
   #edge{from=v1, to=v4, flow=0, capacity=5, cost=3},
   #edge{from=v2, to=v4, flow=0, capacity=5, cost=2},
   #edge{from=v3, to=v4, flow=0, capacity=5, cost=1},
   #edge{from=v4, to=sink, flow=0, capacity=11, cost=1}
  ];

generate_initial_graph(6) ->
  [
    #edge{from=source, to=s1, flow=0, capacity=60, cost=0},
    #edge{from=source, to=s2, flow=0, capacity=20, cost=0},
    #edge{from=source, to=s3, flow=0, capacity=60, cost=0},

    #edge{from=s1, to=t1, flow=0, capacity=9999, cost=500},
    #edge{from=s1, to=t2, flow=0, capacity=9999, cost=300},
    #edge{from=s1, to=t3, flow=0, capacity=9999, cost=250},
   
    #edge{from=s2, to=t1, flow=0, capacity=9999, cost=100},
    #edge{from=s2, to=t2, flow=0, capacity=9999, cost=200},
    #edge{from=s2, to=t3, flow=0, capacity=9999, cost=200},

    #edge{from=s3, to=t1, flow=0, capacity=9999, cost=300},
    #edge{from=s3, to=t2, flow=0, capacity=9999, cost=210},
    #edge{from=s3, to=t3, flow=0, capacity=9999, cost=230},

    #edge{from=t1, to=sink, flow=0, capacity=100, cost=0},
    #edge{from=t2, to=sink, flow=0, capacity=30, cost=0},
    #edge{from=t3, to=sink, flow=0, capacity=10, cost=0}
  ];

generate_initial_graph(7) ->
  [
    #edge{from=source, to=s1, flow=0, capacity=60, cost=0},
    #edge{from=source, to=s2, flow=0, capacity=60, cost=0},

    #edge{from=s1, to=t1, flow=0, capacity=9999, cost=500},
    #edge{from=s1, to=t2, flow=0, capacity=9999, cost=300},
    #edge{from=s1, to=t3, flow=0, capacity=9999, cost=250},
   
    #edge{from=s2, to=t1, flow=0, capacity=9999, cost=300},
    #edge{from=s2, to=t2, flow=0, capacity=9999, cost=210},
    #edge{from=s2, to=t3, flow=0, capacity=9999, cost=230},

    #edge{from=t1, to=sink, flow=0, capacity=80, cost=0},
    #edge{from=t2, to=sink, flow=0, capacity=30, cost=0},
    #edge{from=t3, to=sink, flow=0, capacity=10, cost=0}
  ];

generate_initial_graph(_) ->
  [].

generate_expect_graph(1) ->
  [
   #edge{from=source, to=v1, flow=10, capacity=10, cost=1},
   #edge{from=v1, to=sink, flow=10, capacity=10, cost=1}
  ];

generate_expect_graph(2) ->
  [
   #edge{from=source, to=v1, flow=5, capacity=5, cost=1},
   #edge{from=source, to=v2, flow=5, capacity=5, cost=1},
   #edge{from=v1, to=sink, flow=10, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=5, capacity=5, cost=1}
  ];

generate_expect_graph(3) ->
  [
   #edge{from=source, to=v1, flow=7, capacity=10, cost=30},
   #edge{from=source, to=v2, flow=3, capacity=5, cost=11},
   #edge{from=v1, to=sink, flow=10, capacity=10, cost=1},
   #edge{from=v2, to=v1, flow=3, capacity=3, cost=1}
  ];

generate_expect_graph(4) ->
  [
   #edge{from=source, to=v1, flow=9, capacity=10, cost=5},
   #edge{from=source, to=v2, flow=5, capacity=5, cost=1},
   #edge{from=source, to=v3, flow=4, capacity=4, cost=1},
   #edge{from=v1, to=v4, flow=5, capacity=5, cost=5},
   #edge{from=v1, to=v2, flow=4, capacity=10, cost=5},
   #edge{from=v2, to=v4, flow=2, capacity=2, cost=1},
   #edge{from=v2, to=v3, flow=7, capacity=10, cost=1},
   #edge{from=v3, to=v5, flow=11, capacity=11, cost=1},
   #edge{from=v4, to=sink, flow=7, capacity=7, cost=1},
   #edge{from=v5, to=sink, flow=11, capacity=11, cost=1}
  ];

generate_expect_graph(5) ->
  [
   #edge{from=source, to=v1, flow=1, capacity=5, cost=4},
   #edge{from=source, to=v2, flow=5, capacity=5, cost=2},
   #edge{from=source, to=v3, flow=5, capacity=5, cost=1},
   #edge{from=v1, to=v4, flow=1, capacity=5, cost=3},
   #edge{from=v2, to=v4, flow=5, capacity=5, cost=2},
   #edge{from=v3, to=v4, flow=5, capacity=5, cost=1},
   #edge{from=v4, to=sink, flow=11, capacity=11, cost=1}
  ];

generate_expect_graph(6) ->
  [
    #edge{from=s1, to=t1, flow=20, capacity=9999, cost=500},
    #edge{from=s1, to=t2, flow=30, capacity=9999, cost=300},
    #edge{from=s1, to=t3, flow=10, capacity=9999, cost=250},
    #edge{from=s2, to=t1, flow=20, capacity=9999, cost=100},
    #edge{from=s2, to=t2, flow=0, capacity=9999, cost=200},
    #edge{from=s2, to=t3, flow=0, capacity=9999, cost=200},
    #edge{from=s3, to=t1, flow=60, capacity=9999, cost=300},
    #edge{from=s3, to=t2, flow=0, capacity=9999, cost=210},
    #edge{from=s3, to=t3, flow=0, capacity=9999, cost=230},
    #edge{from=source, to=s1, flow=60, capacity=60, cost=0},
    #edge{from=source, to=s2, flow=20, capacity=20, cost=0},
    #edge{from=source, to=s3, flow=60, capacity=60, cost=0},
    #edge{from=t1, to=sink, flow=100, capacity=100, cost=0},
    #edge{from=t2, to=sink, flow=30, capacity=30, cost=0},
    #edge{from=t3, to=sink, flow=10, capacity=10, cost=0}
  ];

generate_expect_graph(7) ->
  [
    #edge{from=source, to=s1, flow=60, capacity=60, cost=0},
    #edge{from=source, to=s2, flow=60, capacity=60, cost=0},
    #edge{from=s1, to=t1, flow=20, capacity=9999, cost=500},
    #edge{from=s1, to=t2, flow=30, capacity=9999, cost=300},
    #edge{from=s1, to=t3, flow=10, capacity=9999, cost=250},
    #edge{from=s2, to=t1, flow=60, capacity=9999, cost=300},
    #edge{from=s2, to=t2, flow=0, capacity=9999, cost=210},
    #edge{from=s2, to=t3, flow=0, capacity=9999, cost=230},
    #edge{from=t1, to=sink, flow=80, capacity=80, cost=0},
    #edge{from=t2, to=sink, flow=30, capacity=30, cost=0},
    #edge{from=t3, to=sink, flow=10, capacity=10, cost=0}
  ];

generate_expect_graph(_) -> [].

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
