-module(depth_first_search).

-export([test/0]).

% 1 - depth -> integer()
% thinc ply depth.
% 2 - move -> {X:Integer(), Y:Integer()} | null
% old null position.oldstate = exchange(states, move).
% 3 - states: list(state_record)
% states after moved. 
% rf(game_state_record).
% rd(game_state_record, {depth, move, states}).
-record(game_state_record, {depth, move, states}).

% position -> {y: integer(), y: integer()}, value -> integer()
% rf(state_record).
% rd(state_record, {position, value}).
-record(state_record, {position, value}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 1).

-define(OUTPUT_DEBUG(S),
        case (?LOG_LEVEL) =< 1 of
            true -> io:fwrite("[DEBUG] depth_first_search: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_DEBUG(S, Args),
        case (?LOG_LEVEL) =< 1 of
            true ->
                io:fwrite("[DEBUG] depth_first_search: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_INFO(S),
        case (?LOG_LEVEL) =< 2 of
            true -> io:fwrite("[INFO] depth_first_search: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_INFO(S, Args),
        case (?LOG_LEVEL) =< 2 of
            true ->
                io:fwrite("[INFO] depth_first_search: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_ERROR(S),
        case (?LOG_LEVEL) =< 3 of
            true -> io:fwrite("[ERROR] depth_first_search: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_ERROR(S, Args),
        case (?LOG_LEVEL) =< 3 of
            true ->
                io:fwrite("[ERROR] depth_first_search: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(MAX_DEPTH, 99).


% new stack
% Q = queue:new().
% push 
% Q1 = queue:in(1, Q).
% pop
% Value = queue:last(Q3).
% Q4 = queue:liat(Q3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  test(1),
  ok.

test(CaseNo) ->
  GameState = generate_test_game_state(CaseNo),
  Result = search(GameState),
  show_search_result(CaseNo, Result),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search(GameState) -> 
  GameStateStack = queue:new(),
  InitialGameStateStack = queue:in(GameState, GameStateStack),
  ClosedList = [],
  search(InitialGameStateStack, ClosedList).

% GameStateStack -> game state queue.
% ClosedList -> already scene game state
% PlyDepth -> think depth
-spec search(list(game_state_record), list(game_state_record)) -> {solution | no_solution, list(game_state_record)}.
search(GameStateStack, ClosedList) ->
  StackSize = queue:len(GameStateStack),
  case StackSize of
    0 -> 
      {no_solution, ClosedList};
    _ ->
      GameState = queue:last(GameStateStack),
      PopedGameStateStack = queue:liat(GameStateStack),
      case GameState#game_state_record.move of
        null ->
          ?OUTPUT_DEBUG("move is ~w", [null]),
          show_state(GameState#game_state_record.states);
        _ -> ok
      end,
      UpdatedClosedList = ClosedList ++ [GameState],
      Moves = get_valid_move(GameState#game_state_record.states),
      search(GameState, PopedGameStateStack, UpdatedClosedList, Moves)
  end.

search(_, GameStateStack, ClosedList, []) -> 
  search(GameStateStack, ClosedList);
search(GameState, GameStateStack, ClosedList, Moves) ->
  [Move|MovesRetain] = Moves,
  NullPosition = get_null_value_state_record(GameState#game_state_record.states),
  {_, MovedStates} = move_position(GameState#game_state_record.states, Move),
  MovedGameStates = #game_state_record{
    depth=GameState#game_state_record.depth + 1,
    move=NullPosition#state_record.position, 
    states=MovedStates
  },
  ExistsClosed = exists_states(MovedStates, ClosedList),
  IsGoal = is_goal_state(MovedStates),
  InRangeDepth = MovedGameStates#game_state_record.depth < ?MAX_DEPTH,
  {Result, UpdatedGameStackState, UpdatedClosedList} = case {ExistsClosed, IsGoal, InRangeDepth} of
    % arrival goal
    {false, true, _} -> 
      AddGoalSceneClosedList = ClosedList ++ [MovedGameStates],
      {goal, GameStateStack, AddGoalSceneClosedList};

    % not arrival goal and depth in range
    {false, false, true} ->
      AddedGameStateStack = queue:in(MovedGameStates, GameStateStack),
      {not_goal, AddedGameStateStack, ClosedList};

    _ ->
      {not_goal, GameStateStack, ClosedList}
  end,
  case Result of
    goal ->
      {solution, UpdatedClosedList};
    _ ->
      search(GameState, UpdatedGameStackState, UpdatedClosedList, MovesRetain)
  end.

-spec get_valid_move(list(state_record)) -> list(map()).
get_valid_move(States) ->
  State = get_null_value_state_record(States),
  {X, Y} = State#state_record.position,
  Moves = [
    {X - 1, Y},
    {X + 1, Y},
    {X, Y - 1},
    {X, Y + 1}
  ],
  remove_invalid_position(Moves).

-spec remove_invalid_position(list(map())) -> list(map()).
remove_invalid_position([]) -> [];
remove_invalid_position(Positions) ->
  [Position|Retain] = Positions,
  case in_range_position(Position) of
    true ->
      [Position];
    false -> 
      []
  end ++ remove_invalid_position(Retain).

-spec in_range_position(map()) -> true | false.
in_range_position(Position) ->
  {X, Y} = Position,
  XRange = (X >= 1) and (X =< 3),
  YRange = (Y >= 1) and (Y =< 3),
  XRange and YRange.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% game state
-spec move_position(list(state_record), map()) -> {ok, list(state_record)} | {error, null}.
move_position(States, Position) -> 
  NullPosition = get_null_value_state_record(States),
  case is_neary_position(Position, NullPosition#state_record.position) of
    true ->
      % exchange position and null position
      {
        ok,
        exchange_game_state_position(
          States, 
          Position, 
          NullPosition#state_record.position)
      };
    false ->
      {error, null}
  end.

-spec is_neary_position(map(), map()) -> true | false.
is_neary_position(Position1, Position2) ->
  {X1, Y1} = Position1,
  {X2, Y2} = Position2,
  Xdelta = abs(X1 - X2),
  Ydelta = abs(Y1 - Y2),
  case {Xdelta, Ydelta } of
    {2, _} -> false;
    {_, 2} -> false;
    _ -> true
  end.

% null position must exists
-spec get_null_value_state_record(list(state_record)) -> state_record.
get_null_value_state_record(States) ->
  lists:keyfind(
    0,
    #state_record.value,
    States).

-spec exchange_game_state_position(list(state_record), map(), map()) -> list(state_record).
exchange_game_state_position(States, Position1, Position2) ->
  Position1Rec = lists:keyfind(
    Position1,
    #state_record.position,
    States),
  Position2Rec = lists:keyfind(
    Position2,
    #state_record.position,
    States),
  remove_game_state_record(States, [Position1, Position2]) ++
  [
   #state_record{position=Position1, value=Position2Rec#state_record.value},
   #state_record{position=Position2, value=Position1Rec#state_record.value}
  ].
    
-spec remove_game_state_record(list(state_record), list(map())) -> list(state_record).
remove_game_state_record(States, []) -> States;
remove_game_state_record(States, Positions) -> 
  [Position|Retain] = Positions,
  RemovedGameState = lists:keydelete(
    Position,
    #state_record.position,
    States),
  remove_game_state_record(RemovedGameState, Retain).

-spec is_goal_state(list(state_record)) -> true | false.
is_goal_state(States) ->
  equal_game_state(States, get_goal_states()).

-spec get_goal_states() -> list(state_record).
get_goal_states() -> 
  [
    #state_record{position={1, 1}, value=1},
    #state_record{position={1, 2}, value=2},
    #state_record{position={1, 3}, value=3},
    #state_record{position={2, 1}, value=8},
    #state_record{position={2, 2}, value=0},
    #state_record{position={2, 3}, value=4},
    #state_record{position={3, 1}, value=7},
    #state_record{position={3, 2}, value=6},
    #state_record{position={3, 3}, value=5}
  ].

-spec exists_states(list(state_record), list(game_state_record)) -> true | false.
exists_states(_, []) -> false;
exists_states(States, StatesList) ->
  [RangeStates|Retain] = StatesList,
  case equal_game_state(States, RangeStates#game_state_record.states) of
    true -> true;
    false ->
      exists_states(States, Retain)
  end.

-spec equal_game_state(list(state_record), list(state_record)) -> true | false.
equal_game_state(States, ExpectStates) when length(States) /= length(ExpectStates) ->
  false;
equal_game_state([], []) -> true;
equal_game_state(States, ExpectState) ->
  [State|StatesRetain] = States,
  ExpectRec = lists:keyfind(
    State#state_record.position,
    #state_record.position,
    ExpectState),
  ExpectVaslue = ExpectRec#state_record.value,
  case State#state_record.value of
      ExpectVaslue ->
      ExpectStateRetain = lists:keydelete(
        State#state_record.position,
        #state_record.position,
        ExpectState),
      equal_game_state(StatesRetain, ExpectStateRetain);
    _ ->
      false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_test_game_state(1) ->
  #game_state_record{depth=0, move=null, states=
  [
   #state_record{position={1,1}, value=8},
   #state_record{position={1,2}, value=1},
   #state_record{position={1,3}, value=3},
   #state_record{position={2,1}, value=2},
   #state_record{position={2,2}, value=4},
   #state_record{position={2,3}, value=5},
   #state_record{position={3,1}, value=0},
   #state_record{position={3,2}, value=7},
   #state_record{position={3,3}, value=6}
  ]};
generate_test_game_state(_) -> [].

-spec show_search_result(integer(), {solution|no_solution, list(game_state_record)}) -> ok.
show_search_result(CaseNo, SearchResult) ->
  {Solution, GameStates} = SearchResult,  
  case Solution of
    solution ->
      ?OUTPUT_INFO(
        "test ~w: result: ~w, game state length: ~w, null move count: ~w", 
        [
          CaseNo, 
          Solution, 
          length(GameStates),
          count_move_is_null(GameStates)
        ]),
      show_search_route(GameStates);
    no_solution ->
      ?OUTPUT_INFO("test ~w: result: ~w", [CaseNo, Solution])
  end,
  ok.

show_search_route(GameStates) ->
  GoalStates = get_goal_states(),
  show_state(GoalStates),
  GoalGameState = search_game_state_from_states(GameStates, GoalStates),
  GoalNullState = get_null_value_state_record(GoalGameState#game_state_record.states),
  BeforeStates = exchange_game_state_position(
    GoalGameState#game_state_record.states,
    GoalNullState#state_record.position,
    GoalGameState#game_state_record.move),
  BeforeGameState = #game_state_record{
    depth=0, 
    move=null,
    states=BeforeStates
  },
  show_search_route(GameStates, BeforeGameState).

show_search_route(GameStates, FindGameStates) ->
  show_state(FindGameStates#game_state_record.states),
  GameState = search_game_state_from_states(GameStates, FindGameStates#game_state_record.states),
  case GameState#game_state_record.move of
    null -> 
      ?OUTPUT_DEBUG("find move is ~w", [null]),
      ok;
    _ ->
      NullState = get_null_value_state_record(GameState#game_state_record.states),
      BeforeStates = exchange_game_state_position(
        GameState#game_state_record.states,
        NullState#state_record.position,
        GameState#game_state_record.move),
      BeforeGameState = #game_state_record{
        depth=0, 
        move=null,
        states=BeforeStates
      },
      show_search_route(GameStates, BeforeGameState)
  end.

show_state(States) ->
  Positions = [
    {1,1}, {1,2}, {1,3},
    {2,1}, {2,2}, {2,3},
    {3,1}, {3,2}, {3,3}
  ],
  io:format("~w~n", [show_state(States, Positions)]).
show_state(_, []) -> [];
show_state(States, Positions) ->
  [Position|Retain] = Positions,
  State = lists:keyfind(
    Position,
    #state_record.position,
    States),
  case State of
    false ->
      ?OUTPUT_ERROR("show_state failure. state is  ~w, position: ~w", [State, Position]);
    _ ->
      [State#state_record.value] ++ show_state(States, Retain)
  end.

search_game_state_from_states(GameStates, FindStates) ->
  [GameState|Retain] = GameStates,
  Equals = equal_game_state(
    GameState#game_state_record.states,
    FindStates),
  case Equals of
    true ->
      GameState;
    false ->
      search_game_state_from_states(Retain, FindStates)
  end.

count_move_is_null([]) -> 0;
count_move_is_null(GameStateList) ->
  [GameState|Retain] = GameStateList,
  case GameState#game_state_record.move of
    null -> 1;
    _ -> 0
  end + count_move_is_null(Retain).