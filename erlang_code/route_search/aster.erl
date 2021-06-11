% erl -eval "aster:test()."
-module(aster).

-export([test/0]).

-record(game_state_record, {depth, score, move, board_records}).

-record(board_record, {position, value}).

-record(move_record, {from, to}).

-record(position_record, {x, y}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
  case (?LOG_LEVEL) =< 1 of
    true -> io:fwrite("[DEBUG] aster: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_DEBUG(S, Args),
  case (?LOG_LEVEL) =< 1 of
    true ->
      io:fwrite("[DEBUG] aster: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_INFO(S),
  case (?LOG_LEVEL) =< 2 of
    true -> io:fwrite("[INFO] aster: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_INFO(S, Args),
  case (?LOG_LEVEL) =< 2 of
    true ->
      io:fwrite("[INFO] aster: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(OUTPUT_ERROR(S),
  case (?LOG_LEVEL) =< 3 of
    true -> io:fwrite("[ERROR] aster: " ++ S ++ "~n");
    _ -> ok
  end).

-define(OUTPUT_ERROR(S, Args),
  case (?LOG_LEVEL) =< 3 of
    true ->
      io:fwrite("[ERROR] aster: " ++ S ++ "~n", Args);
    _ -> ok
  end).

-define(MAX_PRIORITY, 9999).

% 9!
-define(MAX_DEPTH, 362880).
-define(MIN_DEPTH, 0).

-define(MAX_SCORE, 99999).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec test() -> ok.
test() ->
  test(1),
  test(2),
  test(3),
  test(4),
  test(5),
  test(6),
  ok.

-spec test(integer()) -> ok.
test(TestCase) ->
  GameState = generate_game_state(TestCase),
  EvaluatorType = generate_evaluator_type(TestCase),
  SearchResult = search(GameState, EvaluatorType),
  ?OUTPUT_DEBUG("search finish"),
  show_search_result(TestCase, EvaluatorType, SearchResult),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% EvaluatorType -> 
%   fair_evaluator | good_evalueator | weak_evalueator | bad_evaluator
-spec search(game_state_record, atom()) -> 
  {solution | not_solution, list(game_state_record)}.
search(InitialGameState, EvaluatorType) ->
  OpenGameStateList = [InitialGameState],
  ClosedGameStateList = [],
  search(
    OpenGameStateList, 
    ClosedGameStateList, 
    EvaluatorType).

-spec search(list(game_state_record), list(game_state_record), atom()) -> 
  {solution | not_solution, list(game_state_record)}.
search([], ClosedGameStateList, _) ->
  {not_solution, ClosedGameStateList};
search(OpenGameStateList, ClosedGameStateList, EvaluatorType) ->
  {GameState, RemovedOpenGameStateList} = 
    get_minimum_priority_game_state(OpenGameStateList),
  ArrivalGoal = is_goal_game_state(GameState),
  case ArrivalGoal of
    true ->
      % add goal game state
      AddGoalClosedGameStateList = 
        [GameState] ++ ClosedGameStateList,
      {solution, AddGoalClosedGameStateList};
    false ->
      AddedClosedGameStateList = [GameState] ++ ClosedGameStateList,
      ValidMoves = generate_valid_moves(GameState),
      search(
        GameState,
        RemovedOpenGameStateList, 
        AddedClosedGameStateList, 
        EvaluatorType,
        ValidMoves 
       )
  end.

-spec search(
    game_state_record, 
    list(game_state_record), 
    list(game_state_record), 
    atom(), 
    list(move_record)) -> 
  {solution | not_solution, list(game_state_record)}.
search(_, OpenGameStateList, ClosedGameStateList, EvaluatorType, []) ->
  search(OpenGameStateList, ClosedGameStateList, EvaluatorType);
search(GameState, OpenGameStateList, ClosedGameStateList, EvaluatorType, Moves) ->
  [Move|MoveRetain] = Moves,
  MovedGameState = execute_move(GameState, Move),
  ExistsCloseList = exists_game_state_list(MovedGameState, ClosedGameStateList),
  ExistsOpenGameState = find_game_state(MovedGameState, OpenGameStateList),
  ScoreAfterMove = evaluate_score(MovedGameState, EvaluatorType),
  ScoredMovedGameState = MovedGameState#game_state_record{score=ScoreAfterMove},
  {NextOpenGameStateList} = case {ExistsCloseList, ScoredMovedGameState#game_state_record.depth, ExistsOpenGameState} of
    % exists closed list
    {true, _, _} ->
      {OpenGameStateList};

    % arrival max depth
    {false, ?MAX_DEPTH, _} ->
      {OpenGameStateList};

    % open state not exists
    {false, _, null} ->
      AddNextStateOpenGameStateList = 
        [ScoredMovedGameState] ++ OpenGameStateList,
      {AddNextStateOpenGameStateList};

    % open state exists
    {false, _, OpenGameState} ->
      % remove open game state and add moved game state
      RemovedGreaterSccore = 
        remove_greater_score_game_state(
          ScoreAfterMove, 
          OpenGameState, 
          OpenGameStateList),
      AddNextStateOpenGameStateList = 
        [ScoredMovedGameState] ++ RemovedGreaterSccore,
      {AddNextStateOpenGameStateList}
  end,
  search(
    GameState, 
    NextOpenGameStateList, 
    ClosedGameStateList, 
    EvaluatorType, 
    MoveRetain).

% fair_evaluator | good_evalueator | weak_evalueator | bad_evaluator
-spec evaluate_score(game_state_record, atom()) -> integer().
evaluate_score(GameState, fair_evaluator) -> 
  GoalGameState = get_goal_game_state(),
  evaluate_fair_score(
    GameState#game_state_record.board_records,
    GoalGameState#game_state_record.board_records);

evaluate_score(GameState, good_evalueator) -> 
  GoalGameState = get_goal_game_state(),
  evaluate_fair_score(
    GameState#game_state_record.board_records,
    GoalGameState#game_state_record.board_records) + 
  evaluate_good_score_s(
    GameState#game_state_record.board_records,
    GoalGameState#game_state_record.board_records) * 3;

evaluate_score(GameState, weak_evalueator) -> 
  GoalGameState = get_goal_game_state(),
  evaluate_weak_score(
    GameState#game_state_record.board_records,
    GoalGameState#game_state_record.board_records);

evaluate_score(GameState, bad_evaluator) -> 
  evaluate_bad_score(GameState#game_state_record.board_records).

-spec evaluate_fair_score(list(board_record), list(board_record)) -> integer().
evaluate_fair_score([], _) -> 0;
evaluate_fair_score(BoardRecords, GoalBoardRecords) ->
  [Head | Retain] = BoardRecords,
  GoalRec = lists:keyfind(
      Head#board_record.value,
      #board_record.value,
      GoalBoardRecords),
  abs(Head#board_record.position#position_record.x -
    GoalRec#board_record.position#position_record.x) + 
  abs(Head#board_record.position#position_record.y -
    GoalRec#board_record.position#position_record.y) + 
  evaluate_fair_score(Retain, GoalBoardRecords).

-spec evaluate_good_score_s(list(board_record), list(board_record)) -> integer().
evaluate_good_score_s(BoardRecords, GoalBoardRecords) ->
  CheckPoistions = [
    #position_record{x=1, y=1},
    #position_record{x=2, y=1},
    #position_record{x=3, y=1},
    #position_record{x=3, y=2},
    #position_record{x=3, y=3},
    #position_record{x=2, y=3},
    #position_record{x=1, y=3},
    #position_record{x=1, y=2}
  ],
  evaluate_good_score_s(CheckPoistions, BoardRecords, GoalBoardRecords).

-spec evaluate_good_score_s(
    list(position_record), list(board_record), list(board_record)) -> integer().
evaluate_good_score_s([], _, _) -> 0;
evaluate_good_score_s(Positions, BoardRecords, GoalBoardRecords) ->
  [HeadPosition|PositionRetain] = Positions,
  CheckRec = lists:keyfind(
      HeadPosition,
      #board_record.position,
      BoardRecords),
  GoalRec = lists:keyfind(
      HeadPosition,
      #board_record.position,
      GoalBoardRecords),
  case {CheckRec#board_record.value, GoalRec#board_record.value} of
    {0, _} -> 1;
    {CheckValue, CheckValue} -> 0;
    _ -> 2
  end +
  evaluate_good_score_s(PositionRetain, BoardRecords, GoalBoardRecords).


-spec evaluate_weak_score(list(board_record), list(board_record)) -> integer().
evaluate_weak_score([], _) -> 0;
evaluate_weak_score(BoardRecords, GoalBoardRecords) ->
  [Head | Retain] = BoardRecords,
  GoalRec = lists:keyfind(
      Head#board_record.position,
      #board_record.position,
      GoalBoardRecords),
  case GoalRec#board_record.value == Head#board_record.value of
    true -> 0;
    false -> 1
  end + evaluate_weak_score(Retain, GoalBoardRecords).


-spec evaluate_bad_score(list(board_record)) -> integer().
evaluate_bad_score(BoardRecords) ->
  DeltaPositions = [
    {#position_record{x=1, y=1}, #position_record{x=3, y=3}},
    {#position_record{x=3, y=1}, #position_record{x=1, y=3}},
    {#position_record{x=2, y=1}, #position_record{x=2, y=3}},
    {#position_record{x=1, y=2}, #position_record{x=3, y=2}}
  ],
  DeltaSummary = evaluate_bad_score_delta_summary(DeltaPositions, BoardRecords),
  16 - DeltaSummary.

evaluate_bad_score_delta_summary([], _) -> 0;
evaluate_bad_score_delta_summary(DeltaPositions, BoardRecords) ->
  [{Position1, Position2}|Retain] = DeltaPositions,
  Position1Rec = lists:keyfind(
      Position1,
      #board_record.position,
      BoardRecords),
  Position2Rec = lists:keyfind(
      Position2,
      #board_record.position,
      BoardRecords),
  abs(Position1Rec#board_record.value - Position2Rec#board_record.value) + 
  evaluate_bad_score_delta_summary(Retain, BoardRecords).

-spec get_minimum_priority_game_state(list(game_state_record)) ->
  {game_state_record, list(game_state_record)}.
get_minimum_priority_game_state(GameStateList) ->
  get_minimum_priority_game_state(GameStateList, []).

-spec get_minimum_priority_game_state(
    list(game_state_record),
    list(game_state_record)) ->
  {game_state_record, list(game_state_record)}.
get_minimum_priority_game_state(GameStateList, GameStateListRetain) ->
  [GameState|Retain] = GameStateList,
  LowerPriorityRecords = [Score || 
    #game_state_record{score=Score} <- Retain, 
    Score < GameState#game_state_record.score],
  case length(LowerPriorityRecords) of
    0 ->
     {
        GameState, 
        GameStateListRetain ++ Retain
     };
    _ ->
      get_minimum_priority_game_state(
        Retain, 
        [GameState] ++ GameStateListRetain)
  end.

-spec generate_valid_moves(game_state_record) ->
  list(move_record).
generate_valid_moves(GameState) -> 
  NullBoardRecord = get_null_board_record(GameState),
  #position_record{x=X, y=Y} = NullBoardRecord#board_record.position,
  remove_invalid_move([
    #move_record{
      from=#position_record{x = X - 1, y = Y}, 
      to=NullBoardRecord#board_record.position},
    #move_record{
      from=#position_record{x = X + 1, y = Y}, 
      to=NullBoardRecord#board_record.position},
    #move_record{
      from=#position_record{x = X, y = Y - 1}, 
      to=NullBoardRecord#board_record.position},
    #move_record{
      from=#position_record{x = X, y = Y + 1}, 
      to=NullBoardRecord#board_record.position}
  ]).

-spec remove_invalid_move(list(move_record)) -> list(move_record).
remove_invalid_move([]) -> [];
remove_invalid_move(Moves) ->
  [Move|Retain] = Moves,
  ValidateMove = 
    validate_position(Move#move_record.from) and
    validate_position(Move#move_record.to),
  case ValidateMove of
    true ->
      [Move] ++ remove_invalid_move(Retain);
    false ->
      remove_invalid_move(Retain)
  end.

-spec validate_position(position_record) -> true | false.
validate_position(Position) ->
  ValidX = 
    (Position#position_record.x > 0) 
    and (Position#position_record.x < 4),
  ValidY = 
    (Position#position_record.y > 0) 
    and (Position#position_record.y < 4),
  (ValidX and ValidY).

-spec remove_greater_score_game_state(
  integer(), game_state_record, list(game_state_record)) -> 
    list(game_state_record).
remove_greater_score_game_state(_, null, GameStateList) -> GameStateList;
remove_greater_score_game_state(Score, GameState, GameStateList) ->
  GreaterScore = (Score < GameState#game_state_record.score),
  case GreaterScore of
    true ->
      remove_game_state(GameState, GameStateList);
    false ->
      GameStateList
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% game state function
-spec is_goal_game_state(game_state_record) -> true | false.
is_goal_game_state(GameState) ->
  GoalGameState = get_goal_game_state(),
  equal_board_records(
    GameState#game_state_record.board_records,
    GoalGameState#game_state_record.board_records).


-spec get_goal_game_state() -> game_state_record.
get_goal_game_state() ->
  #game_state_record{
    depth=?MIN_DEPTH,
    score=?MAX_SCORE,
    move=null,
    board_records=[
      #board_record{position=#position_record{x=1, y=1}, value=1},
      #board_record{position=#position_record{x=2, y=1}, value=2},
      #board_record{position=#position_record{x=3, y=1}, value=3},
      #board_record{position=#position_record{x=1, y=2}, value=8},
      #board_record{position=#position_record{x=2, y=2}, value=0},
      #board_record{position=#position_record{x=3, y=2}, value=4},
      #board_record{position=#position_record{x=1, y=3}, value=7},
      #board_record{position=#position_record{x=2, y=3}, value=6},
      #board_record{position=#position_record{x=3, y=3}, value=5}
    ]
  }.

-spec equal_board_records(list(board_record),list(board_record)) -> true | false.
equal_board_records(BoardRecordsA, BoardRecordsB) 
  when length(BoardRecordsA) /=  length(BoardRecordsB) -> false;
equal_board_records([], []) -> true;
equal_board_records(BoardRecordsA, BoardRecordsB) -> 
  [RecA|BoardRecordsARetain] = BoardRecordsA,
  RecB = lists:keyfind(RecA#board_record.position,
    #board_record.position,
    BoardRecordsB),
  EqualState = RecA#board_record.value == RecB#board_record.value,
  case EqualState of
    true ->
      BoardRecordsBRetain = lists:keydelete(
        RecA#board_record.position,
        #board_record.position,
        BoardRecordsB),
      equal_board_records(
        BoardRecordsARetain,
        BoardRecordsBRetain);
    false -> false
  end.

-spec get_null_board_record(game_state_record) -> board_record.
get_null_board_record(GameState) ->
  lists:keyfind(
    0,
    #board_record.value,
    GameState#game_state_record.board_records).

-spec execute_move(game_state_record, move_record) -> game_state_record.
execute_move(GameState, Move) ->
  FromRec = lists:keyfind(
    Move#move_record.from,
    #board_record.position,
    GameState#game_state_record.board_records),
  ToRec = lists:keyfind(
    Move#move_record.to,
    #board_record.position,
    GameState#game_state_record.board_records),
  RemoveFromRecords = lists:keydelete(
    Move#move_record.from,
    #board_record.position,
    GameState#game_state_record.board_records),
  RemoveToRecords = lists:keydelete(
    Move#move_record.to,
    #board_record.position,
    RemoveFromRecords),
  SwapAfterRecords = RemoveToRecords ++
  [
    FromRec#board_record{value=ToRec#board_record.value},
    ToRec#board_record{value=FromRec#board_record.value}
  ],
  GameState#game_state_record{
    depth=GameState#game_state_record.depth + 1,
    move=Move,
    board_records=SwapAfterRecords
  }.

-spec exists_game_state_list(game_state_record, list(game_state_record)) -> true | false.
exists_game_state_list(_, []) -> false;
exists_game_state_list(GameState, GameStateList) ->
  ExistsGameState = find_game_state(GameState, GameStateList),
  case ExistsGameState of
    null -> false;
    _ -> true
  end.

-spec find_game_state(game_state_record, list(game_state_record)) -> 
  game_state_record | null.
find_game_state(_, []) -> null;
find_game_state(GameState, GameStateList) ->
  [Head|Retain] = GameStateList,
  Equals = equal_board_records(
    GameState#game_state_record.board_records,
    Head#game_state_record.board_records),
  case Equals of
    true -> Head;
    false ->
      find_game_state(GameState, Retain)
  end.

-spec remove_game_state(game_state_record, list(game_state_record)) -> 
  list(game_state_record).
remove_game_state(_, []) -> [];
remove_game_state(GameState, GameStateList) -> 
  [Head | Retain] = GameStateList,
  Equals = equal_board_records(
    GameState#game_state_record.board_records,
    Head#game_state_record.board_records),
  {Keep, NextRetain} = case Equals of
    true -> {Retain, []};
    false -> {[Head], Retain}
  end,
  Keep ++ remove_game_state(GameState, NextRetain).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate_evaluator_types() -> 
  list(fair_evaluator | good_evalueator | weak_evalueator | bad_evaluator).
generate_evaluator_types() ->
  [
    fair_evaluator, 
    fair_evaluator, 
    fair_evaluator, 
    good_evalueator, 
    weak_evalueator, 
    bad_evaluator
  ].

-spec generate_evaluator_type(integer()) -> 
  fair_evaluator | good_evalueator | weak_evalueator | bad_evaluator.
generate_evaluator_type(Index) ->
  lists:nth(Index, generate_evaluator_types()).

-spec generate_game_state(integer()) -> game_state_record.
generate_game_state(1) ->
  generate_initial_game_state([1,0,3,8,2,4,7,6,5]);
generate_game_state(2) ->
  generate_initial_game_state([0,1,3,8,2,4,7,6,5]);
generate_game_state(3) ->
  generate_initial_game_state([8,1,3,0,4,5,2,7,6]);
generate_game_state(4) ->
  generate_initial_game_state([8,1,3,0,4,5,2,7,6]);
generate_game_state(5) ->
  generate_initial_game_state([8,1,3,0,4,5,2,7,6]);
generate_game_state(6) ->
  generate_initial_game_state([8,1,3,0,4,5,2,7,6]);
generate_game_state(_) ->
  generate_initial_game_state([1,2,3,8,0,4,7,6,5]).

-spec generate_initial_game_state(list(integer())) -> game_state_record.
generate_initial_game_state(ValueList) ->
  #game_state_record{
    depth = ?MIN_DEPTH, 
    score = ?MAX_SCORE, 
    move = null, 
    board_records = [
      #board_record{position=#position_record{x=1, y=1} , value=lists:nth(1,ValueList)},
      #board_record{position=#position_record{x=2, y=1} , value=lists:nth(2,ValueList)},
      #board_record{position=#position_record{x=3, y=1} , value=lists:nth(3,ValueList)},
      #board_record{position=#position_record{x=1, y=2} , value=lists:nth(4,ValueList)},
      #board_record{position=#position_record{x=2, y=2} , value=lists:nth(5,ValueList)},
      #board_record{position=#position_record{x=3, y=2} , value=lists:nth(6,ValueList)},
      #board_record{position=#position_record{x=1, y=3} , value=lists:nth(7,ValueList)},
      #board_record{position=#position_record{x=2, y=3} , value=lists:nth(8,ValueList)},
      #board_record{position=#position_record{x=3, y=3} , value=lists:nth(9,ValueList)}
    ]
  }.


-spec show_search_result(
    integer(), 
    atom(),
    {solution|no_solution, list(game_state_record)}) -> ok.
show_search_result(CaseNo, EvaluateType, SearchResult) ->
  {Solution, GameStateList} = SearchResult,  
  case Solution of
    solution ->
      RouteCount = count_search_route(GameStateList),
      ?OUTPUT_INFO(
        "test ~w: evaluator: ~w, result: ~w, goal step: ~w, game state length: ~w", 
        [
          CaseNo, 
          EvaluateType,
          Solution, 
          RouteCount,
          length(GameStateList)
        ]);
    not_solution ->
      ?OUTPUT_INFO(
         "test ~w: evaluator: ~w, result: ~w", 
         [CaseNo, 
          EvaluateType,
          Solution])
  end,
  ok.

-spec count_search_route(list(game_state_record)) -> integer().
count_search_route(GameStateList) ->
  GoalGameState = get_goal_game_state(),
  GameState = find_game_state(GoalGameState, GameStateList),
  BeforeGameState = execute_move(GameState, GameState#game_state_record.move),
  1 + count_search_route(GameStateList, BeforeGameState).

-spec count_search_route(list(game_state_record), game_state_record) -> integer(). 
count_search_route(GameStateList, FindGameState) ->
  % show_board_records(FindGameState#game_state_record.board_records),
  GameState = find_game_state(FindGameState, GameStateList),
  case GameState#game_state_record.move of
    null ->
      0;
    _ ->
      BeforeGameState = execute_move(GameState, GameState#game_state_record.move),
      1 + count_search_route(GameStateList, BeforeGameState)
  end.

% for debug
% -spec show_board_records(list(board_record)) -> ok.
% show_board_records(BoardRecords) ->
%   io:format("~w~n", [get_board_record_values_list(BoardRecords)]).

% get_board_record_values_list(BoardRecords) ->
%   Positions = [
%     #position_record{x=1,y=1},
%     #position_record{x=2,y=1}, 
%     #position_record{x=3,y=1},
%     #position_record{x=1,y=2}, 
%     #position_record{x=2,y=2}, 
%     #position_record{x=3,y=2},
%     #position_record{x=1,y=3}, 
%     #position_record{x=2,y=3}, 
%     #position_record{x=3,y=3}
%   ],
%   show_board_records(BoardRecords, Positions).

% -spec show_board_records(list(board_record), list({integer(), integer()})) -> list(integer()).
% show_board_records(_, []) -> [];
% show_board_records(BoardRecords, Positions) ->
%   [Position|Retain] = Positions,
%   BoardRecord = lists:keyfind(
%     Position,
%     #board_record.position,
%     BoardRecords),
%   case BoardRecord of
%     false ->
%       ?OUTPUT_ERROR("show_state failure. state is  ~w, position: ~w", [BoardRecords, Position]);
%     _ ->
%       [BoardRecord#board_record.value] ++ show_board_records(BoardRecords, Retain)
%   end.
