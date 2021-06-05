-module(breadth_first_search).

-export([test/0]).

-record(game_state_record, {move, board_records}).

-record(board_record, {position, value}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 1).

-define(OUTPUT_DEBUG(S),
        case (?LOG_LEVEL) =< 1 of
            true -> io:fwrite("[DEBUG] breadth_first_search: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_DEBUG(S, Args),
        case (?LOG_LEVEL) =< 1 of
            true ->
                io:fwrite("[DEBUG] breadth_first_search: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_INFO(S),
        case (?LOG_LEVEL) =< 2 of
            true -> io:fwrite("[INFO] breadth_first_search: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_INFO(S, Args),
        case (?LOG_LEVEL) =< 2 of
            true ->
                io:fwrite("[INFO] breadth_first_search: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_ERROR(S),
        case (?LOG_LEVEL) =< 3 of
            true -> io:fwrite("[ERROR] breadth_first_search: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_ERROR(S, Args),
        case (?LOG_LEVEL) =< 3 of
            true ->
                io:fwrite("[ERROR] breadth_first_search: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(MAX_DEPTH, 99).
-define(NULL_VALUE, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() -> 
  test(1),
  ok.

test(CaseNo) ->
  GameState = generate_game_state(CaseNo),
  SearchResult = search(GameState),
  show_search_result(CaseNo, SearchResult),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(game_state_record) -> {solution | not_solution, list(game_state_record)}.
search(GameState) ->
  OpenQueue = queue:new(),
  InitializedOpenQueue  = queue:in(GameState, OpenQueue),
  ClosedGameStateLisst = [],
  search(InitializedOpenQueue, ClosedGameStateLisst).

-spec search(list(game_state_record), list(game_state_list)) -> 
  {solution|not_solution, list(game_state_list)}.
search(OpenQueue, ClosedList) ->
  case queue:len(OpenQueue) of
    0 -> {no_solution, ClosedList};
    _ ->
      {{value, HeadGameState}, HeadedOpenQueue} = queue:out(OpenQueue),
      AddedClosedList = [HeadGameState] ++ ClosedList,
      ValidMoves = generate_valid_moves(HeadGameState),
      search(HeadedOpenQueue, AddedClosedList, HeadGameState, ValidMoves)
  end.

-spec search(
    list(game_state_record), 
    list(game_state_record), 
    game_state_record,
    list({integer(), integer()})) ->
  {solution|not_solution, list(game_state_list)}.
search(OpenQueue, ClosedList, _, []) ->
  search(OpenQueue, ClosedList);
search(OpenQueue, ClosedList, GameState, Moves) ->
  [Move|MoveRetain] = Moves,
  % for route trace
  NullPositionBeforeMove = get_null_position(
    GameState#game_state_record.board_records),
  MovedGameState = calculate_moved_game_state(GameState, Move),
  ExistsClosedList = exists_closed_list(MovedGameState, ClosedList),
  IsGoal = is_goal_game_state(MovedGameState),
  {Result, NewOpenQueue, NewClosedList} = case {ExistsClosedList, IsGoal} of
    % arrival goal
    {false, true} ->
      ChangeMovedGameState = MovedGameState#game_state_record{move=NullPositionBeforeMove},
      AddFinishedClosedList = ClosedList ++ [ChangeMovedGameState],
      {solution, OpenQueue, AddFinishedClosedList};
    {false, _} ->
      ?OUTPUT_DEBUG(
        "search - add queue (null: ~w, move: ~w)",
        [NullPositionBeforeMove, Move]),
      ChangeMovedGameState = MovedGameState#game_state_record{move=NullPositionBeforeMove},
      AddMovedOpenQueue = queue:in(ChangeMovedGameState, OpenQueue),
      {no_solution, AddMovedOpenQueue, ClosedList};
    _ ->
      {no_solution, OpenQueue, ClosedList}
  end,
  case Result of
    solution ->
      {solution, NewClosedList};
    _ ->
      search(NewOpenQueue, NewClosedList, GameState, MoveRetain)
  end.

-spec generate_game_state(integer()) -> game_state_record.
generate_game_state(1) ->
  #game_state_record{
     move=null,
     board_records=[
       #board_record{position={1,1} , value=2},
       #board_record{position={1,2} , value=8},
       #board_record{position={1,3} , value=3},
       #board_record{position={2,1} , value=1},
       #board_record{position={2,2} , value=6},
       #board_record{position={2,3} , value=4},
       #board_record{position={3,1} , value=7},
       #board_record{position={3,2} , value=?NULL_VALUE},
       #board_record{position={3,3} , value=5}
     ]
  };
generate_game_state(_) -> null.

-spec generate_valid_moves(game_state_record) -> game_state_record.
generate_valid_moves(GameState) ->
  BoardRecords = GameState#game_state_record.board_records,
  {X, Y} = get_null_position(BoardRecords),
  Moves = [
    {X - 1, Y},
    {X + 1, Y},
    {X, Y - 1},
    {X, Y + 1}
  ],
  remove_invalid_position(Moves).

-spec calculate_moved_game_state(
    game_state_record, {integer(), integer()}) -> 
  game_state_record.
calculate_moved_game_state(GameState, Move) ->
  BoardRecords = GameState#game_state_record.board_records,
  NullPosition = get_null_position(BoardRecords),
  NewBoardRecords = exchange_position_value(
    BoardRecords, 
    Move, 
    NullPosition),
  #game_state_record{
     move=Move,
     board_records=NewBoardRecords 
  }.

-spec exists_closed_list(game_state_record, list(game_state_record)) -> true | false.
exists_closed_list(_, []) -> false;
exists_closed_list(FindGameState, GameStateList) ->
  [GameState|GameStateListRetain] = GameStateList,
  FindBoardRecords = FindGameState#game_state_record.board_records,
  ExpectBoardRecords = GameState#game_state_record.board_records,
  case equals_board_records(FindBoardRecords, ExpectBoardRecords) of
    true -> true;
    false ->
      exists_closed_list(FindGameState, GameStateListRetain)
  end.

-spec exchange_position_value(
    list(board_record),
    {integer(), integer()},
    {integer(), integer()}) ->
  list(board_record).
exchange_position_value(BoardRecords, Position1, Position2) ->
  Position1Rec = lists:keyfind(
    Position1,
    #board_record.position,
    BoardRecords),
  Position2Rec = lists:keyfind(
    Position2,
    #board_record.position,
    BoardRecords),
  remove_board_records_positions(BoardRecords, [Position1, Position2]) ++
  [
   #board_record{position=Position1, value=Position2Rec#board_record.value},
   #board_record{position=Position2, value=Position1Rec#board_record.value}
  ].

-spec remove_board_records_positions(
        list(board_record),
        list({integer(), integer()})) -> list(board_record).
remove_board_records_positions(BoardRecords, []) -> BoardRecords;
remove_board_records_positions(BoardRecords, RemovePositionList) ->
  [RemovePosition | RemovePositionListRetain] = RemovePositionList,
  RemovedBoardRecords = lists:keydelete(
    RemovePosition,
    #board_record.position,
    BoardRecords),
  remove_board_records_positions(RemovedBoardRecords, RemovePositionListRetain).

-spec get_null_position(list(board_record)) -> {integer(), integer()}.
get_null_position(BoardRecords) ->
  BoardRecord = lists:keyfind(
    ?NULL_VALUE ,
    #board_record.value,
    BoardRecords),
  BoardRecord#board_record.position.

-spec remove_invalid_position(list({integer(), integer()})) -> 
  list({integer(), integer()}).
remove_invalid_position([]) -> [];
remove_invalid_position(Positions) ->
  [Position | Retain] = Positions,
  case validate_position(Position) of
    true -> [Position];
    false -> []
  end ++ remove_invalid_position(Retain).

-spec validate_position({integer(), integer()}) -> true | false.
validate_position(Position) ->
  {X, Y} = Position,
  XRange = (X >= 1) and (X =< 3),
  YRange = (Y >= 1) and (Y =< 3),
  XRange and YRange.

-spec is_goal_game_state(game_state_record) -> true | false.
is_goal_game_state(GameState) ->
  BoardRecords = GameState#game_state_record.board_records,
  GoalBoardRecords = get_goal_board_records(),
  equals_board_records(BoardRecords, GoalBoardRecords).

-spec get_goal_board_records() -> list(board_record).
get_goal_board_records() ->
  [
    #board_record{position={1, 1}, value=1},
    #board_record{position={1, 2}, value=2},
    #board_record{position={1, 3}, value=3},
    #board_record{position={2, 1}, value=8},
    #board_record{position={2, 2}, value=0},
    #board_record{position={2, 3}, value=4},
    #board_record{position={3, 1}, value=7},
    #board_record{position={3, 2}, value=6},
    #board_record{position={3, 3}, value=5}
  ].

-spec equals_board_records(
    list(board_record),
    list(board_record)) -> true | false.
equals_board_records(BoardRecords, ExpectsRecords) when 
    length(BoardRecords) /= length(ExpectsRecords) -> false;
equals_board_records([], []) -> true;
equals_board_records(BoardRecords, ExpectsRecords) ->
  [BoardRecord | BoardRecordsRetain] = BoardRecords,
  ExpectRecord = lists:keyfind(
    BoardRecord#board_record.position,
    #board_record.position,
    ExpectsRecords),
  ExpectVaslue = ExpectRecord#board_record.value,
  case BoardRecord#board_record.value of
    ExpectVaslue ->
      ExpectsRecordsRetain = lists:keydelete(
        BoardRecord#board_record.position,
        #board_record.position,
        ExpectsRecords),
      equals_board_records(BoardRecordsRetain, ExpectsRecordsRetain);
    _ ->
      false
  end.


-spec show_search_result(
    integer(), 
    {solution|no_solution, list(game_state_record)}) -> ok.
show_search_result(CaseNo, SearchResult) ->
  {Solution, GameStateList} = SearchResult,  
  case Solution of
    solution ->
      ?OUTPUT_INFO(
        "test ~w: result: ~w, game state length: ~w", 
        [
          CaseNo, 
          Solution, 
          length(GameStateList)
        ]),
      show_search_route(GameStateList);
    not_solution ->
      ?OUTPUT_INFO("test ~w: result: ~w", [CaseNo, Solution])
  end,
  ok.

-spec show_search_route(list(game_state_record)) -> ok.
show_search_route(GameStateList) ->
  GoalBoardRecords= get_goal_board_records(),
  show_board_records(GoalBoardRecords),
  GameState = find_game_state_from_board_records(GameStateList, GoalBoardRecords),
  NullPosition = get_null_position(GameState#game_state_record.board_records),
  NewBoardRecords = exchange_position_value(
    GameState#game_state_record.board_records, 
    GameState#game_state_record.move, 
    NullPosition),
  BeforeGameState = #game_state_record{
    move=null,
    board_records=NewBoardRecords
  },
  show_search_route(GameStateList, BeforeGameState).

-spec show_search_route(list(game_state_record), game_state_record) -> ok.
show_search_route(GameStateList, FindGameState) ->
  show_board_records(FindGameState#game_state_record.board_records),
  GameState = find_game_state_from_board_records(
    GameStateList, 
    FindGameState#game_state_record.board_records),
  case GameState#game_state_record.move of
    null ->
      ?OUTPUT_DEBUG("find move is ~w", [null]),
      ok;
    _ ->
      NullPosition = get_null_position(GameState#game_state_record.board_records),
      NewBoardRecords = exchange_position_value(
        GameState#game_state_record.board_records, 
        GameState#game_state_record.move, 
        NullPosition),
      BeforeGameState = #game_state_record{
        move=null,
        board_records=NewBoardRecords
      },
      show_search_route(GameStateList, BeforeGameState)
  end.

-spec show_board_records(list(board_record)) -> ok.
show_board_records(BoardRecords) ->
  Positions = [
    {1,1}, {1,2}, {1,3},
    {2,1}, {2,2}, {2,3},
    {3,1}, {3,2}, {3,3}
  ],
  io:format("~w~n", [show_board_records(BoardRecords, Positions)]).

-spec show_board_records(list(board_record), list({integer(), integer()})) -> list(integer()).
show_board_records(_, []) -> [];
show_board_records(BoardRecords, Positions) ->
  [Position|Retain] = Positions,
  BoardRecord = lists:keyfind(
    Position,
    #board_record.position,
    BoardRecords),
  case BoardRecord of
    false ->
      ?OUTPUT_ERROR("show_state failure. state is  ~w, position: ~w", [BoardRecords, Position]);
    _ ->
      [BoardRecord#board_record.value] ++ show_board_records(BoardRecords, Retain)
  end.

-spec find_game_state_from_board_records(
    list(game_state_record), 
    list(board_record)) -> game_state_record.
find_game_state_from_board_records([], _) -> 
  ?OUTPUT_ERROR("find_game_state_from_board_records - ~w",[board_records_not_found]),
  #game_state_record{
     move=null,
     board_records=[]
  };
find_game_state_from_board_records(GameStateList, BoardRecords) ->
  [GameState|GameStateListRetain] = GameStateList,
  case equals_board_records(GameState#game_state_record.board_records, BoardRecords) of
    true -> GameState;
    false ->
      find_game_state_from_board_records(GameStateListRetain, BoardRecords)
  end.
