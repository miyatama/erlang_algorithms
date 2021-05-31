-module(alpha_beta).

-export([test/0]).

% side = main | opponent
% rf(player_record).
% rd(player_record, {side}).
-record(player_record, {side}).

% pos -> {X:integer(), Y:integer()}, state -> main | opponent | null
% rf(game_state_record).
% rd(game_state_record, {position, state}).
-record(game_state_record, {position, state}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
        case (?LOG_LEVEL) =< 1 of
            true -> io:fwrite("[DEBUG] alpha_beta: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_DEBUG(S, Args),
        case (?LOG_LEVEL) =< 1 of
            true ->
                io:fwrite("[DEBUG] alpha_beta: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_INFO(S),
        case (?LOG_LEVEL) =< 2 of
            true -> io:fwrite("[INFO] alpha_beta: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_INFO(S, Args),
        case (?LOG_LEVEL) =< 2 of
            true ->
                io:fwrite("[INFO] alpha_beta: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_ERROR(S),
        case (?LOG_LEVEL) =< 3 of
            true -> io:fwrite("[ERROR] alpha_beta: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_ERROR(S, Args),
        case (?LOG_LEVEL) =< 3 of
            true ->
                io:fwrite("[ERROR] alpha_beta: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(MAX_PLYDEPTH, 99).

-define(WIN_SCORE, 100).

-define(LOSE_SCORE, -100).

-define(DRAW_SCORE, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
    test(1),
    test(2),
    test(3),
    test(4),
    test(5),
    ok.

test(CaseNo) ->
    GameState = generate_game_state(CaseNo),
    MainPlayer = generate_main_player(),
    {Move, Score} = best_move(GameState, MainPlayer),
    Expect = generate_expect_move(CaseNo),
    case Move of
        Expect ->
            ?OUTPUT_INFO("test case ~w: ~w,score is ~w.",
                         [CaseNo, pass, Score]);
        _ ->
            ?OUTPUT_INFO("test case ~w: ~w, Expect: ~w, Result: "
                         "~w, score is ~w.",
                         [CaseNo, failure, Expect, Move, Score])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
best_move(State, MainPlayer) ->
    PlyDepth = (?MAX_PLYDEPTH),
    alpha_beta(State,
               PlyDepth,
               MainPlayer,
               null,
               null,
               ?LOSE_SCORE,
               ?WIN_SCORE).

alpha_beta(GameState, PlyDepth, MainPlayer, Move, Score,
           Low, High) ->
    case allow_thinking(GameState, PlyDepth) of
        true ->
            alpha_beta(GameState,
                       PlyDepth,
                       MainPlayer,
                       get_valid_move(GameState),
                       Move,
                       Score,
                       Low,
                       High);
        false ->
            ?OUTPUT_DEBUG("alpha_beta - arrival leaf: ~w",
                          [GameState]),
            {null, evaluate_score(GameState, MainPlayer)}
    end.

alpha_beta(GameState, _, _, [], Move, Score, _, _) ->
    ?OUTPUT_DEBUG("alpha_beta: valid moves not found: ~w",
                  [GameState]),
    {Move, Score};
alpha_beta(GameState, PlyDepth, MainPlayer, ValidMoves,
           Move, Score, Low, High) ->
    [ValidMove | ValidMovesRetain] = ValidMoves,
    MovedGameState = set_move(GameState,
                              MainPlayer,
                              ValidMove),
    {_, OpponentScore} = alpha_beta(MovedGameState,
                                    PlyDepth - 1,
                                    get_opponent_player(MainPlayer),
                                    null,
                                    null,
                                    High * -1,
                                    Low * -1),
    {NewMove, NewScore, NewLow} = case
                                      need_exchange_score(Score, OpponentScore)
                                      of
                                      true ->
                                          {ValidMove,
                                           Low * -1,
                                           OpponentScore * -1};
                                      false -> {Move, Score, Low}
                                  end,
    case NewLow >= High of
        true -> {NewMove, NewScore};
        false ->
            alpha_beta(GameState,
                       PlyDepth,
                       MainPlayer,
                       ValidMovesRetain,
                       NewMove,
                       NewScore,
                       NewLow,
                       High)
    end.

% can not have do it.already finished or max ply depth.
-spec allow_thinking(list(), integer()) -> true | false.

allow_thinking(_, 0) -> false;
allow_thinking(GameState, _) ->
    MainPlayerWin = is_win(GameState,
                           get_main_player_side()),
    OpponentPlayerWin = is_win(GameState,
                               get_opponent_player_side()),
    EmptyPositionCount = get_null_position_count(GameState),
    case {MainPlayerWin,
          OpponentPlayerWin,
          EmptyPositionCount}
        of
        % either one is winning
        {true, false, _} -> false;
        {false, true, _} -> false;
        {_, _, 0} -> false;
        _ -> true
    end.

get_valid_move(GameState) ->
    [Position
     || #game_state_record{position = Position,
                           state = State}
            <- GameState,
        State == null].

% evaluate_score().
% return player score
-spec evaluate_score(list(), map()) -> integer().

evaluate_score(GameState, Player) ->
    Side = Player#player_record.side,
    WinPlayer = is_win(GameState, Side),
    WinOpponentPlayer = is_win(GameState,
                               get_opponent_side(Side)),
    % many empty state is short circuit
    case {WinPlayer, WinOpponentPlayer} of
        {true, false} ->
            (?WIN_SCORE) + get_null_position_count(GameState);
        {false, true} ->
            (?LOSE_SCORE) - get_null_position_count(GameState);
        _ -> calculate_draw_score(GameState, Side)
    end.

need_exchange_score(null, _) -> true;
need_exchange_score(BestScore, OpponentScore) ->
    BestScore < OpponentScore * -1.

-spec calculate_draw_score(list(),
                           main | opponent) -> integer().

calculate_draw_score(GameState, Side) ->
    PlayerValidLine = calculate_valid_line(GameState,
                                           get_lines(),
                                           Side),
    OpponentValidLine = calculate_valid_line(GameState,
                                             get_lines(),
                                             get_opponent_side(Side)),
    PlayerValidLine - OpponentValidLine.

-spec calculate_valid_line(list(), list(),
                           main | opponent) -> integer().

calculate_valid_line(_, [], _) -> 0;
calculate_valid_line(GameState, Lines, PlayerSide) ->
    [Line | LineRetain] = Lines,
    ValidPositionCount = count_valid_position(GameState,
                                              Line,
                                              PlayerSide),
    LineState = case ValidPositionCount of
                    3 -> 1;
                    _ -> 0
                end,
    LineState +
        calculate_valid_line(GameState, LineRetain, PlayerSide).

-spec count_valid_position(list(), list(),
                           main | opponent) -> integer().

count_valid_position(GameState, Line, PlayerSide) ->
    PositionState = get_position_state(GameState, Line),
    ValidPosition = [X
                     || X <- PositionState,
                        X /= get_opponent_side(PlayerSide)],
    length(ValidPosition).

get_lines() ->
    [% vertical line
     [{1, 1}, {1, 2}, {1, 3}],
     [{2, 1}, {2, 2}, {2, 3}],
     [{3, 1}, {3, 2}, {3, 3}],
     % horizontal line
     [{1, 1}, {2, 1}, {3, 1}],
     [{1, 2}, {2, 2}, {3, 2}],
     [{1, 3}, {2, 3}, {3, 3}],
     % diagonal line
     [{1, 1}, {2, 2}, {3, 3}],
     [{3, 1}, {2, 2}, {1, 3}]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% game state function
-spec is_win(list(), main | opponent) -> true | false.

is_win(GameState, PlayerSide) ->
    WinningLines = get_winning_lines(),
    is_win(GameState, PlayerSide, WinningLines).

is_win(_, _, []) -> false;
is_win(GameState, PlayerSide, WinningLines) ->
    [WinningLine | WinningLineRetain] = WinningLines,
    StateCount = count_line_state(GameState,
                                  PlayerSide,
                                  WinningLine),
    case StateCount of
        3 -> true;
        _ -> is_win(GameState, PlayerSide, WinningLineRetain)
    end.

get_null_position_count(GameState) ->
    length([Position
            || #game_state_record{position = Position,
                                  state = State}
                   <- GameState,
               State == null]).

count_line_state(_, _, []) -> 0;
count_line_state(GameState, PlayerSide, Line) ->
    [Position | Retain] = Line,
    State = lists:keyfind(Position,
                          #game_state_record.position,
                          GameState),
    case State#game_state_record.state of
        PlayerSide -> 1;
        _ -> 0
    end
        + count_line_state(GameState, PlayerSide, Retain).

get_winning_lines() ->
    [% vertical line
     [{1, 1}, {1, 2}, {1, 3}],
     [{2, 1}, {2, 2}, {2, 3}],
     [{3, 1}, {3, 2}, {3, 3}],
     % horizontal line
     [{1, 1}, {2, 1}, {3, 1}],
     [{1, 2}, {2, 2}, {3, 2}],
     [{1, 3}, {2, 3}, {3, 3}],
     % diagonal line
     [{1, 1}, {2, 2}, {3, 3}],
     [{3, 1}, {2, 2}, {1, 3}]].

set_move(GameState, Player, Move) ->
    PositionState = lists:keyfind(Move,
                                  #game_state_record.position,
                                  GameState),
    [PositionState#game_state_record{state =
                                         Player#player_record.side}
     | lists:keydelete(Move,
                       #game_state_record.position,
                       GameState)].

% get_position_state()
% return a state list at specified positions.
-spec get_position_state(list(), list()) -> list().

get_position_state(_, []) -> [];
get_position_state(GameState, Positions) ->
    [Position | Retain] = Positions,
    State = lists:keyfind(Position,
                          #game_state_record.position,
                          GameState),
    [State#game_state_record.state] ++
        get_position_state(GameState, Retain).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% player function
generate_main_player() ->
    #player_record{side = get_main_player_side()}.

get_main_player_side() -> main.

get_opponent_player_side() -> opponent.

get_opponent_side(main) -> get_opponent_player_side();
get_opponent_side(opponent) -> get_main_player_side().

get_opponent_player(Player) ->
    #player_record{side =
                       get_opponent_side(Player#player_record.side)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test function
generate_game_state(1) ->
    [#game_state_record{position = {1, 1}, state = null},
     #game_state_record{position = {1, 2}, state = null},
     #game_state_record{position = {1, 3}, state = null},
     #game_state_record{position = {2, 1}, state = null},
     #game_state_record{position = {2, 2}, state = null},
     #game_state_record{position = {2, 3}, state = null},
     #game_state_record{position = {3, 1}, state = null},
     #game_state_record{position = {3, 2}, state = null},
     #game_state_record{position = {3, 3}, state = null}];
% |   |   | o |
% |   | x | x |
% |   |   | o |
generate_game_state(2) ->
    [#game_state_record{position = {1, 1}, state = null},
     #game_state_record{position = {1, 2}, state = null},
     #game_state_record{position = {1, 3}, state = opponent},
     #game_state_record{position = {2, 1}, state = null},
     #game_state_record{position = {2, 2}, state = main},
     #game_state_record{position = {2, 3}, state = main},
     #game_state_record{position = {3, 1}, state = null},
     #game_state_record{position = {3, 2}, state = null},
     #game_state_record{position = {3, 3},
                        state = opponent}];
% | o |   | X |
% |   | o | o |
% | x |   | x |
generate_game_state(3) ->
    [#game_state_record{position = {1, 1}, state = main},
     #game_state_record{position = {1, 2}, state = null},
     #game_state_record{position = {1, 3}, state = opponent},
     #game_state_record{position = {2, 1}, state = null},
     #game_state_record{position = {2, 2}, state = main},
     #game_state_record{position = {2, 3}, state = main},
     #game_state_record{position = {3, 1}, state = opponent},
     #game_state_record{position = {3, 2}, state = null},
     #game_state_record{position = {3, 3},
                        state = opponent}];
% | o | o | X |
% | x |   | o |
% | x | x | o |
generate_game_state(4) ->
    [#game_state_record{position = {1, 1}, state = main},
     #game_state_record{position = {1, 2}, state = main},
     #game_state_record{position = {1, 3}, state = opponent},
     #game_state_record{position = {2, 1}, state = opponent},
     #game_state_record{position = {2, 2}, state = null},
     #game_state_record{position = {2, 3}, state = main},
     #game_state_record{position = {3, 1}, state = opponent},
     #game_state_record{position = {3, 2}, state = opponent},
     #game_state_record{position = {3, 3}, state = main}];
% | o | o | X |
% |   | o | o |
% | x |   | x |
generate_game_state(5) ->
    [#game_state_record{position = {1, 1}, state = main},
     #game_state_record{position = {1, 2}, state = main},
     #game_state_record{position = {1, 3}, state = opponent},
     #game_state_record{position = {2, 1}, state = null},
     #game_state_record{position = {2, 2}, state = main},
     #game_state_record{position = {2, 3}, state = main},
     #game_state_record{position = {3, 1}, state = opponent},
     #game_state_record{position = {3, 2}, state = null},
     #game_state_record{position = {3, 3},
                        state = opponent}];
generate_game_state(_) -> [].

generate_expect_move(1) -> {1, 1};
generate_expect_move(2) -> {2, 1};
generate_expect_move(3) -> {2, 1};
generate_expect_move(4) -> {2, 2};
generate_expect_move(5) -> {2, 1};
generate_expect_move(_) -> null.
