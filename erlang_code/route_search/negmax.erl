-module(negmax).

-export([test/0]).

% side = main | opponent
% rf(player_record).
% rd(player_record, {side}).
-record(player_record, {side}).

% pos -> {X:integer(), Y:integer()}, state -> main | opponent | null
% rf(game_state_record).
% rd(game_state_record, {position, state}).
-record(game_state_record,{position, state}).

% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 2).

-define(OUTPUT_DEBUG(S),
        case ?LOG_LEVEL =< 1 of
            true -> io:fwrite("[DEBUG] negmax: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_DEBUG(S, Args),
        case ?LOG_LEVEL =< 1 of
            true -> io:fwrite("[DEBUG] negmax: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_INFO(S),
        case ?LOG_LEVEL =< 2 of
            true -> io:fwrite("[INFO] negmax: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_INFO(S, Args),
        case ?LOG_LEVEL =< 2 of
            true -> io:fwrite("[INFO] negmax: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_ERROR(S),
        case ?LOG_LEVEL =< 3 of
            true -> io:fwrite("[ERROR] negmax: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_ERROR(S, Args),
        case ?LOG_LEVEL =< 3 of
            true -> io:fwrite("[ERROR] negmax: " ++ S ++ "~n", Args);
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
            ?OUTPUT_INFO(
                "test case ~w: ~w,score is ~w.", 
                [CaseNo, pass, Score]);
        _ ->
            ?OUTPUT_INFO(
                "test case ~w: ~w, Expect: ~w, Result: ~w, score is ~w.", 
                [CaseNo, failure, Expect, Move, Score])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
best_move(State, MainPlayer) ->
    PlyDepth = ?MAX_PLYDEPTH,
    negmax(State, PlyDepth, MainPlayer, null, null).

% negmax(GameState, PlyDepth, MainPlayer, Move, Score) -> {Move, Score}.
-spec negmax(list(), integer(), map(), map(), integer()) -> {map(), integer()}.
negmax(GameState, PlyDepth, MainPlayer, _, _) -> 
    case is_leaf_scene(GameState, PlyDepth) of
        true -> 
            Score = evaluate_score(GameState, MainPlayer),
            ?OUTPUT_DEBUG(
                "negmax - arrival leaf. score is ~w. side is ~w.",
                [Score, MainPlayer#player_record.side]),
            {null, Score};
        false -> 
            EmptyPositions = get_null_positions(GameState),
            negmax(
                GameState, 
                PlyDepth, 
                MainPlayer, 
                EmptyPositions , 
                null,
                null)

    end.

negmax(_, _, _, [], Move, Score) -> {Move, Score};
negmax(GameState, PlyDepth, MainPlayer, EmptyPositions, Move, Score) ->
    [EmptyPosition | EmptyPositionRetain] = EmptyPositions,
    GameStateAfterPlayer = set_game_state(GameState, MainPlayer, EmptyPosition),
    {_, OpponentScore} = negmax(
        GameStateAfterPlayer, 
        PlyDepth - 1, 
        get_opponent_player(MainPlayer), 
        Move, 
        Score),
    NeedExchangeMove = need_exchange_move(
        Score, 
        OpponentScore),
    {NewMove, NewScore} = case NeedExchangeMove of
        true -> 
            case PlyDepth of
                ?MAX_PLYDEPTH ->
                    ?OUTPUT_DEBUG(
                        "negmax - depth: ~w, side: ~w,move: ~w to ~w, score: ~w to ~w", 
                        [PlyDepth,
                            MainPlayer#player_record.side,
                            Move,
                            EmptyPosition,
                            Score,
                            OpponentScore]);
                _ -> ok
            end,
            {EmptyPosition, (-1 * OpponentScore)};
        _ -> {Move, Score}
    end,
    negmax(GameState, PlyDepth, MainPlayer, EmptyPositionRetain, NewMove, NewScore). 

% 打てる手が無い or 最大思考深度に達した
-spec is_leaf_scene(list(), integer()) -> true | false.
is_leaf_scene(_, PlyDepth) when PlyDepth == 0 -> true;
is_leaf_scene(GameState, PlyDepth) ->
    is_leaf_scene(GameState, PlyDepth, is_win_game(GameState, main)).
is_leaf_scene(_, _, win) -> true;
is_leaf_scene(_, _, lose) -> true;
is_leaf_scene(GameState, _, _) ->
    case get_null_positions(GameState) of
        [] -> true;
        _ -> false 
    end.

% evaluate_score().
% return player score
-spec evaluate_score(list(), map()) -> integer().
evaluate_score(GameState, Player) -> 
    Side = Player#player_record.side,
    % many empty state is short circuit
    case is_win_game(GameState, Side) of
        win -> ?WIN_SCORE + length(get_null_positions(GameState));
        lose -> ?LOSE_SCORE - length(get_null_positions(GameState));
        _ -> calculate_draw_score(GameState, Side)
    end. 

-spec need_exchange_move(integer() | nulll, integer()) -> true | false.
need_exchange_move(null, _) -> true;
need_exchange_move(OldScore, NewScore) ->
    ((-1 * NewScore) > OldScore).

-spec is_win_game(list(), main | opponent) -> win | lose | draw.
is_win_game(GameState, PlayerSide) ->
    Positions = get_lines(),
    PlayerWinState = is_win_game(GameState, PlayerSide, Positions),
    OpponentWinState = is_win_game(GameState, get_opponent(PlayerSide), Positions),
    case {PlayerWinState, OpponentWinState} of
        {win, win} -> draw;
        {win, _} -> win;
        {_, win} -> lose;
        _ -> draw
    end.

-spec is_win_game(list(), main | opponent, list()) -> win | no_win.
is_win_game(_, _, []) -> draw;
is_win_game(GameState, PlayerSide, Positions) when is_list(Positions) ->
    [Position|Retain] = Positions,
    PlayerWinGameResult = is_player_win_game(GameState, PlayerSide, Position),
    case PlayerWinGameResult of
        win -> win;
        _ -> is_win_game(GameState, PlayerSide, Retain)
    end.

-spec is_player_win_game(list(), main | opponent, list()) -> win | not_win.
is_player_win_game(GameState, PlayerSide, Position)  ->
    PositionState = get_position_state(GameState, Position),
    PlayerCount = count_player_side(PositionState, PlayerSide),
    OpponentCount = count_player_side(PositionState, get_opponent(PlayerSide)),
    case {PlayerCount, OpponentCount} of
        % もう勝ってる
        {3, 0} -> win;
        % 分からん
        _ -> not_win
    end.

-spec calculate_draw_score(list(), main | opponent) -> integer().
calculate_draw_score(GameState, Side) -> 
    PlayerValidLine = calculate_valid_line(GameState, get_lines(), Side),
    OpponentValidLine = calculate_valid_line(GameState, get_lines(), get_opponent(Side)),
    PlayerValidLine - OpponentValidLine.

-spec calculate_valid_line(list(), list(), main | opponent) -> integer().
calculate_valid_line(_, [], _) -> 0;
calculate_valid_line(GameState, Lines, PlayerSide) ->
    [Line|LineRetain] = Lines,
    ValidPositionCount = count_valid_position(GameState, Line, PlayerSide),
    LineState = case  ValidPositionCount of
        3 -> 1;
        _ -> 0
    end,
    LineState + calculate_valid_line(GameState, LineRetain, PlayerSide).

-spec count_valid_position(list(), list(), main | opponent) -> integer().
count_valid_position(GameState, Line, PlayerSide) ->
    PositionState = get_position_state(GameState, Line),
    ValidPosition = [X || X <- PositionState, X /= get_opponent(PlayerSide)],
    length(ValidPosition).

-spec count_player_side(list(), main | opponent) -> integer().
count_player_side(StateList, PlayerSide) ->
    length([X || X <- StateList, X == PlayerSide]).

get_null_positions(GameState) ->
    [Position || #game_state_record{position=Position, state=State} <- GameState, State == null].

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

set_game_state(GameState, Player, Position) ->
    PositionState = lists:keyfind(Position,
                        #game_state_record.position,
                        GameState),
    case PositionState#game_state_record.state of
        null ->
            [
            PositionState#game_state_record{state = Player#player_record.side} 
            | lists:keydelete(Position,
                            #game_state_record.position,
                            GameState)];
        _ ->
            ?OUTPUT_ERROR("set_game_state - ~w", [replace_not_null]),
            [
            PositionState#game_state_record{state = Player#player_record.side} 
            | lists:keydelete(Position,
                            #game_state_record.position,
                            GameState)]
    end.

generate_game_state(1) ->
    [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=null},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=null},
        #game_state_record{position={2,3}, state=null},
        #game_state_record{position={3,1}, state=null},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=null}
    ];

% |   |   | o |
% |   | x | x |
% |   |   | o |
generate_game_state(2) ->
    [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=null},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=opponent}
    ];

% | o |   | X |
% |   | o | o |
% | x |   | x |
generate_game_state(3) ->
    [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=opponent}
    ];

% | o | o | X |
% | x |   | o |
% | x | x | o |
generate_game_state(4) ->
    [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=opponent},
        #game_state_record{position={2,2}, state=null},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=opponent},
        #game_state_record{position={3,3}, state=main}
    ];

% | o | o | X |
% |   | o | o |
% | x |   | x |
generate_game_state(5) ->
    [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=opponent}
    ];
generate_game_state(_) -> [].

generate_expect_move(1) -> {1,1};
generate_expect_move(2) -> {2,1};
generate_expect_move(3) -> {2,1};
generate_expect_move(4) -> {2,2};
generate_expect_move(5) -> {2,1};
generate_expect_move(_) -> null.

get_lines() ->
    [
        % vertical line
        [{1,1},{1,2},{1,3}],
        [{2,1},{2,2},{2,3}],
        [{3,1},{3,2},{3,3}],
        % horizontal line
        [{1,1},{2,1},{3,1}],
        [{1,2},{2,2},{3,2}],
        [{1,3},{2,3},{3,3}],
        % diagonal line
        [{1,1},{2,2},{3,3}],
        [{3,1},{2,2},{1,3}]
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Player
generate_main_player() -> #player_record{side=main}.

-spec get_opponent(main | opponent) -> main | opponent.
get_opponent(main) -> opponent;
get_opponent(opponent) -> main.

get_opponent_player(Player) ->
    Side = get_opponent(Player#player_record.side),
    #player_record{side=Side}.
