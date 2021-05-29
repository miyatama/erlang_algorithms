-module(minimax).

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
            true -> io:fwrite("[DEBUG] minimax: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_DEBUG(S, Args),
        case ?LOG_LEVEL =< 1 of
            true -> io:fwrite("[DEBUG] minimax: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_INFO(S),
        case ?LOG_LEVEL =< 2 of
            true -> io:fwrite("[INFO] minimax: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_INFO(S, Args),
        case ?LOG_LEVEL =< 2 of
            true -> io:fwrite("[INFO] minimax: " ++ S ++ "~n", Args);
            _ -> ok
        end).

-define(OUTPUT_ERROR(S),
        case ?LOG_LEVEL =< 3 of
            true -> io:fwrite("[ERROR] minimax: " ++ S ++ "~n");
            _ -> ok
        end).

-define(OUTPUT_ERROR(S, Args),
        case ?LOG_LEVEL =< 3 of
            true -> io:fwrite("[ERROR] minimax: " ++ S ++ "~n", Args);
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
    get_position_state_test(),
    evaluate_score_test(),
    get_null_positions_test(),
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
    minimax(State, PlyDepth, MainPlayer, null, null).

% minimax(GameState, PlyDepth, MainPlayer) -> {Move, Score}.
-spec minimax(list(), integer(), map(), map(), integer()) -> {map(), integer()}.
minimax(GameState, PlyDepth, MainPlayer, Move, Score)  ->
    case is_leaf_scene(GameState, PlyDepth) of
        true ->
            NewScore = evaluate_score(GameState, MainPlayer),
            ?OUTPUT_DEBUG(
                "minimax - depth: ~w, side: ~w,move: ~w, score: ~w to ~w", 
                [PlyDepth,
                    MainPlayer#player_record.side,
                    Move,
                    Score,
                    NewScore]),
            show_state_list(GameState),
            {Move, NewScore};
        false ->
            EmptyPositions = get_null_positions(GameState),
            minimax(
                GameState, 
                PlyDepth, 
                MainPlayer, 
                EmptyPositions , 
                null, 
                get_side_min_score(MainPlayer#player_record.side))
    end.

-spec minimax(list(), integer(), map(), list(), map(), integer()) -> {map(), integer()}.
minimax(_, _, _, [], Move, Score) -> 
    {Move, Score};
minimax(GameState, PlyDepth, MainPlayer, EmptyPositions, Move, Score) ->
    [EmptyPosition | EmptyPositionRetain] = EmptyPositions,
    ?OUTPUT_DEBUG(
        "side: ~w, empty position: ~w.",
        [MainPlayer#player_record.side,
         EmptyPosition]),
    GameStateAfterPlayer = set_game_state(GameState, MainPlayer, EmptyPosition),
    {_, OpponentScore} = minimax(
        GameStateAfterPlayer, 
        PlyDepth - 1, 
        get_opponent_player(MainPlayer), 
        Move, 
        Score),
    show_state_list(GameStateAfterPlayer),
    ?OUTPUT_DEBUG(
        "after minimax - side: ~w, score: ~w to ~w.",
        [MainPlayer#player_record.side,
        Score,
        OpponentScore]),
    NeedExchangeMove = need_exchange_move(
        get_opponent_player(MainPlayer),
        Move, 
        Score, 
        OpponentScore),
    {NewMove, NewScore} = case NeedExchangeMove of
        true -> 
            {EmptyPosition, OpponentScore};
        _ -> {Move, Score}
    end,
    ?OUTPUT_DEBUG(
        "minimax - depth: ~w, side: ~w,move: ~w to ~w, score: ~w to ~w", 
        [PlyDepth,
            MainPlayer#player_record.side,
            Move,
            NewMove,
            Score,
            NewScore]),
    minimax(GameState, PlyDepth, MainPlayer, EmptyPositionRetain, NewMove, NewScore).

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

-spec need_exchange_move(map(), map() | null, integer(), integer()) -> true | false.
need_exchange_move(_, null, _, _) -> true;
need_exchange_move(Player, _, Score1, Score2) ->
    case {is_main_player(Player), compare_score(Score1,Score2)} of
        {true, lower_than} -> true;    
        {false, greater_than} -> true;
        _ -> false
    end.

compare_score(Score1, Score1) -> equal_to;
compare_score(Score1, Score2) when Score1 > Score2 -> greater_than;
compare_score(_, _) -> lower_than.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GameState
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

get_null_positions(GameState) ->
    [Position || #game_state_record{position=Position, state=State} <- GameState, State == null].

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

show_state_list(GameState) ->
    Positions = [{1,1}, {1,2}, {1,3},
        {2,1}, {2,2}, {2,3},
        {3,1}, {3,2}, {3,3}
    ],
    StateList = show_state_list(GameState, Positions),
    ?OUTPUT_DEBUG("~n~w~n~w~n~w", 
        [
            lists:sublist(StateList, 1, 3),
            lists:sublist(StateList, 4, 3),
            lists:sublist(StateList, 7, 3)
        ]).
show_state_list([], _) -> [];
show_state_list(_, []) -> [];
show_state_list(GameState, Positions) ->
    [Position|PositionRetain] = Positions,
    PositionState = lists:keyfind(Position,
                        #game_state_record.position,
                        GameState),
    [PositionState#game_state_record.state] ++
    show_state_list(lists:keydelete(Position,
                            #game_state_record.position,
                            GameState),
                            PositionRetain).

get_side_min_score(main) ->    ?LOSE_SCORE;
get_side_min_score(opponent) -> ?WIN_SCORE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Player
generate_main_player() -> #player_record{side=main}.

-spec get_opponent(main | opponent) -> main | opponent.
get_opponent(main) -> opponent;
get_opponent(opponent) -> main.

get_opponent_player(Player) ->
    Side = get_opponent(Player#player_record.side),
    #player_record{side=Side}.

is_main_player(Player) ->
    Player#player_record.side == main.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_position_state_test() ->
    GameState1 = [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=main}
    ],
    Positions1 = [
        {1,1}, {1, 2}, {1, 3}
    ],
    Expect1 = [main, main, main],
    Result1 = get_position_state(GameState1, Positions1),
    show_get_position_state_test_result(
        "get_position_state_test case 001 - all state: ~w",
        Expect1,
        Result1),
    GameState2 = [
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,2}, state=opponent},
        #game_state_record{position={1,3}, state=null}
    ],
    Positions2 = [
        {1,2}, {1, 3}
    ],
    Expect2 = [main, null],
    Result2 = get_position_state(GameState2, Positions2),
    show_get_position_state_test_result(
        "get_position_state_test case 002 - choice state: ~w",
        Expect2,
        Result2),
    ok.

show_get_position_state_test_result(Text, Expects, Results) ->
    case list_equal( Expects, Results) of
        true ->
            ?OUTPUT_INFO(Text, [pass]);
        false ->
            ?OUTPUT_INFO(Text, [failure]),
            ?OUTPUT_INFO("Expect: ~w, Reuslt: ~w", [ Expects, Results])
    end.

list_equal(Expects, Results) when length(Expects) /= length(Results) -> false; 
list_equal([], []) -> true; 
list_equal(Expects, Results) when is_list(Expects) -> 
    [Expect|ExpectsRetain] = Expects,
    [Result|ResultsRetain] = Results,
    case Expect of
        Result -> 
            list_equal(ExpectsRetain, ResultsRetain);
        _ -> false
    end.

evaluate_score_test() ->
    % |   |   | x |
    % |   | o | o |
    % |   |   | x |
    GameState1 = [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=null},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=opponent}                   
    ],
    Player1 = #player_record{side=main},
    Expect1 = 0,
    Result1 = evaluate_score(GameState1, Player1),
    show_evaluate_score_test_result(
        "evaluate_score_test case 001 - main user win: ~w",
        Expect1,
        Result1),
    % |   |   | x |
    % |   | o | o |
    % |   |   | x |
    GameState2 = [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=null},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=opponent}                   
    ],
    Player2 = #player_record{side=opponent},
    Expect2 = 0,
    Result2 = evaluate_score(GameState2, Player2),
    show_evaluate_score_test_result(
        "evaluate_score_test case 002 - opponent user lose: ~w",
        Expect2,
        Result2),
    GameState3 = [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=null},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=null},
        #game_state_record{position={2,3}, state=null},
        #game_state_record{position={3,1}, state=null},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=null}                   
    ],
    Player3 = #player_record{side=main},
    Expect3 = 0,
    Result3 = evaluate_score(GameState3, Player3),
    show_evaluate_score_test_result(
        "evaluate_score_test case 003 - initial state: ~w",
        Expect3,
        Result3),
    GameState4 = [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=main},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=null},
        #game_state_record{position={2,3}, state=opponent},
        #game_state_record{position={3,1}, state=null},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=main}                   
    ],
    Player4 = #player_record{side=main},
    Expect4 = 3,
    Result4 = evaluate_score(GameState4, Player4),
    show_evaluate_score_test_result(
        "evaluate_score_test case 004 - draw score: ~w",
        Expect4,
        Result4),
    GameState5 = [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=null},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=null},
        #game_state_record{position={3,3}, state=opponent}
    ],
    Player5 = #player_record{side=main},
    Expect5 = 1,
    Result5 = evaluate_score(GameState5, Player5),
    show_evaluate_score_test_result(
        "evaluate_score_test case 005 - test3 initial score: ~w",
        Expect5,
        Result5),
    GameState6 = [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=opponent},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=opponent},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=main},
        #game_state_record{position={3,3}, state=opponent}
    ],
    Player6 = #player_record{side=main},
    Expect6 = 0,
    Result6 = evaluate_score(GameState6, Player6),
    show_evaluate_score_test_result(
        "evaluate_score_test case 006 - test3 last scene (draw): ~w",
        Expect6,
        Result6),
    GameState7 = [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=main},
        #game_state_record{position={2,2}, state=main},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=main},
        #game_state_record{position={3,3}, state=opponent}
    ],
    Player7 = #player_record{side=main},
    Expect7 = ?WIN_SCORE,
    Result7 = evaluate_score(GameState7, Player7),
    show_evaluate_score_test_result(
        "evaluate_score_test case 007 - test3 last scene (win): ~w",
        Expect7,
        Result7),
    GameState8 = [
            #game_state_record{position={1,1},state=main},
            #game_state_record{position={1,2},state=main},
            #game_state_record{position={1,3},state=opponent},
            #game_state_record{position={2,1},state=main},
            #game_state_record{position={2,2},state=main},
            #game_state_record{position={2,3},state=main},
            #game_state_record{position={3,1},state=opponent},
            #game_state_record{position={3,2},state=opponent},
            #game_state_record{position={3,3},state=opponent}
    ],
    Player8 = #player_record{side=opponent},
    Expect8 = 0,
    Result8 = evaluate_score(GameState8, Player8),
    show_evaluate_score_test_result(
        "evaluate_score_test case 008 - test3 last scene (draw): ~w",
        Expect8,
        Result8),
    GameState9 = [
        #game_state_record{position={1,1}, state=main},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=opponent},
        #game_state_record{position={2,1}, state=opponent},
        #game_state_record{position={2,2}, state=null},
        #game_state_record{position={2,3}, state=main},
        #game_state_record{position={3,1}, state=opponent},
        #game_state_record{position={3,2}, state=opponent},
        #game_state_record{position={3,3}, state=main}
    ],
    Player9 = #player_record{side=opponent},
    Expect9 = 0,
    Result9 = evaluate_score(GameState9, Player9),
    show_evaluate_score_test_result(
        "evaluate_score_test case 009 - test4 (draw): ~w",
        Expect9,
        Result9),
    ok.

show_evaluate_score_test_result(Text, Expect, Result) ->
    case Expect of
        Result -> 
            ?OUTPUT_INFO(Text, [pass]);
        _ ->
            ?OUTPUT_INFO(Text, [failure]),
            ?OUTPUT_INFO("Expect: ~w, Result: ~w", [Expect, Result])
    end.

get_null_positions_test() ->
    GameState1 = [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=null},
        #game_state_record{position={1,3}, state=null}
    ],
    Expect1 = [{1,1}, {1,2}, {1,3}],
    Result1 = get_null_positions(GameState1),
    show_get_null_positions_test_result(
        "get_null_positions_test case 001 - all null: ~w",
        Expect1,
        Result1),
    GameState2 = [
        #game_state_record{position={1,1}, state=null},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=null}
    ],
    Expect2 = [{1,1}, {1,3}],
    Result2 = get_null_positions(GameState2),
    show_get_null_positions_test_result(
        "get_null_positions_test case 002 - choice: ~w",
        Expect2,
        Result2),
    GameState3 = [
        #game_state_record{position={1,1}, state=opponent},
        #game_state_record{position={1,2}, state=main},
        #game_state_record{position={1,3}, state=main}
    ],
    Expect3 = [],
    Result3 = get_null_positions(GameState3),
    show_get_null_positions_test_result(
        "get_null_positions_test case 003 - not exists: ~w",
        Expect3,
        Result3),
    ok.

show_get_null_positions_test_result(Text, Expects, Results) ->
    case list_equal( Expects, Results) of
        true ->
            ?OUTPUT_INFO(Text, [pass]);
        false ->
            ?OUTPUT_INFO(Text, [failure]),
            ?OUTPUT_INFO("Expect: ~w, Reuslt: ~w", [ Expects, Results])
    end.
