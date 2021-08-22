-module(line_sweep).

-export([test/0]).

-import(utility, 
    [sort/2,
    filter/3,
    remove_point/2,
    equal_points/2,
    remove_lines/2,
    equal_lines/2,
    equal_line/2,
    calculate_coefficient/1,
    calculate_line_x_from_y/2,
    calculate_line_cross_point/2,
    output_lines/2,
    output_debug/3,
    output_info/3,
    output_error/3]).

-include("computational_geometry.hrl").

% type: start_point | end_point | cross_point
-record(event_queue, {type, x, y, upper_lines, lower_lines, cross_lines}).

-record(status, {y, lines}).

-record(cross_point, {x, y, lines}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public function
test() ->
    test(1),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private function
-spec calculate_cross_point(list(line)) -> list(point).
calculate_cross_point(Lines) -> 
    SwappedLines = swap_line_point(Lines),
    AddCoefficientLines = calculate_coefficient(SwappedLines),
    EventQueue = create_event_queue(AddCoefficientLines),
    show_event_queue(EventQueue),
    handle_event(EventQueue).

-spec handle_event(list(event_queue)) -> list(point).
handle_event([]) -> [];
handle_event(EventQueue) -> 
    Status = #status{
        y = 0,
        lines=[]},
    handle_event(EventQueue, Status, []).

-spec handle_event(list(event_queue), status, list(point)) -> list(point).
handle_event([], _, Points) -> Points;
handle_event(EventQueue, Status, Points) -> 
    % output_debug(line_sweep, "handle_event", []),
    show_status(Status),
    % show_cross_points(Points),
    Event = dequeue_event(EventQueue),
    EventQueue2 = remove_event_queue(Event, EventQueue),
    handle_event(EventQueue2, Status, Points, Event, Event#event_queue.type).

handle_event(EventQueue, Status, Points, Event, start_point) -> 
    output_debug(line_sweep, "handle_event - start point", []),
    output_lines(line_sweep, Event#event_queue.upper_lines),
    Status2 = recalculate_status_line(Event, Status),
    % judge left and right lines and intersections
    CrossPoints = calculate_cross_points_addlines(Event#event_queue.upper_lines, Status2),
    CrossPoints2 = remove_duplicate_cross_point(CrossPoints),
    CrossPoints3 = remove_registered_cross_point(CrossPoints2, Points),
    show_cross_points(CrossPoints3),
    Points2 = Points ++ CrossPoints3,
    EventQueue2 = add_cross_points_to_event_queue(
        CrossPoints3,
        EventQueue),
    handle_event(EventQueue2, Status2, Points2);

handle_event(EventQueue, Status, Points, Event, end_point) -> 
    output_debug(line_sweep, "handle_event - end point", []),
    % judge the intersection of left and right lines
    CrossPoints = calculate_cross_point_remove_lines(Event#event_queue.lower_lines, Status),
    CrossPoints2 = remove_duplicate_cross_point(CrossPoints),
    CrossPoints3 = remove_registered_cross_point(CrossPoints2, Points),
    Status2 = recalculate_status_line(Event, Status),
    show_cross_points(CrossPoints3),
    Points2 = Points ++ CrossPoints3,
    EventQueue2 = add_cross_points_to_event_queue(
        CrossPoints3,
        EventQueue),
    handle_event(EventQueue2, Status2, Points2);

handle_event(EventQueue, Status, Points, Event, cross_point) -> 
    output_debug(line_sweep, "handle_event - cross point", []),
    CrossPoints = calculate_cross_point_cross_lines(
        Event#event_queue.cross_lines,
        Status),
    CrossPoints2 = remove_duplicate_cross_point(CrossPoints),
    CrossPoints3 = remove_registered_cross_point(CrossPoints2, Points),
    show_cross_points(CrossPoints3),
    Points2 = Points ++ CrossPoints3,
    Status2 = recalculate_status_line(Event, Status),
    EventQueue2 = add_cross_points_to_event_queue(
        CrossPoints3,
        EventQueue),
    handle_event(EventQueue2, Status2, Points2).

-spec swap_line_point(list(line)) -> list(line).
swap_line_point([]) -> [];
swap_line_point(Lines) ->
    [Line | Retain] = Lines,
    NewLine = case Line#line.p1y < Line#line.p2y of
        true ->
            #line{
                name=Line#line.name, 
                p1x=Line#line.p2x, 
                p1y=Line#line.p2y, 
                p2x=Line#line.p1x, 
                p2y=Line#line.p1y};
        false ->
            Line
    end,
    [NewLine] ++ swap_line_point(Retain).

-spec create_event_queue(list(line)) -> list(event_queue).
create_event_queue(Lines) -> 
    create_event_queue(Lines, []).
-spec create_event_queue(list(line), list(event_queue)) -> list(event_queue).
create_event_queue([], EventQueue) -> EventQueue;
create_event_queue(Lines, EventQueue) -> 
    [Line | Retain] = Lines,
    % output_debug(line_sweep, "create_event_queue - start_point (~w, ~w) - ~w", [Line#line.p1x, Line#line.p1y, Line#line.name]),
    EventQueue2 = add_event_queue(
        start_point, 
        Line#line.p1x, 
        Line#line.p1y, 
        [Line], 
        [], 
        EventQueue),
    % output_debug(line_sweep, "create_event_queue - end_point (~w, ~w) - ~w", [Line#line.p2x, Line#line.p2y, Line#line.name]),
    EventQueue3 = add_event_queue(
        end_point, 
        Line#line.p2x, 
        Line#line.p2y, 
        [], 
        [Line], 
        EventQueue2),
    create_event_queue(Retain, EventQueue3).

%%% event queue function
-spec dequeue_event(list(event_queue)) -> event_queue.
dequeue_event(EventQueue) ->
    Y = get_event_max_y(EventQueue),
    Candidates = [Events || Events <-EventQueue, Events#event_queue.y == Y],
    case length(Candidates) of
        1 -> 
            [Event] = Candidates,
            Event;
        _ ->
            choose_event_with_type(Candidates)
    end.

-spec get_event_max_y(list(event_queue)) -> double.
get_event_max_y(EventQueue) ->
    [Head | Retain] = EventQueue,
    Candidate = [X || X <- Retain, X#event_queue.y >= Head#event_queue.y],
    case length(Candidate) of
        0 -> Head#event_queue.y;
        _ -> get_event_max_y(Retain)
    end.

-spec choose_event_with_type(list(event_queue)) -> event_queue.
choose_event_with_type(Events) ->
    StartEvents = [X || X <- Events, X#event_queue.type == start_point],
    CrossEvents = [X || X <- Events, X#event_queue.type == cross_point],
    EndEvents = [X || X <- Events, X#event_queue.type == end_point],
    choose_event_with_type(StartEvents, CrossEvents, EndEvents).
choose_event_with_type(StartEvents, _, _) when length(StartEvents) > 0 -> 
    [Head | _] = StartEvents,
    Head;
choose_event_with_type(_, CrossPoint, _) when length(CrossPoint) > 0 -> 
    [Head | _] = CrossPoint,
    Head;
choose_event_with_type(_, _, EndEvents) ->
    [Head | _] = EndEvents,
    Head.

-spec remove_event_queue(event_queue, list(event_queue)) -> list(event_queue).
remove_event_queue(Event, EventQueue) ->
    remove_event_queue(
        Event#event_queue.type,
        Event#event_queue.x,
        Event#event_queue.y,
        EventQueue).

-spec remove_event_queue(atom, integer, integer, list(event_queue)) -> list(event_queue).
remove_event_queue(Type, X, Y, EventQueue) -> 
    IsNotRemoveEvent = fun(Event) -> 
        (Event#event_queue.type =/= Type)
        or (Event#event_queue.x =/= X) 
        or (Event#event_queue.y =/= Y) end,
    [Event || Event <- EventQueue, IsNotRemoveEvent(Event)].

-spec add_event_queue(atom, integer, integer, list(line), list(line), list(event_queue)) -> list(event_queue).
add_event_queue(Type, X, Y, UpperLines, LowerLines, EventQueue) ->
    % output_debug(line_sweep, "add_event_queue - ~w, (~w, ~w) to ~w", [Type, X ,Y, EventQueue]),
    Event = find_event_queue(Type, X, Y, EventQueue),
    case Event of
        null -> 
            % output_debug(line_sweep, "add_event_queue ~w, (~w, ~w) - new event", [Type, X ,Y]),
            [#event_queue{
                type=Type, 
                x=X, 
                y=Y, 
                upper_lines=UpperLines, 
                lower_lines=LowerLines,
                cross_lines=[]}] ++ EventQueue;
        _ ->
            % output_debug(line_sweep, "add_event_queue ~w, (~w, ~w) - update event", [Type, X ,Y]),
            [#event_queue{
                type=Type, 
                x=X,
                y=Y,
                upper_lines = Event#event_queue.upper_lines ++ UpperLines,
                lower_lines = Event#event_queue.lower_lines ++ LowerLines,
                cross_lines = []}] ++
            remove_event_queue(Type, X, Y, EventQueue)
    end.

-spec add_cross_points_to_event_queue(list(cross_point), list(event_queue)) -> list(event_queue).
add_cross_points_to_event_queue([], EventQueue) -> EventQueue;
add_cross_points_to_event_queue(CrossPoints, EventQueue) -> 
    [CrossPoint | CrossPointsRetain] = CrossPoints,
    % output_debug(line_sweep, "add_cross_points_to_event_queue - ~w", [CrossPoint#cross_point.lines]),
    [#event_queue{
        type=cross_point,
        x=CrossPoint#cross_point.x,
        y=CrossPoint#cross_point.y,
        upper_lines=[],
        lower_lines=[],
        cross_lines=CrossPoint#cross_point.lines}]
         ++ add_cross_points_to_event_queue(CrossPointsRetain, EventQueue).

-spec find_event_queue(atom, integer, integer, list(event_queue)) -> event_queue | null.
find_event_queue(Type, X, Y, EventQueue) ->
    % output_debug(line_sweep, "find_event_queue - ~w, (~w, ~w), ~w", [Type, X, Y, EventQueue]),
    IsTargetEvent = fun(Event) -> 
        (Event#event_queue.type == Type) 
        and (Event#event_queue.x == X) 
        and (Event#event_queue.y == Y) end,
    Events = [Event || Event <- EventQueue, IsTargetEvent(Event)],
    case length(Events) of
        0 -> null;
        _ -> 
            [Head | _] = Events,
            Head
    end.

-spec show_event_queue(list(event_queue)) -> ok.
show_event_queue([]) -> ok;
show_event_queue(EventQueue) ->
    show_event_queue(EventQueue, 1, length(EventQueue)).
show_event_queue([], _, _) -> ok;
show_event_queue(EventQueue, Index, TotalLength) ->
    Event = dequeue_event(EventQueue),
    % [Event | _] = EventQueue,
    % output_debug(line_sweep, "show_event_queue - Event: ~w", [Event]),
    output_debug(line_sweep, 
        "show_event_queue - Event[~w / ~w]: ~w, (~w, ~w)", 
        [Index, TotalLength, Event#event_queue.type, Event#event_queue.x, Event#event_queue.y]),
    case Event#event_queue.type of
        start_point ->
            output_lines(line_sweep, Event#event_queue.upper_lines);
        end_point ->
            output_lines(line_sweep, Event#event_queue.lower_lines);
        cross_point ->
            output_lines(line_sweep, Event#event_queue.cross_lines)
    end,
    EventQueue2 = remove_event_queue(Event, EventQueue),
    show_event_queue(EventQueue2, Index + 1, TotalLength).

%%% status function
-spec recalculate_status_line(event_queue, status) -> status.
recalculate_status_line(Event, Status) ->
    recalculate_status_line(Event, Status, Event#event_queue.type).
recalculate_status_line(Event, Status, start_point) ->
    recalculate_status_line_add(
        Event#event_queue.upper_lines, 
        Event#event_queue.y,
        Status);
recalculate_status_line(Event, Status, end_point) ->
    recalculate_status_line_remove(Event#event_queue.lower_lines, Status);
recalculate_status_line(Event, Status, cross_point) -> 
    recalculate_status_line_cross(Event#event_queue.cross_lines, Status).

-spec recalculate_status_line_add(list(line),double, status) -> status.
recalculate_status_line_add([], _, Status) -> Status;
recalculate_status_line_add(Lines, Y, Status) -> 
    [Line | LineRetain] = Lines,
    recalculate_status_line_add(LineRetain, Y, Line, Status).

-spec recalculate_status_line_add(list(line), double, line, status) -> status.
recalculate_status_line_add(Lines, Y, Line, Status) -> 
    % output_debug(line_sweep, "recalculate_status_line_add - retain: ~w", [length(Lines)]),
    % output_lines(line_sweep, [Line]),
    % show_status(Status),
    LineX = calculate_line_x_from_y(Y, Line),
    FrontLines = [X || X <- Status#status.lines, (LineX >= calculate_line_x_from_y(Y, X))],
    RearLines = [X || X <- Status#status.lines, (LineX < calculate_line_x_from_y(Y, X))],
    Status2 = Status#status{
        lines=FrontLines ++ [Line] ++ RearLines},
    % show_status(Status2),
    recalculate_status_line_add(Lines, Y, Status2).

% remove line
-spec recalculate_status_line_remove(list(lines), status) -> status.
recalculate_status_line_remove(Lines, Status) ->
    % output_debug(line_sweep, "recalculate_status_line_remove", []),
    % output_lines(line_sweep, Lines),
    Status2 = Status#status{
        lines=remove_lines(
            Lines, 
            Status#status.lines)},
    % output_debug(line_sweep, "recalculate_status_line_remove - line count ~w to ~w", [length(Status#status.lines), length(Status2#status.lines)]),
    Status2.

% Lines length is 2.left line and right line on cross point.
-spec recalculate_status_line_cross(list(lines), status) -> status.
recalculate_status_line_cross(Lines, Status) ->
    [LeftLine, RightLine] = Lines,
    LeftIndex = get_line_index_in_status(LeftLine, Status),
    RightIndex = get_line_index_in_status(RightLine, Status),
    FrontLines = lists:sublist(Status#status.lines, LeftIndex - 1),
    MiddleLines = case (RightIndex- LeftIndex) > 1 of
        true -> lists:sublist(Status#status.lines, LeftIndex + 1, RightIndex - (LeftIndex + 1));
        false -> []
    end,
    RearLines = lists:sublist(
        Status#status.lines, 
        RightIndex + 1, 
        length(Status#status.lines) - RightIndex),
    Status#status{
        lines=FrontLines ++ [RightLine] ++ MiddleLines ++ [LeftLine] ++ RearLines}.

-spec calculate_cross_points_addlines(list(line), stauts) -> list(cross_point).
calculate_cross_points_addlines([], _) -> [];
calculate_cross_points_addlines(Lines, Status) -> 
    [Line | LinesRetain] = Lines,
    % calculate_cross_points_addlines(LinesRetain, Status, Line, (Line#line.a == 0)).
    calculate_cross_points_addlines(LinesRetain, Status, Line, true).
calculate_cross_points_addlines(Lines, Status, Line, true) -> 
    % parallel to the X axis
    calculate_lines_cross_point(Line, Status#status.lines) ++
        calculate_cross_points_addlines(Lines, Status);
calculate_cross_points_addlines(Lines, Status, Line, _) -> 
    Index = get_line_index_in_status(Line, Status),
    LeftLine = get_line_in_status(Index - 1, Status),
    RightLine = get_line_in_status(Index + 1, Status),
    case calculate_line_cross_point(Line, LeftLine) of
        null -> [];
        {X, Y} ->
                [#cross_point{
                    x=X,
                    y=Y,
                    lines=[LeftLine, Line]}]
    end ++ 
    case calculate_line_cross_point(Line, RightLine) of
        null -> [];
        {X, Y} ->
                [#cross_point{
                    x=X,
                    y=Y,
                    lines=[Line, RightLine]}]
    end ++ 
    calculate_cross_points_addlines(Lines, Status).

-spec calculate_cross_point_remove_lines(list(lines), status) -> list(cross_point).
calculate_cross_point_remove_lines([], _) -> [];
calculate_cross_point_remove_lines(Lines, Status) ->
    [Line | LinesRetain] = Lines,
    % output_debug(line_sweep, "calculate_cross_point_remove_lines - remove line ~w", [Line#line.name]),
    % show_status(Status),
    LineIndex = get_line_index_in_status(Line, Status),
    LeftLine = get_line_in_status(LineIndex - 1, Status),
    RightLine = get_line_in_status(LineIndex + 1, Status),
    % output_debug(line_sweep, "calculate_cross_point_remove_lines - left: ~w, right: ~w", [LeftLine, RightLine]),
    CrossPoint = case calculate_line_cross_point(LeftLine, RightLine) of
        null -> [];
        {X, Y} ->
                [#cross_point{
                    x=X,
                    y=Y,
                    lines=[LeftLine, RightLine]}]
    end,
    RemovedLineStasut = Status#status{
        lines=remove_lines([Line], Status#status.lines)},
    CrossPoint ++ calculate_cross_point_remove_lines(LinesRetain, RemovedLineStasut).

-spec calculate_cross_point_cross_lines(list(line), status) -> list(cross_point).
calculate_cross_point_cross_lines(Lines, Status) ->
    [LeftLine, RightLine] = Lines,
    LeftLineIndex = get_line_index_in_status(LeftLine, Status),
    RightLineIndex = get_line_index_in_status(RightLine, Status),
    % output_debug(line_sweep, "calculate_cross_point_cross_lines - line ~w and ~w", [LeftLine#line.name, RightLine#line.name]),
    CrossCandidateLeftLine = get_line_in_status(
        LeftLineIndex - 1,Status),
    CrossCandidateRightLine = get_line_in_status(
        RightLineIndex + 1, Status),
    % output_debug(line_sweep, "calculate_cross_point_cross_lines - right and ~w", [CrossCandidateLeftLine]),
    % output_debug(line_sweep, "calculate_cross_point_cross_lines - left and ~w", [CrossCandidateRightLine]),
    case calculate_line_cross_point(CrossCandidateLeftLine, RightLine) of
        null -> [];
        {X, Y} ->
                [#cross_point{
                    x=X,
                    y=Y,
                    lines=[CrossCandidateLeftLine, RightLine]}]
        end ++
        case calculate_line_cross_point(LeftLine, CrossCandidateRightLine) of
            null -> [];
            {X, Y} ->
                    [#cross_point{
                        x=X,
                        y=Y,
                        lines=[LeftLine, CrossCandidateRightLine]}]
        end.

calculate_lines_cross_point(_, []) -> [];
calculate_lines_cross_point(Line, Lines) ->
    [Head | LinesRetain] = Lines,
    output_debug(line_sweep, "calculate_lines_cross_point", []),
    output_lines(line_sweep, [Line, Head]),
    case calculate_line_cross_point(Line, Head) of
        null -> [];
        {X, Y} ->
            [#cross_point{
                x=X,
                y=Y,
                lines=[Line, Head]}]
    end ++ calculate_lines_cross_point(Line, LinesRetain).

% line exists status#lines
-spec get_line_index_in_status(line, status) -> integer.
get_line_index_in_status(Line, Status) -> 
    get_line_index_in_status(Line, Status, 1).
get_line_index_in_status(Line, Status, Index) -> 
    StatusLine = lists:nth(Index, Status#status.lines),
    case equal_line(Line, StatusLine) of
        true -> Index;
        false -> get_line_index_in_status(Line, Status, Index + 1)
    end.

get_line_in_status(Index, Status) -> 
    InRange = (Index >= 1) and (Index =< length(Status#status.lines)),
    get_line_in_status(Index, Status, InRange).
get_line_in_status(_, _, false) -> null;
get_line_in_status(Index, Status, _) -> 
    lists:nth(Index, Status#status.lines).

%%% cross point function
-spec remove_duplicate_cross_point(list(cross_point)) -> list(cross_point).
remove_duplicate_cross_point([]) -> [];
remove_duplicate_cross_point(CrossPoints) ->
    [CrossPoint | CrossPointsRetain] = CrossPoints,
    Duplicates = [X || X <- CrossPointsRetain,
        equal_lines(X#cross_point.lines, CrossPoint#cross_point.lines)],
    case length(Duplicates) of
        0 -> [CrossPoint];
        _ -> []
    end ++ remove_duplicate_cross_point(CrossPointsRetain).

-spec remove_registered_cross_point(list(cross_point), list(cross_point)) -> list(cross_point).
remove_registered_cross_point([], _) -> [];
remove_registered_cross_point(CandidateCrossPoints, CrossPoints) ->
    [CandidateCrossPoint | CandidateCrossPointsRetain] = CandidateCrossPoints,
    case registered_cross_point(CandidateCrossPoint, CrossPoints) of
        true -> [];
        false -> [CandidateCrossPoint]
    end ++ remove_registered_cross_point(CandidateCrossPointsRetain, CrossPoints).

-spec registered_cross_point(cross_point, list(cross_point)) -> true | false.
registered_cross_point(CrossPoint, CrossPoints) ->
    Candidates = [X || X <- CrossPoints, 
        (X#cross_point.x == CrossPoint#cross_point.x) 
        and (X#cross_point.y == CrossPoint#cross_point.y)
        and equal_lines(CrossPoint#cross_point.lines, X#cross_point.lines)],
    registered_cross_point(CrossPoint, CrossPoints, Candidates).
registered_cross_point(_, _, Candidates) when length(Candidates) == 0 -> false;
registered_cross_point(_, _, _) -> true.

show_cross_points(CrossPoints) ->
    show_cross_points(CrossPoints, length(CrossPoints)).
show_cross_points([], _) -> ok;
show_cross_points(CrossPoints, TotalLength) ->
    [Head | Retain]  = CrossPoints,
    output_debug(line_sweep, "cross point[~w] - ~w", [TotalLength - length(Retain), Head#cross_point.lines]),
    show_cross_points(Retain, TotalLength).

%%% status function
show_status(Status) ->
    LineNames = [Line#line.name || Line <-Status#status.lines],
    output_debug(line_sweep, "status lines: ~w", [LineNames]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test function
test(TestCase) ->
    Lines = generate_test_lines(TestCase),
    Result = calculate_cross_point(Lines),
    Expect = generate_expect_cross_point(TestCase),
    show_test_result(TestCase, Expect, Result),
    ok.

show_test_result(TestCase, Expect, Result) ->
    case equal_points(Expect, Result) of
        true ->
            output_info(line_sweep, "case ~w: ~w", [TestCase, pass]);
        false ->
            output_info(line_sweep, "case ~w: ~w", [TestCase, failure]),
            output_info(line_sweep, "Expect", []),
            show_test_result_cross_points(Expect),
            output_info(line_sweep, "Result", []),
            show_test_result_cross_points(Result)
    end,
    ok.

show_test_result_cross_points(CrossPoints) ->
    show_test_result_cross_points(CrossPoints, 1, length(CrossPoints)).
show_test_result_cross_points([], _, _) -> ok;
show_test_result_cross_points(CrossPoints, Index, TotalLength) ->
    [CrossPoint | Retain] = CrossPoints,
    output_info(line_sweep, "[~w / ~w] - line: ~w, point: (~w, ~w)",
        [Index, TotalLength, 
            [X#line.name || X <- CrossPoint#cross_point.lines],
            CrossPoint#cross_point.x, CrossPoint#cross_point.y]),
    show_test_result_cross_points(Retain, Index + 1, TotalLength).

generate_test_lines(1) ->
    [
        #line{name=l1, p1x=5, p1y=24, p2x=5, p2y=6},
        #line{name=l2, p1x=5, p1y=24, p2x=27, p2y=1},
        #line{name=l3, p1x=18, p1y=16, p2x=5, p2y=6},
        #line{name=l4, p1x=18, p1y=16, p2x=31, p2y=2},
        #line{name=l5, p1x=28, p1y=14, p2x=20, p2y=0},
        #line{name=l6, p1x=5, p1y=6, p2x=28, p2y=6}
    ].

generate_expect_cross_point(1) -> [].