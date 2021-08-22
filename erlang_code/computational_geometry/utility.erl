-module(utility).

-export([sort/2,
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

% LOG LEVEL
% 1: DEBUG
% 2: INFO
% 3: ERROR
-define(LOG_LEVEL, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public function
% bucket sort
-spec sort(list(), fun((map()) -> integer())) -> list().
sort(List, GetValue) ->
    Map = push_sort_maps(List, GetValue, maps:new()),
    take_sort_maps(Map, 0).

% list should sorted value ascending
-spec filter(list(), fun((map()) -> integer()), integer()) -> list().
filter([], _, _) -> [];
filter(List, GetValue, FilterValue) -> 
    [Head | Retain] = List,
    Value = GetValue(Head),
    {Continue, FilterdValue} = case compare_integer(Value, FilterValue) of
        equal -> {true, [Head]}; 
        lower_than -> {true, []};
        greater_than -> {false, []}
    end,
    FilterdValue ++ filter(Retain, GetValue, FilterValue, Continue).    

filter(List, GetValue, FilterdValue, true) ->
    filter(List, GetValue, FilterdValue);
filter(_, _, _, false) -> [].

%%% points function
-spec remove_point(point, list(point)) -> list(point).
remove_point(_, []) -> [];
remove_point(BasePoint, Points) ->
    [Point | Retain] = Points,
    Equals = (BasePoint#point.x == Point#point.x) and (BasePoint#point.y == Point#point.y),
    case Equals of
        true -> [];
        false -> [Point]
    end ++ remove_point(BasePoint, Retain).

-spec equal_points(list(point), list(point)) -> true | false.
equal_points(Points1, Points2) when length(Points1) /= length(Points2) -> false;
equal_points(Points1, Points2) -> 
    [Point1 | Points1Retain] = Points1,
    EqualPoint = [#point{x=X, y=Y} || #point{x=X, y=Y} <- Points2 , (X == Point1#point.x) and (Y == Point1#point.y)],
    Points2Retain = remove_point(Point1, Points2),
    equal_points(Points1Retain, Points2Retain, (length(EqualPoint) == 0)).

equal_points(_, _, true) -> false;
equal_points(Points1, Points2, false) -> 
    equal_points(Points1, Points2).

%%% lines function
-spec remove_lines(list(line), list(line)) -> list(line).
remove_lines([], Lines) -> Lines;
remove_lines(RemoveLines, Lines) ->
    [RemoveLine | RemoveLinesRetain] = RemoveLines,
    Lines2 = remove_line(RemoveLine, Lines),
    remove_lines(RemoveLinesRetain, Lines2).
remove_line(RemoveLine, Lines) ->
    [X || X <- Lines, 
        (X#line.p1x /= RemoveLine#line.p1x)
        or (X#line.p1y /= RemoveLine#line.p1y)
        or (X#line.p2x /= RemoveLine#line.p2x)
        or (X#line.p2y /= RemoveLine#line.p2y)].

calculate_coefficient([]) -> [];
calculate_coefficient(Lines) ->
    [Head | Retain] = Lines,
    calculate_coefficient(Head, Retain).
calculate_coefficient(Line, Lines) ->
    A = Line#line.p2y - Line#line.p1y,
    B = Line#line.p1x - Line#line.p2x,
    C = Line#line.p1y * (Line#line.p2x - Line#line.p1x) - 
        Line#line.p1x * (Line#line.p2y - Line#line.p1y),
    [Line#line{a=A, b=B, c=C}] ++ calculate_coefficient(Lines).

-spec calculate_line_x_from_y(double, line) -> double.
calculate_line_x_from_y(Y, Line) ->
    calculate_line_x_from_y(Y, Line, (Line#line.a == 0)).

% inclination of x is 0
calculate_line_x_from_y(Y, _, true) ->Y;
calculate_line_x_from_y(Y, Line, _) ->
    -(Line#line.b * Y / Line#line.a) - (Line#line.c / Line#line.a).

-spec equal_lines(list(line), list(line)) -> true | false.
equal_lines([], []) -> true;
equal_lines(Lines1, Lines2) when length(Lines1) /= length(Lines2) -> false;
equal_lines([], _) -> false;
equal_lines(Lines1, Lines2) ->
    [Line1 | Lines1Retain] = Lines1,
    CandidateLength = length(
        [X || X <- Lines2,
            (Line1#line.p1x == X#line.p1x) and (Line1#line.p1y == X#line.p1y) 
            and (Line1#line.p2x == X#line.p2x) and (Line1#line.p2y == X#line.p2y)]),
    equal_lines(Lines1Retain, Lines2, Line1, CandidateLength).

equal_lines(_, _, _, 0) -> false;
equal_lines(Lines1, Lines2, Line1, _) -> 
    NewLiness2 = remove_lines([Line1], Lines2),
    equal_lines(Lines1, NewLiness2).

-spec equal_line(line, line) -> true | false.
equal_line(Line, Line) -> true;
equal_line(Line1, Line2) -> 
    (Line1#line.p1x == Line2#line.p1x) 
    and (Line1#line.p1y == Line2#line.p1y) 
    and (Line1#line.p2x == Line2#line.p2x) 
    and (Line1#line.p2y == Line2#line.p2y).

-spec exists_cross_point(line, line) -> true | false.
exists_cross_point(null, null) -> false;
exists_cross_point(null, _) -> false;
exists_cross_point(_, null) -> false;
exists_cross_point(Line1, Line2) ->
    exists_cross_point(Line1, Line2, true).
exists_cross_point(Line1, Line2, true) ->
    T1 = Line1#line.a * Line2#line.p1x + Line1#line.b * Line2#line.p1y + Line1#line.c,
    T2 = Line1#line.a * Line2#line.p2x + Line1#line.b * Line2#line.p2y + Line1#line.c,
    ((T1 >= 0) and (T2 =< 0)) or ((T1 =< 0) and (T2 >= 0)) and
        exists_cross_point(Line2, Line1, false);
exists_cross_point(Line1, Line2, false) ->
    T1 = Line1#line.a * Line2#line.p1x + Line1#line.b * Line2#line.p1y + Line1#line.c,
    T2 = Line1#line.a * Line2#line.p2x + Line1#line.b * Line2#line.p2y + Line1#line.c,
    ((T1 >= 0) and (T2 =< 0)) or ((T1 =< 0) and (T2 >= 0)).

-spec calculate_line_cross_point(line, line) -> {x, y} | null.
calculate_line_cross_point(Line, Line) -> null;
calculate_line_cross_point(Line1, Line2) ->
    calculate_line_cross_point(Line1, Line2, exists_cross_point(Line1, Line2)).
calculate_line_cross_point(_, _, false) -> null;
calculate_line_cross_point(Line1, Line2, _) -> 
    X = (Line1#line.b * Line2#line.c - Line2#line.b * Line1#line.c) /
        (Line1#line.a * Line2#line.b - Line2#line.a * Line1#line.b),
    Y = (Line2#line.a * Line1#line.c - Line1#line.a * Line2#line.c) /
        (Line1#line.a * Line2#line.b - Line2#line.a * Line1#line.b),
    {X, Y}.

output_lines(_, []) -> ok;
output_lines(ModuleName, Lines) ->
    [Head | Retain] = Lines,
    output_line(ModuleName, Head),
    output_lines(ModuleName, Retain).

output_line(ModuleName, null) ->
    output_debug(ModuleName, "line is null", []);
output_line(ModuleName, Line) ->
    output_debug(ModuleName, output_line_text(), output_line_param(Line)).

output_line_text() ->
    "line ~w (~w, ~w) to (~w, ~w), a=~w, b=~w, c=~w".
output_line_param(Line) ->
    [Line#line.name, Line#line.p1x, Line#line.p1y, Line#line.p2x, Line#line.p2y, Line#line.a, Line#line.b, Line#line.c].

%%% for debug
output_debug(ModuleName, Text, Param) ->
    output_debug(ModuleName, Text, Param, ?LOG_LEVEL).
output_debug(ModuleName, Text, Param, LogLevel) when LogLevel < 2 ->
    io:fwrite("[DEBUG] ~w: " ++ Text ++ "~n", [ModuleName] ++ Param);
output_debug(_, _, _, _) -> ok.

output_info(ModuleName, Text, Param) ->
    output_info(ModuleName, Text, Param, ?LOG_LEVEL).
output_info(ModuleName, Text, Param, LogLevel) when LogLevel < 3 ->
    io:fwrite("[INFO] ~w: " ++ Text ++ "~n", [ModuleName] ++ Param);
output_info(_, _, _, _) -> ok.

output_error(ModuleName, Text, Param) ->
    output_error(ModuleName, Text, Param, ?LOG_LEVEL).
output_error(ModuleName, Text, Param, LogLevel) when LogLevel < 4 ->
    io:fwrite("[ERROR] ~w: " ++ Text ++ "~n", [ModuleName] ++ Param);
output_error(_, _, _, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private function

push_sort_maps([], _, Map) -> Map;
push_sort_maps(List, GetValue, Map) ->
    [Head | Retain] = List,
    Index = GetValue(Head),
    Array = maps:get(Index, Map, []),
    NewMap = maps:put(
        Index,
        [Head] ++ Array,
        Map),
    push_sort_maps(Retain, GetValue, NewMap).

take_sort_maps(Map, Index) ->
    take_sort_maps(Map, Index, maps:size(Map)).
take_sort_maps(_, _, 0) -> [];
take_sort_maps(Map, Index, _) ->
    {Array, Map2} = case maps:take(Index, Map) of
        {Points, NewMap} -> {Points, NewMap};
        error -> {[], Map}
    end,
    Array ++ take_sort_maps(
        Map2, Index + 1, maps:size(Map2)).

compare_integer(Value1, Value1) -> equal;
compare_integer(Value1, Value2) when Value1 > Value2 -> greater_than;
compare_integer(_, _) -> lower_than.


