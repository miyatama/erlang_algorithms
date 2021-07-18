-module(convex_hull_scan).

-export([test/0]).

-import(utility, 
    [sort/2,
    filter/3,
    remove_point/2,
    output_debug/3,
    output_info/3,
    output_error/3]).

-include("computational_geometry.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public function
test() -> 
    test(1),
    test(2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions
-spec convex_hull(list(point)) -> list(point).
convex_hull(Points) when length(Points) < 4 -> Points;
convex_hull(Points) ->
    SortedPoints = sort_points(Points),
    UpperConvexHull = get_upper_convex_hull(SortedPoints),
    output_debug(convex_hull_scan, "Upper Convex Hull: ~w", [UpperConvexHull]),
    LowerConvexHull = get_lower_convex_hull(SortedPoints),
    output_debug(convex_hull_scan, "Lower Convex Hull: ~w", [LowerConvexHull]),
    join_convex_hull(UpperConvexHull, LowerConvexHull).

% sort point order by x, y asc
-spec sort_points(list(point)) -> list(point).
sort_points(Points) -> 
    SortedX = sort(Points, fun(Point) -> Point#point.x end),
    sort_xmap_for_y(SortedX).

% map({integer(), list(point)})
-spec sort_xmap_for_y(list(point)) -> list(point).
sort_xmap_for_y(List) -> 
    LastPoint = lists:last(List),
    sort_xmap_for_y(List, 0, LastPoint#point.x).
sort_xmap_for_y(_, BaseValue, MaxValue) when BaseValue > MaxValue -> [];
sort_xmap_for_y(List, BaseValue, MaxValue) -> 
    FilterdPoints = filter(List, fun(Point) -> Point#point.x end, BaseValue),
    sort(FilterdPoints, fun(Point) -> Point#point.y end) ++
        sort_xmap_for_y(List, BaseValue + 1, MaxValue).

-spec get_upper_convex_hull(list(point)) -> list(point).
get_upper_convex_hull(Points) -> 
    [Point1, Point2 | Retain] = Points,
    ConvexHullPoints = [Point1, Point2],
    get_upper_convex_hull(Retain, ConvexHullPoints).

-spec get_upper_convex_hull(list(point), list(point)) -> list(point).
get_upper_convex_hull([], ConvexHullPoints) -> ConvexHullPoints;
get_upper_convex_hull(Points, ConvexHullPoints) ->
    [Point | Retain] = Points,
    ConvexHullPoints2 = ConvexHullPoints ++ [Point],
    ConvexHullPoints3 = remove_last_of_middle_tree(ConvexHullPoints2),
    get_upper_convex_hull(Retain, ConvexHullPoints3).

-spec get_lower_convex_hull(list(point)) -> list(point).
get_lower_convex_hull(Points) -> 
    ReversePoints = lists:reverse(Points),
    get_upper_convex_hull(ReversePoints).

-spec remove_last_of_middle_tree(list(point)) -> list(point).
remove_last_of_middle_tree(ConvexHullPoints) when length(ConvexHullPoints) < 3 -> ConvexHullPoints;
remove_last_of_middle_tree(ConvexHullPoints) ->
    LastPoints = get_last_tree_point(ConvexHullPoints),
    IsLeftRotate = is_left_rotate(LastPoints),
    output_debug(convex_hull_scan, "left rotate is ~w. ~w", [IsLeftRotate, LastPoints]),
    remove_last_of_middle_tree(ConvexHullPoints, IsLeftRotate).

remove_last_of_middle_tree(ConvexHullPoints, false) -> ConvexHullPoints;
remove_last_of_middle_tree(ConvexHullPoints, _) -> 
    DeletePoint = lists:nth(length(ConvexHullPoints) - 1, ConvexHullPoints),
    ConvexHullPoints2 = lists:delete(DeletePoint, ConvexHullPoints),
    remove_last_of_middle_tree(ConvexHullPoints2).

is_left_rotate(Points) ->
    [Point1, Point2, Point3] = Points,
    CP = ((Point2#point.x - Point1#point.x) * (Point3#point.y - Point1#point.y)) - 
        ((Point2#point.y - Point1#point.y) * (Point3#point.x - Point1#point.x)),
    (CP > 0).

get_last_tree_point(Points) ->
    lists:sublist(Points, length(Points) - 2, 3).

-spec join_convex_hull(list(point), list(point)) -> list(point).
join_convex_hull(Points1, Points2) -> 
    RemovedPoints = remove_duplicate_point(Points1, Points2),
    Points1 ++ RemovedPoints.

remove_duplicate_point([], Points2) -> Points2;    
remove_duplicate_point(Points1, Points2) ->
    [Point | Retain] = Points1,
    Removed = remove_point(Point, Points2),
    remove_duplicate_point(Retain, Removed).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% test function
-spec test(integer()) -> list(position).
test(TestCase) ->
    Points = get_test_pointers(TestCase),
    ConvexHullPoints = convex_hull(Points),
    ExpectConvexHullPoins = get_expect_convex_hull_positions(TestCase),
    show_result(TestCase, ExpectConvexHullPoins, ConvexHullPoints),
    ok.

get_test_pointers(1) ->
    [
        #point{x=1, y=1},
        #point{x=1, y=3},
        #point{x=3, y=3},
        #point{x=3, y=1},
        #point{x=2, y=2}
    ];
get_test_pointers(2) ->
    [
        #point{x=1, y=7},
        #point{x=2, y=10},
        #point{x=3, y=7},
        #point{x=4, y=1},
        #point{x=5, y=12},
        #point{x=6, y=9},
        #point{x=6, y=5},
        #point{x=7, y=1},
        #point{x=9, y=4},
        #point{x=8, y=8},
        #point{x=7, y=13},
        #point{x=10, y=11},
        #point{x=11, y=6},
        #point{x=12, y=3}
    ];
get_test_pointers(_) -> [].

get_expect_convex_hull_positions(1) ->
    [
        #point{x=1, y=1},
        #point{x=1, y=3},
        #point{x=3, y=3},
        #point{x=3, y=1}
    ];
get_expect_convex_hull_positions(2) ->
    [
        #point{x=1, y=7},
        #point{x=2, y=10},
        #point{x=5, y=12},
        #point{x=7, y=13},
        #point{x=10, y=11},
        #point{x=12, y=3},
        #point{x=7, y=1},
        #point{x=4, y=1}
    ];
get_expect_convex_hull_positions(_) -> [].

show_result(TestCase, Expect, Result) ->
    case equal_points(Expect, Result) of
        true ->
            output_info(convex_hull_scan, "case ~w: ~w", [TestCase, true]);
        false ->
            output_info(convex_hull_scan, "case ~w: ~w", [TestCase, false]),
            output_info(convex_hull_scan, "expect: ~w", [Expect]),
            output_info(convex_hull_scan, "result: ~w", [Result])
    end,
    ok.

-spec equal_points(list(point), list(point)) -> true | false.
equal_points([], []) -> true;
equal_points(Pointers01, Pointers02) when length(Pointers01) /= length(Pointers02) -> false;
equal_points(Pointers01, Pointers02) ->
    [Point01 | Pointers01Retain] = Pointers01,
    [Point02 | Pointers02Retain] = Pointers02,
    equal_points(equal_point(Point01, Point02), Pointers01Retain, Pointers02Retain).

-spec equal_points(true | false, list(point), list(point)) -> true | false.
equal_points(false, _ , _) -> false;
equal_points(true, Pointers01, Pointers02) ->
    equal_points(Pointers01, Pointers02).

-spec equal_point(point, point) -> true | false.
equal_point(Point1, Point2) ->
    #point{x=X1, y=Y1} = Point1,
    #point{x=X2, y=Y2} = Point2,
    (X1 == X2) and (Y1 == Y2).