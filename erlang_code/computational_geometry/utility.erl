-module(utility).

-export([sort/2,
    filter/3,
    remove_point/2,
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

-spec remove_point(point, list(point)) -> list(point).
remove_point(_, []) -> [];
remove_point(BasePoint, Points) ->
    [Point | Retain] = Points,
    Equals = (BasePoint#point.x == Point#point.x) and (BasePoint#point.y == Point#point.y),
    case Equals of
        true -> [];
        false -> [Point]
    end ++ remove_point(BasePoint, Retain).

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


