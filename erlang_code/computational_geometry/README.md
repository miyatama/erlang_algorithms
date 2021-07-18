# computational geometry

+ convex hull scan
+ crossing line
+ voronoi diagram
+ line sweep

## convex hull scan

 + sort points
 + generate upper convex hull
 + generate lower convex hull
 + join convex hull upper and lower(remove duplicate point)

convex hull is union of triangle right rotate.right rotate is `cp <= 0`.

```math
cp = \begin{vmatrix}
L_{i-1}.x & L_{i-1}.y & 1 \\
L_{i}.x & L_{i} & 1 \\
p.x & p.y & 1
\end{vmatrix} \\
cp = (L_{i}.x - L_{i-1}.x)(p.y - L_{i-1}.y) - (L_{i}.y - L_{i-1}.y)(p.x - L_{i-1}.x)
```

[source code](./erlang_code/computational_geometry/convex_hull_scan.erl)

<details><summary>Convex hull scan</summary><div>

```erlang
-spec convex_hull(list(point)) -> list(point).
convex_hull(Points) when length(Points) < 4 -> Points;
convex_hull(Points) ->
    SortedPoints = sort_points(Points),
    UpperConvexHull = get_upper_convex_hull(SortedPoints),
    output_debug(convex_hull_scan, "Upper Convex Hull: ~w", [UpperConvexHull]),
    LowerConvexHull = get_lower_convex_hull(SortedPoints),
    output_debug(convex_hull_scan, "Lower Convex Hull: ~w", [LowerConvexHull]),
    join_convex_hull(UpperConvexHull, LowerConvexHull).
```

</div></details>