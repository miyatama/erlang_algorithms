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

[source code](./convex_hull_scan.erl)

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

## line sweep

straight line through the origin(inclination is m).

```math
y = mx
```

straight line not through the (a, b).

```math
b = ma + k \\
k = b - ma
```

the straight line passing through (a, b) is `y = mx + k`.

```math
y = mx + k \\
k = b - ma \\
y = mx + (b - ma) \\
y - b = m(x - a)
```

line (x1, y1), (x2, y2)

```math
\left\{
\begin{array}{}
ax_1 + by_1 + c = 0 \\
ax_2 + by_2 + c = 0 
\end{array}
\right.
```

inclination is

```math
m = \Delta y - \Delta x \\
m = (y_2 - y_1) / (x_2 - x_1)
```

straight line is

```math
y - y_1 = (\Delta y / \Delta x)(x - x_1) \\
(\Delta y / \Delta x)(x - x_1) = y - y_1 \\
\Delta y (x - x_1) = \Delta x(y - y_1) \\
\Delta yx - \Delta yx_1 = \Delta xy - \Delta xy_1 \\
\Delta yx - \Delta yx_1 - \Delta xy + \Delta xy_1 = 0 \\
x(y_2 - y_1) - x_1(y_2 - y_1) - y(x_2 - x_1) + y_1(x_2 - x_1) = 0 \\
x(y_2 - y_1) - y(x_2 - x_1) - x_1(y_2 - y_1) + y_1(x_2 - x_1) = 0 \\
x(y_2 - y_1) + y(x_1 - x_2) + y_1(x_2 - x_1) - x_1(y_2 - y_1) = 0
```

a, b, c is

```math
a = y_2 - y_1 \\
b = x_1 - x_2 \\
c = y1(x_2 - x_1) - x_1(y_2 - y_1)
```

line cross point

```math
L_1 = (a_1, b_1, c_1) \\
L_2 = (a_2, b_2, c_2) \\
\left\{
\begin{array}{}
a_1x + b_1y + c_1 = 0 \\
a_2x + b_2y + c_2 = 0
\end{array}
\right.
```

```math
x = (b_1c_2 - b_2c_1) / (a_1b_2 - a_2b_1) \\
y = (a_2c_1 - a_1c_2) / (a_1b_2 - a_2b_1)
```

is cross line?

```math
t1 = l.a * x_1 + l.b * y_1 + l.c \\
t2 = l.a * x_2 + l.b * y_2 + l.c \\
(t1 >= 0\ and\ t2 <= 0) \ or \ (t1 <= 0\ and\ t2 >= 0)
```

[source code](./line_sweep)