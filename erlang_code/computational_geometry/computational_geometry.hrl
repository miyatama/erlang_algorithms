% x: integer(). least 0.
% y: integer(). least 0.
-record(point, {x, y}).

% p1x: point1.x
% p1y: point1.y
% p2x: point2.x
% p2y: point2.y
-record(line, {name, p1x, p1y, p2x, p2y, a, b, c}).