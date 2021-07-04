% from -> vertex atom()
% to -> vertex atom()
% flow -> integer()
% cost -> integer()
% capacity -> integer()
-record(edge, {from, to, flow, cost, capacity}).

% direction -> atom: forward | backward
-record(argpath, {vertex, previous, direction}).