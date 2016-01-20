
array[int] of int: durations;
int: n = length(durations);  % number of tracks
set of int: TRACKS = 1..n;

int: k;   % number of groups
set of int: GROUPS = 1..k;
array[TRACKS] of var GROUPS: groups;

% each group sums its durations
int: MIN_SUM = min(durations);
int: MAX_SUM = sum(durations);
set of int: SUM = MIN_SUM..MAX_SUM;
array[GROUPS] of var SUM: sums; % no group sums to zero

% the goal
var int: obj;

%-------------------------------------------------

include "globals.mzn";

% splits are increasing, at least one apart
constraint increasing(groups);

% collects durations into group sums
constraint forall(i in GROUPS) (
  sums[i] = sum([durations[j] * bool2int(groups[j] == i) | j in TRACKS])
);

% only max
%constraint obj = max(sums);

% both max and min
constraint obj = max(sums) - min(sums);

%-------------------------------------------------

solve :: int_search(groups, dom_w_deg, indomain_median, complete)
  minimize obj;

output [
  "durations = ", show(durations), "\n",
  "groups    = ", show(groups), "\n",
  "sums      = ", show(sums), " -- ", show(min(sums)), "-", show(max(sums)), "\n",
  "obj       = ", show(obj), "\n"
];

