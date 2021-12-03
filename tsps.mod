set NODES ordered;
param hpos {NODES} >= 0;
param vpos {NODES} >= 0;

set PAIRS := {i in NODES, j in NODES: ord(i) < ord(j)};

param dist {(i,j) in PAIRS}
   := sqrt((hpos[j]-hpos[i])**2 + (vpos[j]-vpos[i])**2);

var X {PAIRS} >= 0, <= 1;

minimize Tour_Length: sum {(i,j) in PAIRS} dist[i,j] * X[i,j];

subject to Visit_All {i in NODES}:
   sum {(i,j) in PAIRS} X[i,j] + sum {(j,i) in PAIRS} X[j,i] = 2;

# -------

param nSubtours >= 0 integer;
set SUB {1..nSubtours} within NODES;

subject to Subtour_Elimination {k in 1..nSubtours}:
   sum {i in SUB[k], j in NODES diff SUB[k]} 
      if (i,j) in PAIRS then X[i,j] else X[j,i] >= 2;
