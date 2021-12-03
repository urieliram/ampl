set NUTR ordered;
set FOOD ordered;

param cost {FOOD} > 0;
param f_min {FOOD} >= 0;
param f_max {j in FOOD} >= f_min[j];

param n_min {NUTR} >= 0;
param n_max {i in NUTR} >= n_min[i];

param amt {NUTR,FOOD} >= 0;

var Buy {j in FOOD} >= f_min[j], <= f_max[j];

minimize Total_Cost:  sum {j in FOOD} cost[j] * Buy[j];

subject to Diet_Req {i in NUTR}:
   n_min[i] <= sum {j in FOOD} amt[i,j] * Buy[j] <= n_max[i];

# ------------------------------------------------------------

set COLS ordered := FOOD union NUTR;
set BASIS ordered by COLS;

param MATR {i in NUTR, j in COLS} := 
   if j in FOOD then amt[i,j] else (if j = i then -1 else 0);

var Tabl {i in BASIS, j in COLS};

subj to Tabl_Eqns {i in NUTR, j in COLS}:
   sum {k in BASIS} MATR[i,k] * Tabl[k,j] = MATR[i,j];