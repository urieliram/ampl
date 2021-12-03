#AMPL formulations of the set-covering problem

param n > 0;
param c {1..n} >= 0;

set U;
set S {1..n} within U;
var x {1..n} binary;

minimize cost:
sum {j in 1..n} c[j] * x[j];

subject to complete {i in U}:
sum {j in 1..n: i in S[j]} x[j] >= 1;