#Densest Subgraph problem: AMPL model http://recherche.enac.fr/~cafieri/slides/poly/TD6.pdf

param n >= 1, integer;
set V := 1..n;
set E within {V,V};

# arc colours
param kmax default 10; # max number of colours
param k <= kmax, >= 0, integer, default 1;
param mu{E} >=0, integer, <= kmax;

# variables
var x{V} binary;
var y{(u,v) in E} >= 0, <= min(max(0, mu[u,v]-k+1), max(0,k-mu[u,v]+1));

# model

maximize densesubgraph : sum{(u,v) in E} y[u,v] - sum{v in V} x[v];

# linearization constraints
subject to lin1 {(u,v) in E} : y[u,v] <= x[u];
subject to lin2 {(u,v) in E} : y[u,v] <= x[v];
subject to lin3 {(u,v) in E} : y[u,v] >= x[u] + x[v] - 1;