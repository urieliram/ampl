set N;			            	                     # nodes in the network
set BR within {N, N}; 	                     # branches in the network
set BRindex;

param INYmin{N} default 0;		    # supply/demand for node i
param INYmax{N} default 0;		    # supply/demand for node i

param c {BR}              default 0;	 # cost of one of flow on arc(i,j)
param Fstraight {BR} default 0;     # upper bound on flow on arc(i,j) in direction 'n' to 'm'
param Freverse {BR}  default 0;     # upper bound on flow on arc(i,j)in direction 'm' to 'n'

var ptdf {N,BR};				                     # power tranfer distribution factors in branch br
var iny {N};				                         # injection in node
var f {BR};				                         # flow on arc (i,j)

param NODO0;
param t; 

minimize z: 3;

#subject to cons1{(n,m) in BR}:
#sum{j in N} ptdf[j, (n,m) ]*iny[j] == f[n,m] 

subject to cons2:
sum{n in N} iny[n] == 0

#subject to pis {i in N:i==NODO0}:
#sum{j in N: (i,j) in BR} x[i,j] - sum{j in N: (j,i) in BR} x[j,i] =  f;

#Flow Out(i) - Flow In(i) = b(i)
#subject to pi {i in N:i<>t and i<>s}:
#sum{j in N: (i,j) in BR} x[i,j] - sum{j in N: (j,i) in BR} x[j,i] = b[i]; #=0

#subject to pit{i in N : i==t}:
#sum{j in N: (i,j) in BR} x[i,j] - sum{j in N: (j,i) in BR} x[j,i] = - f;

#subject to alfa{(i,j) in BR}: l[i,j] <= x[i,j] <= u[i,j];