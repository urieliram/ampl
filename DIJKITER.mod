# AMPL model for the Minimum Cost Network Flow Problem
# AMPL https://s2.smu.edu/~olinick/emis8374/handouts/amplmcnf.html
#
# By default, this model assumes that b[i] = 0, c[i,j] = 0,
# l[i,j] = 0 and u[i,j] = Infinity.
#
# Parameters not specified in the data file will get their default values.


set NODES;			            	# nodes in the network
set ARCS within {NODES, NODES}; 	# arcs in the network

param b {NODES} default 0;		    # supply/demand for node i
param c {ARCS} default 0;	     	# cost of one of flow on arc(i,j)
param l {ARCS} default 0;           # lower bound on flow on arc(i,j)
param u {ARCS} default Infinity;	# upper bound on flow on arc(i,j)
param F {ARCS} default 0;           # 
var x {ARCS};				        # flow on arc (i,j)


param s0 default 1; # nodo inicial s


minimize cost: sum{(i,j) in ARCS} c[i,j] * x[i,j];

# Flow Out(i) - Flow In(i) = b(i)
subject to flow_balance {i in NODES}:
sum{j in NODES: (i,j) in ARCS} x[i,j] - sum{j in NODES: (j,i) in ARCS} x[j,i] = b[i];

subject to capacity {(i,j) in ARCS}: l[i,j] <= x[i,j] <= u[i,j];