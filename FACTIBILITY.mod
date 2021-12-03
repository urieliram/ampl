# AMPL model for the Shortest Path Problem
# AMPL https://s2.smu.edu/~olinick/emis8374/handouts/amplmcnf.html
# Proporciona una solución inicial factible, la más cara

set NODES;			            	# nodes in the network
set ARCS within {NODES, NODES}; 	# arcs in the network

param b {NODES} default 0;		    # supply/demand for node i
param c {ARCS} default 0;	     	# cost of one of flow on arc(i,j)
param l {ARCS} default 0;           # lower bound on flow on arc(i,j)
param u {ARCS} default Infinity;	# upper bound on flow on arc(i,j)
var x {ARCS};				        # flow on arc (i,j)
var f;				                # flow on arc (i,j)

maximize z: sum{(i,j) in ARCS} c[i,j] * x[i,j];

# Flow Out(i) - Flow In(i) = b(i)
subject to pi {i in NODES}:
sum{j in NODES: (i,j) in ARCS} x[i,j] - sum{j in NODES: (j,i) in ARCS} x[j,i] = b[i];

subject to alfa{(i,j) in ARCS}: x[i,j] <= u[i,j];

subject to beta{(i,j) in ARCS}: l[i,j] <= x[i,j] ;