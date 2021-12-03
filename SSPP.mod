# AMPL model for the Sucesive Shortest Path Problem 
# to solve MIN COST FLOW

set NODES;			            	# nodes in the network
set ARCS within {NODES, NODES}; 	# arcs in the network

param b {NODES} default 0;		    # supply/demand for node i
param bT {NODES} default 0;		    # supply/demand for node s TO ALL NODES
param c {ARCS} default 0;	     	# cost of one of flow on arc(i,j)
param cpi {ARCS} default 0;	     	# reduce cost of one of flow on arc(i,j)
param u {ARCS} default Infinity;	# upper bound on flow on arc(i,j)
param r {ARCS} default Infinity;	# residual capacity on arc(i,j)
var   x {ARCS}  >=0;				# flow on arc (i,j)
var   xacum {ARCS}  >=0;				# flow on arc (i,j)

param s default 1;
param t default card(NODES);

#minimize z: sum{(i,j) in ARCS} c[i,j] * x[i,j];

# Flow Out(i) - Flow In(i) = b(i)
#subject to bal {i in NODES}:
#sum{j in NODES: (i,j) in ARCS} x[i,j] - sum{j in NODES: (j,i) in ARCS} x[j,i] = b[i]; #all nodes

#subject to alfa {(i,j) in ARCS}: 
#x[i,j] <= u[i,j];


