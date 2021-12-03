# AMPL model for the Traveler Salesman Problem
# By default, this model assumes that b[i] = 0, c[i,j] = 0,
# l[i,j] = 0 and u[i,j] = Infinity.
# Parameters not specified in the data file will get their default values.

# AMPL model for the Traveler Salesman Problem using GG Gavish Graves & Graves 
# Conjuntos
set NODES;			            	                # nodes in the network
set ARCS within {NODES, NODES}; 	# arcs in the network

# Parametros
param c {ARCS} default Infinity;	# cost of one of flow on arc(i,j)
param p;	
 
# Definicion de las variables
var x {ARCS} , binary >= 0;		   # flow on arc (i,j)
var g {ARCS} >= 0 ;			           # restriccion de flujo para romper subtours

# Objetive function
minimize z: sum{ ( i , j ) in ARCS} c[ i , j ] * x[ i , j ];

# He leaves node i exactly once
subject to leaves{ i in NODES }:
sum{ j in NODES:( i , j ) in ARCS } x[ i , j ] = 1;
 
# He arrives node j exactly once
subject to arrives{ j in NODES }:
sum{ i in NODES:( i , j ) in ARCS } x[ i , j ] = 1;

# Restricciones de sub-tour GG
subject to Gavish{ i in NODES : i >= 2 }:
sum{ j in NODES:( i , j ) in ARCS and j >= 1 } g[ j , i ] - sum{ j in NODES:( i , j ) in ARCS and j >= 2 } g[ i , j ] = 1;

# Restricciones de sub-tour GG
subject to Graves{ i in NODES, j in NODES : i >= 1 and j >= 2 and i<>j}:
g[ i , j ] <= ( p - 1 ) * x[ i , j ] ;
