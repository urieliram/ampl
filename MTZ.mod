# AMPL model for the Traveler Salesman Problem
# By default, this model assumes that b[i] = 0, c[i,j] = 0,
# l[i,j] = 0 and u[i,j] = Infinity.
# Parameters not specified in the data file will get their default values.

# AMPL model for the Traveler Salesman Problem using MTZ
# Conjuntos
set NODES;			            	                # nodes in the network
set ARCS within {NODES, NODES}; 	# arcs in the network

# Parametros
param c {ARCS} default Infinity;	# cost of one of flow on arc(i,j)
param p;	
 
# Definicion de las variables
var x {ARCS}, binary >= 0;			# flow on arc (i,j)
var u {NODES} ;			                    # restriccion de orden para romper subciclos

# Objetive function
minimize z: sum{ ( i , j ) in ARCS} c[ i , j ] * x[ i , j ];

# He leaves node i exactly once
subject to leaves{ i in NODES }:
sum{ j in NODES:( i , j ) in ARCS } x[ i , j ] = 1;
 
# He arrives node j exactly once
subject to arrives{ j in NODES }:
sum{ i in NODES:( i , j ) in ARCS } x[ i , j ] = 1;

# Restricciones de sub-tour MTZ
subject to subtours{ i in NODES , j in NODES : i<>j and i>=1 and j>=1}:
u[ i ] - u[ j ] + p * x[ i , j ] <= p - 1;

subject to origen:
u[ 0 ] =  0;

subject to origen2{ i in NODES: i>1 }:
u[ i ] >= 1;