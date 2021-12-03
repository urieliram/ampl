# AMPL model for the Traveler Salesman Problem
# By default, this model assumes that b[i] = 0, c[i,j] = 0,
# l[i,j] = 0 and u[i,j] = Infinity.
# Parameters not specified in the data file will get their default values.

# AMPL model for the Traveler Salesman Problem
# Conjuntos
set NODES;			            	                # nodes in the network
set ARCS within {NODES, NODES}; 	# arcs in the network

# Parametros
param c {ARCS} default Infinity;	# cost of one of flow on arc(i,j)

# Definicion de las variables
var x {ARCS}, binary >= 0;			# flow on arc (i,j)

# Objetive function
minimize z: sum{(i,j) in ARCS} c[i,j] * x[i,j];

# He leaves node i exactly once
subject to leaves{i in NODES}:
sum{ j in NODES:(i,j) in ARCS } x[i,j] = 1;
 
# He arrives node j exactly once
subject to arrives{j in NODES}:
sum{ i in NODES:(i,j) in ARCS } x[i,j] = 1;

# Se agregan las nuevas restricciones al problema
#subject to subtour{k in 1..nCICLO}:
#sum {(i,j) in SUB16 } x[i,j] <= card(SUB16);

#set SUBTOUR default {ARCS};  #arcs in cycles or subtours
#set T default SUBTOUR;       #arcs in cycles or subtours

# Conjuntos subtours TSP
#set SUB16  within {NODES, NODES}; 	# cycle in the network
#set SUB24  within {NODES, NODES}; 	# cycle in the network
#set SUB35  within {NODES, NODES}; 	# cycle in the network
#set SUB163 within {NODES, NODES}; 	# cycle in the network
#set SUB245 within {NODES, NODES}; 	# cycle in the network

# Restriccion de sub-ciclos I
#subject to subtour:
#x[1,6] + x[6,1] <= 1;

#subject to subtour24:
#x[2,4] + x[4,2] <= 1;

#subject to subtour35:
#x[3,5] + x[5,3] <= 1;

# Restriccion de sub-ciclos II
#subject to subtour163:
#x[1,6] + x[6,3] + x[3,1] <= 2;

#subject to subtour245:
#x[2,4] + x[4,5] + x[5,2] <= 2;