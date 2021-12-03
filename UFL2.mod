#######  #######  #######  #######  #######  #######  #######  #######
#######               Uncapacity Facility Location             #######
#######  Fuente:      Integer Programming                	   #######
#######  Autor:       Laurenmce A. Wolsey       			   #######
#######  Programador: Uriel Iram Lezama Lope				   #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				   #######
#######  #######  #######  #######  #######  #######  #######  #######

#######  CONJUNTOS

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set N;          # {i}   CLIENTES
set M;          # {j}   DEPOSITOS

#######  PARAMETROS 

param c { N , M } ;    #  
param f { N } ;        #

#######  VARIABLES

var x { N , M }     >=0;   #
var y { N } binary, >=0;   #

#######  FUNCION OBJETIVO

minimize z :
sum{ i in N } sum{ j in M } ( c[i,j] * x[i,j] ) + sum{ i in N }  ( f[i] * y[i] );

###################  RESTRICCIONES

##  Satisfaction of the demand of client i
subject to demand_client { j in 1..card(M) }: 
sum { i in 1..card(N) } x[ i , j ] >= 1 ;

##  Link between x and y variables
subject to open_depot { i in N , j in M }:
-x[ i , j ]  + y[ i ] >=0;

subject to y1:
y[ 1 ] =  1;
subject to y2:
y[ 2 ] =  0;
subject to y3:
y[ 3 ] =  1;
