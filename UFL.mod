#######  #######  #######  #######  #######  #######  #######  #######
#######               Uncapacity Facility Location             #######
#######  Fuente:      Integer Programming                	   #######
#######  Autor:       Laurenmce A. Wolsey       			   #######
#######  Programador: Uriel Iram Lezama Lope				   #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				   #######
#######  #######  #######  #######  #######  #######  #######  #######

#######  CONJUNTOS

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set M;          # {i}   CLIENTES
set N;          # {j}   DEPOSITOS

#######  PARAMETROS 

param c { M , N } ;    #  
param f { N } ;        #

#######  VARIABLES

var x { M , N }  >=0; 	   #
var y { N } binary, >=0;   #

#######  FUNCION OBJETIVO

minimize z :
sum{ i in M } sum{ j in N} (c[i,j] * x[i,j]) + sum{ j in N }  (f[j] * y[j]);

###################  RESTRICCIONES 

##  Satisfaction of the demand of client i
subject to demand { i in 1..card(M) }: 
sum { j in 1..card(N) } x[ i , j ] = 1 ;

##  Link between x and y variables
subject to fixedcost { j in N }:
sum { i in M } x[ i , j ] <= card(M) * y[j];
