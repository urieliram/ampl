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

#######  VARIABLES

var x { M , N }  >=0; 	   #
var y { N } binary, >=0;   #

#######  FUNCION OBJETIVO

minimize z :
sum{ i in M } sum{ j in N } (c[i,j] * x[i,j]);

###################  RESTRICCIONES 
subject to ida { i in 1..card(M) }: 
sum { j in 1..card(N):i<>j } x[ i , j ] = 1 ;

subject to regreso { j in 1..card(N) }: 
sum { i in 1..card(M):i<>j } x[ i , j ] = 1 ;

