#######   #######  #######  #######  #######  #######
#######      Multiple traveling salesman problem     #######
#######     Programador: Uriel Iram Lezama Lope    #######
#######    Posgrado en Ing Sistemas, PISIS, UANL	    #######
#######   #######  #######  #######  #######  #######

#######  CONJUNTOS
#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.
set N;                                # {i} {j}   Número de Ciudades

#######  PARAMETROS 
param c { N , N } ;  #  costos de ciudad i a la ciudad j
param m ;              #  Agentes
param p ;               #  p denotes the maximum number of nodes that can be visited by any salesman
param T ;               # T := n/m max {cij}

#######  VARIABLES
var x { N , N } binary; 	      #1  Si se usa el arco , cero si no
var u { N }  >=0;   #

#######  FUNCION OBJETIVO
minimize mtsp :
sum{ i in N } sum{ j in N } ( c[ i , j ] * x[ i , j ] );

minimize mlp :
sum{ i in N }  u[ i ] ;

###################  NODO DEPOSITO 
subject to depo1: 
sum { j in N:j<>1 } x[1 , j ] = m ;

subject to depo2 : 
sum { j in N:j<>1 } x[ j ,1 ] = m ;

###################  ASIGNACIÓN 
subject to outdegree { j in N: j<>1 }: 
sum { i in N:i<>j } x[ i , j ] = 1 ;

subject to indegree {  i in N: i<>1 }: 
sum { j in N:i<>j } x[ i , j ] = 1 ;

###################  DANTZING 
subject to dzn3267: 
 x[3,2] +  x[2,6] + x[6,7]+ x[7,3]  <= 3 ;
 
###################  MILLER     ..............¡falta corregir!
subject to mtz {i in N , j in N: (j<>i and i<>1 and j<>1) }: 
u[ i ] - u[ j ] + p * x[ i , j ] <= p - 1 ;

###################  BEKTAS 2006 ............... ¡falta corregir!
subject to bek1 { i in N , j in N: j<>i and i<>1 and j<>1}: 
u[ i ] - u[ j ] + ( T + c[ i , j ]) * x[ i , j ] + ( T - c[ j , i ]) * x[ j , i ]<= T ;

subject to bek2 { i in N: i<>1 }: 
u[ i ] >= c[ 1 , i ] * x[ 1 , i ] ;






