#######   #######  #######  #######  #######  #######
#######        Multiple Minimun Latecy  Problem      #######
#######     Programador: Uriel Iram Lezama Lope    #######
#######    Posgrado en Ing Sistemas, PISIS, UANL	    #######
#######   #######  #######  #######  #######  #######

#######  CONJUNTOS
#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.
set N;                                # {i} {j}   Número de Ciudades

#######  PARAMETROS 
param c { N , N } ;  #  costos de ciudad i a la ciudad j
param m ;              #  Agentes
param T ;               # T := n/m max {cij}

#######  VARIABLES
var x { N , N } binary; 	      # 1  Si se usa el arco , cero si no
var u { N }  >=0;                   # longutud del path from del nodo 0 a el nodo i

#######  FUNCION OBJETIVO
minimize mmlp :
sum{ i in N }  u[ i ] ;

###################  NODO DEPOSITO 
subject to depo1: 
sum { j in N: j<>0 } x[0 , j ] = m ;

subject to depo2 : 
sum { j in N: j<>0 } x[ j ,0 ] = m ;

###################  ASIGNACIÓN 
subject to indegree {  i in N: i<>0 }: 
sum { j in N: i<>j } x[ i , j ] = 1 ;

subject to outdegree { j in N: j<>0 }: 
sum { i in N: i<>j } x[ i , j ] = 1 ;

###################  BEKTAS 2006
subject to bektas1 { i in N , j in N: j<>i and i<>0 and j<>0}: 
u[ i ] - u[ j ] + ( T + c[ i , j ]) * x[ i , j ] + ( T - c[ j , i ]) * x[ j , i ]<= T ;

subject to bektas2 { i in N: i<>0 }: 
u[ i ] >= c[ 0 , i ] * x[ 0 , i ] ;