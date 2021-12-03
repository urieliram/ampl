#######   #######  #######  #######  #######  #######
#######        Multiple Minimun Latecy  Problem       #######
#######                        Multilevel method                  #######
#######     Programador: Uriel Iram Lezama Lope    #######
#######    Posgrado en Ing Sistemas, PISIS, UANL	    #######
#######   #######  #######  #######  #######  #######

#######  CONJUNTOS
#  Nombre de Conjunto  # {indice} Descripcion del conjunto.
set N;                                # {i}   Ciudades
set n;                                # {i}    Replica de Ciudades

#######  PARAMETROS 
param c { N , N } ;  #  costos de ciudad i a la ciudad j
param m ;              #  Agentes

#######  VARIABLES
var x { N , N } binary; 	      # 1  Si se usa el arco , cero si no
var y { N , N , N} binary; 	  # 1  Si se usa el arco del nodo j al nodo i  en el nivel r, cero si no

#######  FUNCION OBJETIVO
minimize mmlp :
 sum{ j in n: j<>0 } c[ 0 , j ]  * ( sum{r in N: r<>0 } r * y[ 0 , j , r ] ) + 
 sum{ i in n: i<>0 } sum{ j in n: i<>j and i<>0 } c[ i , j ]  *
  sum{ r in 1..(card(N)-2) } r * y[ i , j , r ]    ;

###################  un nodo activo i por nivel r
subject to multi30 { i in n: i<>0 }: 
sum { r in N: r<>0 } x[ i , r ] = 1 ;

###################  every path has assigned at least one node of I 
subject to multi31 : 
sum { i in n: i<>0 } x[ i , 1 ] = m ;

###################  ensures that there are exactly m paths
subject to multi32 :
sum { r in N: r<>0 } sum { j in n: j<>0 } y[ 0 , j , r ] = m ;

###################  outdegree, from level r +1 can leave arcs from the nodes that are active at that level
subject to multi33 { i in 1..card(n)-1 , r in 1..(card(N)-2) }: 
sum { j in n: j<>0 and j<>i } y[ i , j , r ] = x[ i , r+1 ] ;

###################  indegree, impose that at level r can arrive arcs to nodes that are active at that level
subject to multi34 { j in 1..card(n)-1 , r in 1..(card(N)-2) } : 
y[ 0 , j , r ] + sum { i in n: i<>0 and j<>i} y[ i , j , r ] = x[ j , r ] ;

###################  force node 0 at level N + 1 to be connected to these nodes.
subject to multi35 { j in 1..card(n)-1} : 
y[ 0 , j , card(N)-1 ] = x[ j , card(N)-1 ] ;









