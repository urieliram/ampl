#######  #######  #######  #######  #######  #######  ######
#######  Autor:  Gabriela Renata Huarachi                              #######
#######  Autor:  Julio César Martínez                                      #######
#######  Autor: Uriel Iram Lezama                                          #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  #######  #######  #######  #######  #######  #######

#######  CONJUNTOS
  
 set T ; # Periodos de planeación, indice [ i ].
 set P ; # Tipo de pipas , indice [ j ].
 
 #######  VARIABLES
 
 var x{ T , P } >= 0, integer ; # Numero  de pipas surtidas en el periodo i de pipas de tipo j.
 var y{ T } >= 0 ;                     # Nivel del tanque al final del periodo i , en litros .
 var u{ T } >= 0;                      # Uso de inventario de reserva para satisfacer demanda al final del periodo i, en litros .
 
 #######  PARAMETROS

 param d{ T };         # Demanda del periodo t.
 param s{ T };          # Inventario al final del periodo t.

 param Cmin;			# capacidad maxima del tanque.
 param Cmax;			# capacidad minima del tanque.
 param cp{ P };		# costo unitario del combustible surtido por la pipa de tipo j.
 param cs;				# costo unitario por el uso de la reserva de inventario, en pesos.
 param y0;				# nivel inicial del tanque al inicio del periodo de planeación.
 param pt;				# numero máximo de pipas por turno.
 param p{ P };			# capacidad de la pipa de tipo j.  
 
  ######   FUNCION OBJETIVO  
 minimize Z:
 sum{ i in T } ( sum{ j in P } ( cp[ j ] * x[ i , j ] * p[ j ] )  +  cs * u[ i ]  )  ;
 
######   RESTRICCIONES

# Cotas del tanque 
 subject to limite_min0 : 
  -d[ 1 ] + y0 + sum{ j in P } ( x[ 1 , j ] * p[ j ] ) >= Cmin;

 subject to limite_max0 :
 -d[ 1 ] + y0 + sum{ j in P } ( x[ 1 , j ] * p[ j ]  ) <= Cmax;
 
 subject to limite_min { i in T: i<>1 }:  # para todo i in T
 -d[ i ] + y[ i-1 ] + sum{ j in P } ( x[ i , j ] * p[ j ] ) >= Cmin;
 
 subject to limite_max {i in T: i<>1}:  # para todo i in T
 -d[ i ] + y[ i-1 ] + sum{ j in P } ( x[ i , j ] * p[ j ] ) <= Cmax;
  
# Balance en el tanque
subject to balance_1 :
 - d[ 1 ] + y0       + sum{ j in P } (x[ 1 , j ] * p[ j ] ) = y[ 1 ];    
  
subject to balance_i { i in T :  i<>1 }:  # para todo i in T
 - d[ i ] + y[ i - 1 ] + sum{ j in P } ( x[ i , j ] * p[ j ] ) = y[ i ];  

# Pedidos máximos por turno
subject to pipas_turno {i in T}:  # para todo i in T
sum{ j in P }  x[ i  , j ] <= pt;

# Balance de inventario
 subject to inventario { i in T }:  # para todo i in T
 s[ i ] - y[ i ]  <=   u[ i ] ;