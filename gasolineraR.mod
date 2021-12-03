#######  #######  #######  #######  #######  #######  ######
#######  Autor:  Gabriela Renata Huarachi                              #######
#######  Autor:  Julio César Martínez                                      #######
#######  Autor: Uriel Iram Lezama                                          #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  #######  #######  #######  #######  #######  #######

#######  CONJUNTOS
  
 set T ; # Periodos de planeacion, indice [ i ].
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
 subject to limite_min { i in T }: 
  y[ i ]  >= Cmin ;
 
 subject to limite_max { i in T }:
 y[ i ] <= Cmax ;
 
 # Balance en el tanque
subject to balance_1 :
 - d[ 1 ] + y0       + sum{ j in P } (x[ 1 , j ] * p[ j ] ) = y[ 1 ] ;    
  
subject to balance_i { i in T :  i<>1 }: 
 - d[ i ] + y[ i - 1 ] + sum{ j in P } ( x[ i , j ] * p[ j ] ) = y[ i ] ;  
 
# Balance de inventario
 subject to inventario { i in T }:  
 s[ i ] - y[ i ]  <=   u[ i ] ;
  
# Pedidos máximos por turno
subject to pipas_turno { i in T }: 
sum{ j in P }  x[ i  , j ] <= pt ;
 
# Corte1: La diferencia de nivel entre dos periodos sucesivos debe ser mayor que la demanda - maxima cantidad de litros surtidos.
#subject to corte1 :  
#y0 - y[ 1 ] >= d[ 1 ] - pt *  p[ 2 ] ;
 
#Corte2: La diferencia de nivel entre dos periodos sucesivos debe ser mayor que la demanda - maxima cantidad de litros surtidos.
#subject to corte2 { i in T : i<>1 }: 
#y[ i - 1 ] - y[ i ] >= d[ i ] - pt * p[ 2 ] ;
 
# Corte3 La diferencia entre dos periodos sucesivos no puede ser mayor que la demanda.
#subject to corte3 :  
#y0 - y[ 1 ] <= d[ 1 ] ;
 
# Corte4 La diferencia entre dos periodos sucesivos no puede ser mayor que la demanda.
#subject to corte4 { i in T : i<>1 }: 
#y[ i - 1 ] - y[ i ] <= d[ i ] ;