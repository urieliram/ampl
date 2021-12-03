#######  #######  #######  #######  #######  #######  #######  #######
#######  Programadores: Gabriela Renata                                                  #######
#######  Programadores: Julio César                                                           #######
#######  Programadores: Uriel Iram                                                            #######
#######  Posgrado en Ing Sistemas, PISIS, UANL		                     	        #######
#######  #######  #######  #######  #######  #######  #######  #######

# Basado en el artículo: "Project scheduling with resource constraints a  branch and 
# bound approach", European Journal of Operational Research 29 (1987) 262-273, 
#North-Holland, autores: Nicos CHRISTOFIDES, R.. ALVAREZ-VALDES, J.M. TAMARIT.

###########  CONJUNTOS
set S;     #  Actividades en proceso indice s.
set T;     #  Conjunto de días de la programación, indice t.
set H;    #  Conjunto de pares de actividades con restricciones de precedencia, indice i , j.
set K;     #  Conjunto de recursos requeridos por la actividad i , indice k.
  
###########  PARAMETROS
param d[ S ],     >=0 ;        #  tiempo de duración de la actividad i, en horas.
param r[ S , K ], >=0 ;        # cantidad de recurso k requerido por la actividad i.
param b[ K ],     >=0 ;       # disponibilidad total del recurso k.

###########  VARIABLES
var t[ i ], integer, >= 0 ;    # tiempo de inicio de la actividad i,  i = 1, . . . , n, en horas.
 
###########  FUNCION OBJETIVO
minimize Z:
sum { i in T } ( t[ i ] ) ;
 
###########  RESTRICCIONES

#Duración de las actividades
subject to duracion{ ( i , j ) in H }: 
 t[ j ] - t[ i ]>= d[ i ];
  
# Recursos limitados
subject to recursos{ t in T, k in K } :   
sum { i in S } r[ i , k ] <= b[ k ];