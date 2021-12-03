#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una entidad financiera capta dep�sitos y presta dinero�La captaci�n de dep�sitos lleva una
## hora para convencer al cliente y otra de trabajo burocr�tico. El pr�stamo de dinero lleva una hora para
## convencer al cliente y dos horas de trabajo burocr�tico. El m�ximo n�mero de horas de trabajo disponibles
## es de 40 horas para convencer a los clientes y 60 horas para el trabajo burocr�tico. El beneficio
## obtenido por prestar dinero es 1/3 mayor que el de captar dep�sitos. �Cu�ntas operaciones de cada
## tipo le conviene realizar a la entidad para obtener el m�ximo beneficio?

##                                HORAS REQUERIDAS
## OPERACION                 Convencimiento  Tr�mites  BENEFICIOS
## Captaci�n de dep�sitos    1               1               1 * a
##              Pr�stamos    1               2         (1+1/3) * a
## Disponibilidad de tiempo  40              60

#######  CONJUNTOS

##  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set OPERACION ;         # {i} Tipos de operaci�n
set RECURSOS ;          # {i} Tipos de recursos


#######  PARAMETROS

param beneficios{OPERACION}   >=0 ;            # Beneficio unitario por cada operaci�n
param uso_recursos{OPERACION,RECURSOS}   >=0 ; # Horas requeridas de los recursos por cada operacion
param disponibilidad{RECURSOS} >=0 ;           # Horas totales disponibles de cada recurso
param a;                                       # Factor de escala del beneficio

#######  VARIABLES

## X1 N�mero de operaciones de captaci�n de dep�sitos a realizar en el per�odo
## X2 N�mero de operaciones de Pr�stamos a realizar en el per�odo

var X {OPERACION}    >=0 ;   

#######  FUNCION OBJETIVO 

maximize Beneficios_totales: 
 sum{ i in OPERACION } beneficios[ i ] * X [ i ] * a;

######   RESTRICCIONES

##  Restriccion de disponibilidad de recursos
subject to Res_recursos {j in RECURSOS}:
sum{ i in OPERACION } uso_recursos[ i , j ] * X[ i ] <= disponibilidad[ j ];



