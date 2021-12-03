#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una entidad financiera capta depósitos y presta dinero»La captación de depósitos lleva una
## hora para convencer al cliente y otra de trabajo burocrático. El préstamo de dinero lleva una hora para
## convencer al cliente y dos horas de trabajo burocrático. El máximo número de horas de trabajo disponibles
## es de 40 horas para convencer a los clientes y 60 horas para el trabajo burocrático. El beneficio
## obtenido por prestar dinero es 1/3 mayor que el de captar depósitos. ¿Cuántas operaciones de cada
## tipo le conviene realizar a la entidad para obtener el máximo beneficio?

##                                HORAS REQUERIDAS
## OPERACION                 Convencimiento  Trámites  BENEFICIOS
## Captación de depósitos    1               1               1 * a
##              Préstamos    1               2         (1+1/3) * a
## Disponibilidad de tiempo  40              60

#######  CONJUNTOS

##  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set OPERACION ;         # {i} Tipos de operación
set RECURSOS ;          # {i} Tipos de recursos


#######  PARAMETROS

param beneficios{OPERACION}   >=0 ;            # Beneficio unitario por cada operación
param uso_recursos{OPERACION,RECURSOS}   >=0 ; # Horas requeridas de los recursos por cada operacion
param disponibilidad{RECURSOS} >=0 ;           # Horas totales disponibles de cada recurso
param a;                                       # Factor de escala del beneficio

#######  VARIABLES

## X1 Número de operaciones de captación de depósitos a realizar en el período
## X2 Número de operaciones de Préstamos a realizar en el período

var X {OPERACION}    >=0 ;   

#######  FUNCION OBJETIVO 

maximize Beneficios_totales: 
 sum{ i in OPERACION } beneficios[ i ] * X [ i ] * a;

######   RESTRICCIONES

##  Restriccion de disponibilidad de recursos
subject to Res_recursos {j in RECURSOS}:
sum{ i in OPERACION } uso_recursos[ i , j ] * X[ i ] <= disponibilidad[ j ];



