#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
## La empresa de electronica ha sacado al mercado su ultimo modelo en radios, del
## que se tienen que entregar 20.000 radios en las proximas 4 semanas. El cliente pagará
## $40 por cada radio que se entregue la primera semana, $36 por cada uno entregado en
## la segunda semana, $32 por cada uno entregado en la tercera semana, y $28 por cada
## radio entregado la altima semana. Si cada trabajador puede ensamblar solo 50 radios
## por semana, entonces la compania no puede afrontar la entrega con su actual plantilla
## de 40 trabajadores expertos fijos, debiendo contratar y entrenar trabajadores nuevos
## temporales para ese mes. Cualquiera de los trabajadores expertos puede dedicarse
## bien a la linea de ensamblaje o bien entrenar durante una semana a un grupo de
## no mas de tres trabajadores nuevos, momento a partir del cual costos ya entrenados
## son considerados como expertos, aunque temporales. Esto es, a partir de su primera
## semana en la compania pueden dedicarse a la linea de ensamblaje o a entrenar a otros
## nuevos, aunque al final de las 4 semanas perderian el trabajo. Teniendo presente que
## la compania no tiene otros contractos actuales en esas mismas 4 semanas, tan pronto
## como se fabriquen las 20.000 radios los empleados no haran nada, aunque sí cobrarán
## en cualquier caso el mes integro. El costo de un empleado fijo es de $2.000 por mes,
## mientras que el de un empleado temporal es la mitad. Ademas se le dara un plus de
## $100 por cada semana en la que un trabajador (fijo o temporal) ensenie a ensamblar.
## Si el costo de produccion de un modelo de radio, a parte de mano de obra, es de $5
## por radio, plantear un modelo para maximizar los beneficios menos los costos de la
## compania.

###########  CONJUNTOS

set SEMANAS;

###########  PARAMETROS

param pr{SEMANAS}   ; ## precio del producto por semana i
param cr{SEMANAS}   ; ## costo de producto para la semana i
param cb  ;           ## costo empleado de base
param ce  ;           ## costo empleado eventual
param cc  ;           ## costo por capacitacion a nuevos empleados

param  d  ;           ## demanda de radios
param  a  ;           ## maxima produccion por empleado a la semana
param  b  ;           ## maxima capacitacion por empleado a la semana
param  f  ;           ## numero de empleados fijos

###########  VARIABLES

 var   x {SEMANAS} >= 0, integer; ## cantidad de radios a producir en la semana i
 var   y {SEMANAS} >= 0, integer; ## cantidad de trabajadores que van a entrenar en la semana i
 var   w {SEMANAS} >= 0, integer; ## cantidad de trabajadores eventuales a contratar en la semana i
 var   s {SEMANAS} >= 0, integer; ## cantidad de trabajadores que van a ensamblar en la semana i
 
###########  FUNCION OBJETIVO

 maximize Z:
 - f * cb + sum{i in SEMANAS}(  pr[i] * x[i] - cr[i] * x[i] - cc * y[i] - ce * w[i] ) ;
 
###########  RESTRICCIONES
 
#Restriccion de cumplimiento de la demanda de los radios
 subject to Rest_Demanda: 
 sum{i in SEMANAS} x[ i ]  = d;

#Restriccion de produccion de radios por empleado
 subject to Rest_Radios_Empleados { i in SEMANAS}: 
 x[ i ] <= a * s[ i ]   ; 
 
 #Restriccion de numero de trabajadores a entrenarse
  subject to Rest_Capacitacion_eventuales { i in SEMANAS : i<>4}: 
  w[ i ] <=  b * y[ i ]      ;
 
 #Restriccion balance de trabajadores semana 1
  subject to Rest_balance_trabajadores1 :
  f - y[ 1 ] = s[ 1 ]; 

 #Restriccion balance de trabajadores semana i = 2...#semanas
  subject to Rest_balance_trabajadores {i in SEMANAS: i<>1 }: #: i<>1 and i<>4
  f - y[ i ] + sum{ tao in 1..i-1 } w[ tao ] = s[ i ];
  
  
  
  
  
  
  
  
  
  
  