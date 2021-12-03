#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Se está programando la producción de un producto para cada una de las próximas cuatro
## semanas. El costo de la producción de una unidad es de 100 u.m. para las dos primeras semanas y 150
## u.m. para las dos últimas. Las demandas son de 70, 80, 90 y 100 unidades semanales y tienen que ser
## satisfechas. La planta puede producir un máximo de 90 unidades; además se pueden emplear horas
## extra durante la tercera y cuarta semana. Esto incrementa la producción semanal en 20 unidades pero
## el costo de producción también sube en 58 u.m. por unidad producida en horas extra. El exceso de
## producción puede ser almacenado a un costo unitario de 3 u.m. por semana. ¿Cómo programar la
## producción de tal manera que minimice los costos totales? Formular el modelo.

##             COSTO DE PRODUCCION
## Semana   Producción    Producción   Demandas     Costo de     Capacidad
##            normal        extra                  almacenaje    producción
##      1      100            -           70           3             90
##      2      100            -           80           3             90
##      3      150           208          90           3            110
##      4      150           208         100           3            110

## VARIABLES
## xi Número de unidades producidas en tiempo normal en la semana i = 1, 2, 3, 4
## yi Número de unidades producidas en tiempo extra en la semana j = 1,2 ,3, 4
## si Inventario final para el período i = 1, 2, 3, 4

###########  CONJUNTOS

set SEMANAS;

###########  PARAMETROS

param d{SEMANAS}  ; ## demanda por semana

param s0   ; ## inventario_inicial

param c1{SEMANAS}  ; ## costo de produccion normal
param c2{SEMANAS}  ; ## costo de produccion extra
param c3{SEMANAS}  ; ## costo de inventario

param maxB{SEMANAS}   ; ## maxima producción costo base
param maxE{SEMANAS}   ; ## maxima producción costo horas extra

###########  VARIABLES

 var   x {SEMANAS} >= 0, integer;
 var   y {SEMANAS} >= 0, integer;
 var   s {SEMANAS} >= 0, integer;

###########  FUNCION OBJETIVO

 minimize Z:
 sum{i in SEMANAS}(  x[i] * c1[i] + y[i] * c2[i] + s[i] * c3[i] ) ;

###########  RESTRICCIONES
 
 #Restriccion de balance trimestre 1
  subject to Rest_Balance0 {i in SEMANAS:i=1}: #i=1
  s0 + x[ i ] + y[ i ] - d[ i ] = s[ i ];
 
 #Restriccion de balance por trimestre
  subject to Rest_Balance {i in SEMANAS:i>1}: #::i>1
  s[ i - 1 ] + x[ i ] + y[ i ] - d[ i ] = s[ i ];
 
 #Restriccion de maximo de pedidos de producción normal
  subject to Rest_Normal {i in SEMANAS}:
  x[ i ] <= maxB[ i ]; 
 
 #Restriccion de maximo de pedidos de producción extra
  subject to Rest_Extra {i in SEMANAS}:
  y[ i ] <= maxE[ i ];