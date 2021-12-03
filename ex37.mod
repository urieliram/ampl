#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio 37

## Colpetro procesa petróleo para producir combustible para aviones y aceite de maquina.
## Cuesta 40 u.m. comprar 1000 barriles de petróleo, que luego destilados producen 500 barriles de
## combustible para aviones y 500 barriles de aceite. Lo que se obtiene de la destilación puede ser vendido
## directamente o ser procesado nuevamente con un fraccionador catalítico. Si se vende sin el segundo
## proceso, el combustible para aviones se vende a 60 u.m. por 1000 barriles y el aceite para calentar se
## vende a 40 u.m. por 1000 barriles. Lleva una hora procesar 1000 barriles de combustible para aviones
## en el fraccionador catalítico, y esos 1000 barriles se venden a 130 u.m. El mismo proceso demora 45
## minutos para 1000 barriles de aceite para calentar, y esos 1000 barriles se venden a 90 u.m. Cada día,
## se pueden comprar a lo sumo 20000 barriles de petróleo, y se tienen disponibles ocho horas del
## fraccionador catalítico. Formular un LP que maximice los beneficios de Colpetro.

## Costo de 1000 barriles de petróleo = 40 u.m.

##                                                         Precio de venta
## 1000 barriles  500 barriles de combustible de avión   60 u.m./1000 barriles
## de petróleo    500 barriles de aceite                 40 u.m./l000 barriles
## 
## Tiempo de procesamiento de 1000           1 hora     130 u.m./1000 barriles
## barriles de combustible de avión                               
## Tiempo de procesamiento de 1000         3/4 hora      90 u.m./1000 barriles
##              barriles de aceite 
## Se puede comprar diariamente 20.000 barriles de petróleo
## Se dispone de ocho horas de proceso en el fraccionador catalítico

#######  CONJUNTOS

set PRODUCTO1 	; 
set PRODUCTO2 	; 

set RECURSOS1   ;
set RECURSOS2   ;
 
#######  PARAMETROS

 param c          ;                # Beneficio por producto No refinado.  
 
 param b1{PRODUCTO1};              # Beneficio por producto No refinado.  
 param b2{PRODUCTO2};              # Beneficio por producto Refinado.  
 
 param a1{PRODUCTO1,RECURSOS1};    # Cantidad de recurso necesario por producto No refinado.
 param a2{PRODUCTO2,RECURSOS2};    # Cantidad de recurso necesario por producto Refinado.
 
 param B1{RECURSOS1};              # Cantidad de recursos disponible por producto No refinado.
 param B2{RECURSOS2};              # Cantidad de recursos disponible por producto Refinado.
 
 param barriles  ;                 # Limite de barriles diarios
 
#######  VARIABLES   

 var x{PRODUCTO1} >= 0, integer;  # Miles de barriles No Refinados diariamente
 var y{PRODUCTO2} >= 0, integer;  # Miles de barriles Refinados producidos diariamente
 var XT >= 0          , integer;  # Total de barriles comprados diariamente
 
#######  FUNCION OBJETIVO 

 maximize Z:
sum{i in PRODUCTO1}( (b1[ i ]) * x[ i ] ) + sum{i in PRODUCTO2}( (b2[ i ]) * y[ i ]) - (XT * c) ;
 
######   RESTRICCIONES

subject to Limite_crudo_diario:
sum{i in PRODUCTO1} x[ i ] + sum{i in PRODUCTO2} y[ i ] <= B1['BARRILES'];

subject to Limite_recursos_Norefinados{j in RECURSOS1}:
a1[ 'ACEITE', j ] * x[ 'ACEITE' ] = a1[ 'COMB', j ] * x[ 'COMB' ] ;

subject to Limite_recursos_Refinados{j in RECURSOS2}:
sum{i in PRODUCTO2} a2[ i , j ] * y[ i ] <= B2[ j ];

subject to Barriles_totales:
XT = sum{i in PRODUCTO1} x[ i ]  + sum{i in PRODUCTO2} y[ i ]   