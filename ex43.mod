#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Problema de Transporte
## Cervecer�a El Halc�n produce una marca de cerveza en tres plantas, en tres
## ciudades diferentes. De estas tres plantas, se env�a la cerveza en camiones a cuatro centros de
## distribuci�n; los administradores han comenzado a realizar un estudio para determinar si es posible
## reducir los costos de transporte. Los gerentes de producci�n de las tres plantas han estimado la producci�n
## mensual esperada para sus respectivas plantas.

## Se fabricar� en total en las tres plantas una cantidad suficiente para cargar 300 camiones. El
## gerente general de la cervecer�a ha asignado la producci�n total a los respectivos centros examinando
## datos de meses anteriores. En la tabla se presenta la informaci�n de producci�n y demanda junto con
## los costos de transporte para cada combinaci�n de oferta y demanda. �Cu�ntos camiones de cerveza
## deben enviarse de cada planta a cada centro de distribuci�n para minimizar los costos de transporte?

## DESTINOS CENTROS DE DISTRIBUCION
## ORIGEN 	1 		2		3		4		Produccion
##			(U.M.)	(U.M.)	(U.M.)	(U.M.)	(Oferta)
## Planta 1 4000 	5130 	6500 	8000	75
## Planta 2 3520 	4600 	6900 	7900	125
## Planta 3 9900 	6820 	3880 	6800 	100
## Demanda  80		65 		70 		85 		300

#######  CONJUNTOS

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set PLANTAS ;         # {i}   Productoras de Cerveceria.
set CEDIS;            # {j}   Centros de Distribucion.

#######  PARAMETROS

param C{PLANTAS, CEDIS} >=0;  	  # Costo de transporte de la PLANTAS i al CEDIS j
param oferta{PLANTAS}   >=0 ;  	  # Oferta de la planta i
param demanda{CEDIS}    >=0 ;  	  # Demanda del CEDIS j

#######  VARIABLES
# X : Cantidad de camiones enviados desde la planta de producci�n i hasta el centro
# de consumo j

var X {PLANTAS,CEDIS} >=0 ;    # Cantidad de unidades enviadas desde la planta de producci�n 
                               # i (i = 1, 2, 3) hasta el centro de distrubuci�n j (j = 1, 2, 3, 4)

#######  FUNCION OBJETIVO 

minimize Z: 
 sum{i in PLANTAS} sum{j in CEDIS} C[i,j] * X[i,j];

######   RESTRICCIONES

##  Restriccion de oferta 
subject to Res_Produccion {i in PLANTAS}:
sum{ j in CEDIS} X[i,j] <= oferta[i];


##  Restriccion de demanda 
subject to Res_Oferta {j in CEDIS}:
sum{ i in PLANTAS} X[i,j] >= demanda[j];