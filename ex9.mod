#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una empresa tiene dos factorías A y B. En ellas se fabrica un determinado producto, a razón
## de 500 y 400 unidades por día respectivamente. El producto ha de ser distribuido posteriormente a
## tres centros I, II y III, que requieren, respectivamente, 200, 300 y 400 unidades. Los costos de
## transportar cada unidad del producto desde cada factoría a cada centro distribuidor son los indicados
## en la tabla siguiente:
## Factoría      I    II   III  Fabricación
## (unidades)
##           A   50   60   10   500  u
##           B   25   40   20   400  u
## Demanda       200  300  400
## ¿De qué manera deben organizar el transporte a fin de que los gastos sean mínimos?

## VARIABLES
## Xij Cantidad de producto a enviar desde la factoría i (i = A, B) hasta el centro j (j = 1, 2, 3)

#######  CONJUNTOS

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set FACTORIAS ;        # {i}   Factorias Productoras 
set CENTROS;           # {j}   Centros de Distribucion.

#######  PARAMETROS

param C{FACTORIAS,CENTROS} >=0;  	  # Costo de transporte de la PLANTAS i al CEDIS j
param oferta{FACTORIAS}    >=0 ;  	  # Oferta de la planta i
param demanda{CENTROS}     >=0 ;  	  # Demanda del CEDIS j

#######  VARIABLES
# X : Cantidad de camiones enviados desde la planta de producción i hasta el centro de consumo j

var X {FACTORIAS,CENTROS} >=0 ;    # Cantidad de unidades enviadas desde la planta de producción 
                                   # i (i = 1, 2, 3) hasta el centro de distrubución j (j = 1, 2, 3, 4)

#######  FUNCION OBJETIVO 

minimize Z: 
sum{i in FACTORIAS} sum{j in CENTROS} C[i,j] * X[i,j];

######   RESTRICCIONES

##  Restriccion de oferta 
subject to Res_Produccion {i in FACTORIAS}:
sum{ j in CENTROS} X[i,j] <= oferta[i];

##  Restriccion de demanda 
subject to Res_Oferta {j in CENTROS}:
sum{ i in FACTORIAS} X[i,j] >= demanda[j];