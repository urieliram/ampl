#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Supongamos una economía integrada por dos grandes sectores: bienes agrícolas y básicos (BAB),
## y bienes terminados y servicios (BTS), de los que se conocen los coeficientes de insumo-producto:

##      BAB  BTS
## BAB  0.1  0.1
## BTS  0.25 0.25

## También se dispone de los coeficientes de insumo de los factores primarios de mano de obra y capital:

##                 BAB  BTS
## Mano_de_obra  4    3
## Capital       1.5  1.8

## Se ha determinado para estos mismos factores la disponibilidad para el año siguiente. 
## Los resultados de esta proyeccion son:

## Mano_de_obra 2000 unidades
## Capital      600 unidades

## Los precios de mercado para los productos o bienes considerados son, respectivamente, de 1 y 1.5 
## Se trata de determinar de acuerdo con la información anterior, cuál sería la estructura más favorable
## de la demanda final, si se tiene presente una maximización del producto nacional sin variar los recursos
## disponibles.

## VARIABLES DE DECISION:
## X1: Produccion total de los BAB
## X2: Produccion total de los BTS

## MAX Z = X1 + 1.5 X2
## Sujeta a:

##  0.1  X1 + 0.1  X2  < X1
##  0.25 X1 + 0.25 X2  < X2

##  4    X1 + 3    X2  < 2000
##  1.5  X1 + 1.8  X2  < 600

##            X1 , X2  > 0

 set SECTORES ;               
 set PRIMARIOS ;    
 
 param Coef_Insumo_Producto   {SECTORES,SECTORES};  # coeficientes de insumo-producto: 
 param Coef_Insumos_Primarios {SECTORES,PRIMARIOS}; # coeficientes de insumo de los factores primarios de mano de obra y capital:
 param Disp_Primarios         {PRIMARIOS};
 param Precios                {SECTORES};
  
 # Variables del Problema, cantidad a producir de A y B
 var X {SECTORES} >= 0 ;
 
 # Funcion objetivo
 maximize Z:
 sum{i in SECTORES} Precios[i] * X[i];
 
 # Restriccion de Recursos_Insumos_Producto
 subject to Recursos_Insumos_Producto {j in SECTORES}:
 sum{i in SECTORES} Coef_Insumo_Producto[i,j] * X[i] <= X[j]; 
 
 # Restriccion de Recursos_Primarios
 subject to Recursos_Insumos_Primario {i in PRIMARIOS}:
 sum{j in SECTORES} Coef_Insumos_Primarios[j,i] * X[j] <= Disp_Primarios[i];
 
 
 
  
 
 
 
 
 
 
 
 

