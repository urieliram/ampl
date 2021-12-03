#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS

set PRODUCTOS;
set RECURSOS;

#######  PARAMETROS

param Beneficios {PRODUCTOS};
param CostosPorProducto {RECURSOS, PRODUCTOS};
param TotalCostos {RECURSOS};

#######  VARIABLES

var X {PRODUCTOS} >= 0; # Cantidades de un producto

#######  FUNCION OBJETIVO 
maximize Ganancias:
    sum{i in PRODUCTOS} Beneficios[i] * X[i]
;

######   RESTRICCIONES
subject to Limit {i in RECURSOS}:
    sum{j in PRODUCTOS} CostosPorProducto[i,j] * X[j] <= TotalCostos[i]
;