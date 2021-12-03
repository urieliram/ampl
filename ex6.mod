#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Imaginemos que las necesidades semanales m�nimas de una persona en prote�nas, hidratos de
## carbono y grasas son, respectivamente, 8, 12 y 9 unidades. Supongamos que debemos obtener un
## preparado con esa composici�n m�nima mezclando dos productos Ay B, cuyos contenidos por kilogramo
## son los que se indican en la siguiente tabla:
##           Prote�nas Hidratos Grasas Costo/kg
##         A 2         6        1      600
##         B 1         1        3      400
## �Cu�ntos kilogramos de cada producto deber�n comprarse semanalmente para que el costo de
## preparar la dieta sea m�nimo?
## Plantear y resolver el anterior problema como un modelo de programaci�n lineal.

#######  CONJUNTOS
set PRODUCTOS;
set NUTRIENTES;

#######  PARAMETROS
param Costos {PRODUCTOS};
param NutrientesPorProducto {NUTRIENTES, PRODUCTOS};
param Necesidades {NUTRIENTES};

#######  VARIABLES#

## XA Cantidad en kg de producto A a utilizar semanalmente
## XB Cantidad en kg de producto B a utilizar semanalmente

var X {PRODUCTOS} >= 0; # Kilogramos de productos a comprar

#######  FUNCION OBJETIVO 
minimize Z:
    sum{i in PRODUCTOS} Costos[i] * X[i];

######   RESTRICCIONES
subject to RestriccionPorNutriente {i in NUTRIENTES}:
    sum{j in PRODUCTOS} NutrientesPorProducto[i,j]*X[j] >= Necesidades[i];