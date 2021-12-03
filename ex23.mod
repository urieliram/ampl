#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 23

# Un carpintero tiene que construir mesas rectangulares de tal manera que las dimensiones no
# sobrepasen 2 m y la suma de su dimension mayor y el doble de la menor no sobrepase 4 m. ¿Cuál es
# el máximo valor del perímetro de dichas mesas?

#######  CONJUNTOS

set LADOS;
set REQUERIMIENTOS;

#######  PARAMETROS
param numero{LADOS} >= 0, integer ;

param proporciones{LADOS,REQUERIMIENTOS} >=0;
param total{REQUERIMIENTOS} >=0;

param maximalongitud{LADOS} >= 0 ; # Lado mas largo de la mesa

#######  VARIABLES

#Definicion de variables
var X{LADOS} >= 0; # Dimensiones de la mesa


#######  FUNCION OBJETIVO 
# Funcion objetivo
  maximize PERIMETRO:
sum{i in LADOS} numero[i] * X[i];

######   RESTRICCIONES
# Tamanio maximo de los lados
  subject to maximo_dimensiones {i in LADOS}:
X[i] <= maximalongitud[i];

# proporciones para los LADOS
  subject to req {j in REQUERIMIENTOS}:
sum{i in LADOS} proporciones[i,j] * X[i] <= total[j];