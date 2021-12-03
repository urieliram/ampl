#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 35

#######  CONJUNTOS

#  Nombre de Conjunto # {indice usado en la ecuacion} Descripcion del conjunto.
set MAQUINA;          # {i}   MAQUINA.
set LISTON;           # {j}   TIPO DE LISTON.

set A1 within  {MAQUINA , LISTON}; # A1 denota las rutas entre(i,j) de la planta  i al cliente j.

#######  PARAMETROS

param costo{MAQUINA, LISTON} >=0;  	    # Costo de produccion en la maquina i el liston j
param cant{MAQUINA, LISTON}  >=0 ;      # Cantidad de liston j por la maquina i
param Tiempo{MAQUINA}        >=0 ;  	# Tiempo de operacion de la maquina  i
param Disponibilidad{LISTON} >=0 ;      # Cantidad del liston j

#######  VARIABLES

var X {MAQUINA,LISTON}   >=0  ;         # Cantidad de unidades enviadas desde la planta de producción i (i = 1, 2, 3) hasta el centro de distrubución j (j = 1, 2, 3, 4)

#######  FUNCION OBJETIVO 

minimize Costo_Total  {i in MAQUINA}: 
 sum{ j in LISTON:(i,j) in A1}  costo[i,j] * X[i,j] ;

######   RESTRICCIONES
 
subject to Tiempos {i in MAQUINA}:
sum{ j in LISTON:(i,j) in A1} X[i,j] <= Tiempo[i];

subject to Disponibilidades {j in LISTON}:
sum{ i in MAQUINA:(i,j) in A1} cant[i,j]*X[i,j] >= Disponibilidad[j];