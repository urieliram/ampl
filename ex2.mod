#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
# Ejercicio numero 14

# VARIABLES:
# X1 : Numero de lotes clase A
# X2 : Numero de lotes clase B
# Z : Funcion de maximizacion del beneficio

# Modelo:
# MAX Z = 1200 X1 + 1400 X2 

# Sujeta a:
# X1	+	2X2	< 	800 
#2X1	+	 X2	< 	800 
# X1	+	 X2	< 	500
# X1	,    X2	>	0

#######  CONJUNTOS
 set PRODUCTO;   
 set RECURSOS; 
          
 #######  PARAMETROS
 param B{PRODUCTO};           # Beneficio por producto.
  
 param U{PRODUCTO,RECURSOS};  # Cantidad de recurso necesario.
 
 param T{RECURSOS};           # Cantidad de recurso necesario. 

 #######  VARIABLES 
 var X {PRODUCTO} >= 0;       # Variables del Problema
 
 #######  FUNCION OBJETIVO 
 maximize Z:
 sum{i in PRODUCTO} B[i] * X[i];
 
 ######   RESTRICCIONES
subject to R{j in RECURSOS}:
sum{i in PRODUCTO} U[i,j] * X[i] <= T[j];
   