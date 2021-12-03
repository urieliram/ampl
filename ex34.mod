#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 34

# VARIABLES:
# X1 : Cantidad de producto A (g)
# X2 : Cantidad de producto B (g)

# Z : Funcion de maximizacion de las vitaminas 

# Modelo:
# MAX Z = 0.3 X1 + 0.2 X2

# Sujeta a:
# (1) LIMITE DE CONTENIDO
# X1	+	X2	<	150
# -X1	-	X2	<	50
# -X1	+	X2	<	0
# X1			<	100
# X1	, 	X2	>	0

 #######  CONJUNTOS
 set PRODUCTO;                # tipo de producto en la mezcla
 set RECURSOS;                # recursos 
       
 #######  PARAMETROS
 param B{PRODUCTO};           # Aporte del producto a la mezcla.
  
 param U{PRODUCTO,RECURSOS};  # Restricciones por producto.
 
 param T{RECURSOS};           # Restricciones. 
 
 #######  VARIABLES
 var X {PRODUCTO} >= 0, integer;             # Variables del Problema. 
 
 #######  FUNCION OBJETIVO 
 maximize Z:
 sum{i in PRODUCTO} B[i] * X[i];
   
 ######   RESTRICCIONES
 subject to R{j in RECURSOS}:
 sum{i in PRODUCTO} U[i,j] * X[i] <= T[j];
