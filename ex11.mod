#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
# Ejercicio 11

# VARIABLES:
# X1 : Cantidad de remolacha a plantar en la parcela 1 (Ha)
# X3 : Cantidad de remolacha a plantar en la parcela 2
# X2 : Cantidad de algodon a plantar en la parcela 1 (Ha)
# X4 : Cantidad de algodon a plantar en la parcela 2
# Z : Funcion de maximizacion del beneficio de plantar el cultivo en la parcela

# Modelo:
# MAX Z = 700 X1 + 700 X3 + 500 X2 + 500 X4

# Sujeta a:
# (1) LIMITE DE RECURSO
# 3X1	+	2X2					< 	500 
# 					3X3	+	2X4	< 	1200 
# X1			+	X3			< 	800
# 		+ 	X2			+	X4	< 	600
# X1	+	x2					<	400
# 					X3	+	X4	<	900
#9X1	+ 	9X2	-	4X3	+	4X4	=	0

# X1	,	X2	,	X3	,	X4	>	0

 #######  CONJUNTOS

 set PRODUCTO;   
 set RECURSOS; 
 set RECURSOS1;         
 
 #######  PARAMETROS
 
 param B{PRODUCTO};           # Beneficio por producto.
  
 param U{PRODUCTO,RECURSOS};  # Cantidad de recurso necesario.
 
 param U1{PRODUCTO,RECURSOS1};  # Cantidad de recurso necesario.
 
 param T{RECURSOS};           # Cantidad de recurso necesario. 
 
 #######  VARIABLES
  
 var X {PRODUCTO} >= 0, integer;       # Variables del Problema
 
 #######  FUNCION OBJETIVO 
 
 maximize Z:
 sum{i in PRODUCTO} B[i] * X[i];
 
 ######   RESTRICCIONES
 
subject to R1{j in RECURSOS}:
sum{i in PRODUCTO} U[i,j] * X[i] <= T[j];

subject to R2{j in RECURSOS1}:
sum{i in PRODUCTO} U1[i,j] * X[i] = 0;
   