#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
# Ejercicio numero 21

# VARIABLES:
# X1 : Cantidad de carga en bodega inferior (t)
# X2 : Cantidad de carga en bodega media (t)
# X3 : Cantidad de carga en bodega superior (t)
# Z : Funcion de maximizacion de las utilidades 

# Modelo:
# MAX Z = 8000 X1 + 10000 X2 + 12 000 X3

# Sujeto a:
# X1	+	X2	+	X3	<	100 
# X1					<	40
# X1	-	3X2			=	0
# X1			-2.5X3	=	0	
#		+	X2	+	X3	<	60


# X1	, 	X2	,	X3	>	0

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
 var X {PRODUCTO} >= 0, integer;         # Variables del Problema, cantidad de carga en bodega 
 
 #######  FUNCION OBJETIVO 
 maximize Z:
 sum{i in PRODUCTO} B[i] * X[i];
 
 ######   RESTRICCIONES
 subject to R1{j in RECURSOS}:
 sum{i in PRODUCTO} U[i,j] * X[i] <= T[j]; 
 
subject to R2{j in RECURSOS1}:
sum{i in PRODUCTO} U1[i,j] * X[i] = 0;
