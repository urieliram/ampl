#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 14

#var X1 >= 0, integer;
#var X2 >= 0, integer;
#var X3 >= 0, integer;
#var X4 >= 0, integer;
#var X5 >= 0, integer;
#var X6 >= 0, integer;

#maximize Profit: 50*X1 + 40*X2 + 40*X3 + 40*X4 + 50*X5 + 50*X6;

#subject to R1: X1 + X2 >= 60;
#subject to R2: X2 + X3 >= 50;
#subject to R1: X3 + X4 >= 35;
#subject to R1: X4 + X5 >= 55;
#subject to R1: X5 + X6 >= 40;
#subject to R1: X1 + X6 >= 25;

#option solver CPLEXAMP;

#solve;
#display X1, X2, X3, X4, X5, X6; 

 #######  CONJUNTOS
  
 set TURNO ;               # 1, 2, 3, 4, 5, 6
 set RECURSOS ;            # 
 
 #######  PARAMETROS

 param A {TURNO,RECURSOS};  # Cantidad de recurso necesario para el producto
 
 param B {RECURSOS};            # Disponibilidad del Recurso 
 
 param C {TURNO};           # Beneficio por producto
 
 #######  VARIABLES
 
 var   X {TURNO} >= 0, integer;
 
 #######  FUNCION OBJETIVO 
 
 minimize Z:
 sum{i in TURNO} C[i] * X[i];
 
  ######   RESTRICCIONES
 subject to Recursos {j in RECURSOS}:
 sum{i in TURNO} A[i,j] * X[i] >= B[j];