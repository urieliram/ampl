#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#var X1 >= 0, integer;
#var X2 >= 0, integer;

#maximize Profit: 2500 * X1 + 4500 * X2;

#subject to R1: 0.5*X1 + 3*X2 <= 9;
#subject to R2: X1 + X2 <= 8;

#option solver CPLEXAMP;

#solve;
#display X1, X2; 
  
 set MOTOS ;                 # N, U 
 set RECURSOS  ;                 # 
 
 param A {MOTOS,RECURSOS};  # Cantidad de recurso necesario para el producto
 
 param B {RECURSOS};            # Disponibilidad del Recurso 
 
 param C {MOTOS};           # Beneficio por producto
 
 var   X {MOTOS} >= 0, integer;
 
 maximize Z:
 sum{i in MOTOS} C[i] * X[i];
 
 #para todo RECURSO
 
 subject to Recursos {j in RECURSOS}:
 sum{i in MOTOS} A[i,j] * X[i] <= B[j];