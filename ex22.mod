#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 14

#######  CONJUNTOS
 set TRABAJADORES;     # Mecanicos , Electricos 
 set RECURSOS  ;        
#######  PARAMETROS

 param A {TRABAJADORES,RECURSOS}; 
 param C {TRABAJADORES};            #Costo por camion
 param D {TRABAJADORES};            #Cantidad de camiones
 
#######  VARIABLES
 var   X {TRABAJADORES} >= 0, integer;
 
 #######  FUNCION OBJETIVO 
 maximize Z: sum{i in TRABAJADORES} C[i] * X[i];
 subject to R1 {j in RECURSOS}: sum{i in TRABAJADORES} A[i,j] * X[i] >= 0;
 subject to R2 {i in TRABAJADORES}:  X[i] <= D[i];