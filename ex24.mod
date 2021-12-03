#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 14

 #######  CONJUNTOS
set COLONIA;   # Colonia A y B
set RECURSOS;  # Recursos a cumplir
 
 #######  PARAMETROS
param P {COLONIA};
param A {COLONIA, RECURSOS};
param B {RECURSOS};

#######  VARIABLES
# XA Numero de litros de colonia A a preparar diariamente
# XB Numero de litros de colonia A a preparar diariamente

var X {COLONIA};

#######  FUNCION OBJETIVO 
# Funcion objeto - max la ganancia
maximize Z : sum {i in COLONIA} P[i]*X[i];

subject to Recurso_disponible {j in RECURSOS}: 
sum {i in COLONIA} A[i,j]* X[i] <= B[j]; 
