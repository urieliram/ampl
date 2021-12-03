#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

  
#######  CONJUNTOS

 set PRODUCTOS ;             
 set RECURSOS  ;        

#######  PARAMETROS

 param A {PRODUCTOS,RECURSOS};  
 
 param B {RECURSOS};       
 
 param C {PRODUCTOS};        
 
#######  VARIABLES

 var   X {PRODUCTOS} >= 0, integer;
 
#######  FUNCION OBJETIVO 

 maximize Z:
 sum{i in PRODUCTOS} C[i] * X[i];
 
 
######   RESTRICCIONES

 subject to Recursos {j in RECURSOS}:
 sum{i in PRODUCTOS} A[i,j] * X[i] <= B[j];