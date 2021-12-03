#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS
set MINAS;
set PRODUCTOS;

#######  PARAMETROS
param gastos {MINAS};
param produccion {MINAS,PRODUCTOS};
param demanda {PRODUCTOS};

#######  VARIABLES
 var   X {MINAS} >= 0; #Numero de dias a explotar la mina i
 
 #######  FUNCION OBJETIVO 
 minimize Z:
 sum{i in MINAS}  gastos[i] * X[i];
 
 ######   RESTRICCIONES
 subject to tipos_carbon {j in PRODUCTOS}:
 sum{i in MINAS} produccion[i,j] * X[i] >= demanda[j];