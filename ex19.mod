#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

set PRODUCTOS;
set INGREDIENTES;

#######  PARAMETROS
param requerimiento{PRODUCTOS,INGREDIENTES};	#Parametros por producto
param disponibilidad{INGREDIENTES};		#Disponibilidad por restricciones
param beneficio{PRODUCTOS};	#Utilidad por producto

#######  VARIABLES
var produccion{PRODUCTOS}>=0 integer;	#Produccion por producto por operador

#######  FUNCION OBJETIVO 
maximize ganancias:sum{i in PRODUCTOS} beneficio[i]*produccion[i]; #Objetivo: total de ventas

######   RESTRICCIONES
subject to limitantes{j in INGREDIENTES}:sum{i in PRODUCTOS} requerimiento[i,j]*produccion[i]<=disponibilidad[j];	#Restricciones