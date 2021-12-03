#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS

set PIENSO;
set PRODUCTOS;

#######  PARAMETROS

param requerimiento{PIENSO,PRODUCTOS};	#Parametros por producto
param disponibilidad{PRODUCTOS};		#Disponibilidad por restricciones
param precio{PIENSO};	#Utilidad por producto

#######  VARIABLES

var produccion{PIENSO}>=0 integer;	#Produccion por producto por operador

#######  FUNCION OBJETIVO 

maximize ganancias:sum{i in PIENSO} precio[i]*produccion[i]; #Objetivo: total de ventas

######   RESTRICCIONES

subject to limitantes{j in PRODUCTOS}:sum{i in PIENSO} requerimiento[i,j]*produccion[i]<=disponibilidad[j];	#Restricciones