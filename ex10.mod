#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una multinacional farmac�utica desea fabricar un compuesto nutritivo a base de dos productos
## A y B. El producto A contiene 30% de prote�nas, un 1 % de grasas y un 10% de az�cares. El producto
## B contiene un 5% de prote�nas, un 7% de grasas y un 10% de az�cares.
## El compuesto tiene que tener, al menos, 25g de prote�nas, 6g de grasas y 30g de az�cares.
## e) coste del producto A es de 0.6 u.m./g. y el de B es de 0.2 u.m./g.
## �Cu�ntos gramos de cada producto debe tener el compuesto para que el coste total sea m�nimo?

## VARIABLES
## XA Cantidad en gramos del producto A a utilizar
## Xb Cantidad en gramos del producto B a utilizar

#######  CONJUNTOS
	
set PRODUCTOS;
set NUTRIENTES;

#######  PARAMETROS

param contenido{PRODUCTOS,NUTRIENTES};	#Porcentaje de nutriente por producto
param requerimiento{NUTRIENTES};		#Requerimiento minimo de nutrientes
param costo{PRODUCTOS};	                #Costo por gramo de producto

#######  VARIABLES

var cantidad{PRODUCTOS}>=0 integer;	#Cantidad de gramos por producto

#######  FUNCION OBJETIVO 

minimize gasto:sum{i in PRODUCTOS} costo[i]*cantidad[i]; #Objetivo: total de gasto

######   RESTRICCIONES

#Restricciones de minimo de nutrientes
subject to limitantes{j in NUTRIENTES}:
	sum{i in PRODUCTOS} contenido[i,j]/100*cantidad[i]>=requerimiento[j];	






