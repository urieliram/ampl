#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS

set PRODUCTOS;
set PETROLEOS;

#######  PARAMETROS

param inventario{PETROLEOS};		# Disponibilidad por petroleo
param beneficio{PRODUCTOS};	        # Utilidad por producto
param minima{PRODUCTOS};	        # limites de ventas por producto
param maxima{PRODUCTOS};	        # limites de ventas por producto
param presion_pro{PRODUCTOS};	    # limites de ventas por producto
param octanaje_pro{PRODUCTOS};	    # limites de ventas por producto
param presion_pet{PETROLEOS};	    # limites de ventas por producto
param octanaje_pet{PETROLEOS};	    # limites de ventas por producto
param costo{PETROLEOS};             # Costo por barril de petroleo usado

#######  VARIABLES

var mezcla{PRODUCTOS,PETROLEOS}>=0 integer;	#Barriles de petroleo j para producto i

#######  FUNCION OBJETIVO 

maximize ganancias:((sum{i in PRODUCTOS} beneficio[i]*(sum{j in PETROLEOS} mezcla[i,j]))-(sum{j in PETROLEOS} costo[j]*(sum{i in PRODUCTOS} mezcla[i,j]))); #Objetivo: total de ventas

######   RESTRICCIONES

subject to disponibilidad{j in PETROLEOS}:
sum{i in PRODUCTOS} mezcla[i,j]<=inventario[j];	#Petroleo disponible

subject to ventas{i in PRODUCTOS}: 
maxima[i]>=(sum{j in PETROLEOS} mezcla[i,j])>=minima[i];	#Productos dentro de limites de ventas

subject to p_vapor{i in PRODUCTOS}: 
sum{j in PETROLEOS} presion_pet[j]*mezcla[i,j]<=presion_pro[i]*(sum{j in PETROLEOS} mezcla[i,j]);	#Especificacion presion

subject to octanaje{i in PRODUCTOS}: 
sum{j in PETROLEOS} octanaje_pet[j]*mezcla[i,j]>=octanaje_pro[i]*(sum{j in PETROLEOS} mezcla[i,j]);	#Especificacion octanaje


# ganancias = 125000
# sum{j in PETROLEOS} mezcla['regular',j] = 50000
# sum{j in PETROLEOS} mezcla['extra',j] = 5000
# ampl: include ex5_5.run;
# maximize ganancias:
#	4*mezcla['regular','nacional'] - 3*mezcla['regular','importado'] + 
#	6*mezcla['extra','nacional'] - mezcla['extra','importado'];
# subject to disponibilidad['nacional']:
#	mezcla['regular','nacional'] + mezcla['extra','nacional'] <= 40000;
# subject to disponibilidad['importado']:
#	mezcla['regular','importado'] + mezcla['extra','importado'] <= 60000;
# subject to ventas['regular']:
#	50000 <= mezcla['regular','nacional'] + mezcla['regular','importado']
#	 <= 1e+05;
# subject to ventas['extra']:
#	5000 <= mezcla['extra','nacional'] + mezcla['extra','importado'] <= 
#	20000;
# subject to p_vapor['regular']:
#	2*mezcla['regular','nacional'] - 8*mezcla['regular','importado'] <= 0;
# subject to p_vapor['extra']:
#	2*mezcla['extra','nacional'] - 8*mezcla['extra','importado'] <= 0;
# subject to octanaje['regular']:
#	-mezcla['regular','nacional'] + 10*mezcla['regular','importado'] >= 0;
# subject to octanaje['extra']:
#	-6*mezcla['extra','nacional'] + 5*mezcla['extra','importado'] >= 0;