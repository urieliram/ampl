#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una empresa especializada en la fabricaci�n de mobiliario para casas de mu�ecas,
## produce cierto tipo de mini mesas y mini sillas que vende a 2000 U monetarias y 
## 3000 U monetarias por cada art�culo, respectivamente. Desea saber cu�ntas unidades
## de cada art�culo debe fabricar diariamente un operario para maximizar los ingresos,

## Teniendo las siguientes restricciones: 
## El n�mero total de unidades de los dos tipos no podr� exceder de cuatro por d�a y operario.
## Cada mini mesa requiere dos horas para su fabricaci�n; cada mini silla, tres horas. 
## La jornada laboral m�xima es de 10 horas. 
## El material utilizado en cada mini mesa cuesta 400 U monetarias .
## el utilizado en cada mini silla cuesta 200 U monetarias. 
## Cada operario dispone de 1200 U monetarias diarias para material.

 set PRODUCTOS ;                      # Minimesas, minisillas 
 set RECURSOS  ;                      # Recursos utilizados
 
 param recurso {PRODUCTOS,RECURSOS};  # Cantidad de recurso necesario para el producto
 
 param disponibilidad {RECURSOS};     # Disponibilidad del Recurso 
 
 param beneficio {PRODUCTOS};         # Beneficio por producto
 
 var   X {PRODUCTOS} >= 0, integer;
 
 maximize Beneficios:
 sum{i in PRODUCTOS} beneficio[i] * X[i];
 
 #Restriccion de recursos
 subject to Rest_Recursos {j in RECURSOS}:
 sum{i in PRODUCTOS} recurso[i,j] * X[i] <= disponibilidad[j];