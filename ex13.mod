#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
## Ejercicio numero 13
##  Una compañía aérea dispone de dos tipos de aviones A, y A., para cubrir un determinado
##  trayecto. El avión A, debe hacer el trayecto más veces que el avión A2 pero no puede sobrepasar 120
##  viajes. Entre los dos aviones deben hacer más de 60 vuelos, pero menos de 200. En cada vuelo, A!
##  consume 900 litros de combustible y A, 700 litros. En cada viaje del avión A, la empresa gana 30.000
##  u.m. y 20.000 u.m. por cada viaje del avión A,.
##  ¿Cuántos viajes debe hacer cada avión para obtener el máximo de ganancias?

#######  VARIABLES
var a1 >=0;
var a2 >=0;

#######  FUNCION OBJETIVO 
maximize Z: (30000 - 900)* a1 + (20000 - 700) * a2; #precio - costo

######   RESTRICCIONES
RES1: a1 <= 120;
RES2: 60 <= a1 + a2 <= 200;
