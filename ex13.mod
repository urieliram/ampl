#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
## Ejercicio numero 13
##  Una compa��a a�rea dispone de dos tipos de aviones A, y A., para cubrir un determinado
##  trayecto. El avi�n A, debe hacer el trayecto m�s veces que el avi�n A2 pero no puede sobrepasar 120
##  viajes. Entre los dos aviones deben hacer m�s de 60 vuelos, pero menos de 200. En cada vuelo, A!
##  consume 900 litros de combustible y A, 700 litros. En cada viaje del avi�n A, la empresa gana 30.000
##  u.m. y 20.000 u.m. por cada viaje del avi�n A,.
##  �Cu�ntos viajes debe hacer cada avi�n para obtener el m�ximo de ganancias?

#######  VARIABLES
var a1 >=0;
var a2 >=0;

#######  FUNCION OBJETIVO 
maximize Z: (30000 - 900)* a1 + (20000 - 700) * a2; #precio - costo

######   RESTRICCIONES
RES1: a1 <= 120;
RES2: 60 <= a1 + a2 <= 200;
