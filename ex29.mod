#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una empresa fabrica dos tipos de rotuladores: de la clase A 200 u.m. la unidad 
## y de la clase B 150 u.m. En la producción diaria se sabe que el número de rotuladores 
## de la clase B no supera en 1000 unidades a los de la A; además, entre las dos clases 
## no superan las 3000 unidades y la de la clase B no bajan de 1000 unidades por día. 
## Hallar el costo mínimo de la producción diaria.

#######  CONJUNTOS         {indice}    Descripcion del conjunto
set ROTULADOR;           # {i} Tipos de rotulador
set RECURSOS;            # {j} Recursos

#######  PARAMETROS

param C{ROTULADOR}            ; # Costo rotulador
param A{ROTULADOR,RECURSOS}   ; # Criterios de unidades a cumplir por rotulador
param B{RECURSOS}             ; # Criterios de unidades a cumplir

#######  VARIABLES

## XA Rotulador A a producir
## XB Rotulador b a producir

var X {ROTULADOR} >=0; 

#######  FUNCION OBJETIVO 

minimize Z: 
sum{i in ROTULADOR} C[i] * X[i];

######   RESTRICCIONES

##  Restriccion de disponibilidad de recursos
subject to Restriccion_1{j in RECURSOS}:
sum{i in ROTULADOR} A[i,j] * X[i] <= B[j];