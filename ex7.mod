#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

##Descripcion del conjunto

set PRODUCTO;             # {i} Producto A
set SUSTANCIA;            # {j} Sustancia B
set RECURSOS;             # {k} Recursos disponibles

#######  PARAMETROS

param P{PRODUCTO}            ; # Ganancia del producto A
param C{SUSTANCIA}           ; # Costo de la sustancia B
param A{PRODUCTO,RECURSOS}   ;# Uso del recurso
param A2{SUSTANCIA,RECURSOS} ; # Uso del recurso
param B{RECURSOS}            ; # Recursos diponibles

#######  VARIABLES

## X Producto A a producir
## Y Sustancia B a utilizar

var X {PRODUCTO} >=0, integer; 
var Y {SUSTANCIA} >=0, integer; 

#######  Funcion objetivo - Max la ganancia

maximize Z: 
sum{i in PRODUCTO} (P[i] * X[i]) - sum{j in SUSTANCIA} (C[j] * Y[j]);

######   RESTRICCIONES

##  Restriccion de disponibilidad de recursos
subject to R1{k in RECURSOS}:
sum{i in PRODUCTO} A[i,k] * X[i] + sum{j in SUSTANCIA} A2[j,k] * Y[j] <= B[k];