##  Problema de la mochila por: 
##  Uriel Iram Lezama Lope
##  (Se omiten acentos)

set OBJETOS;
set ATRIBUTOS;

##  Se definen los parametros

param beneficio{OBJETOS};
param peso{OBJETOS,ATRIBUTOS};
param capacidad{ATRIBUTOS};

##  Se definen las variables

var x{OBJETOS} >= 0, <= 1, integer; # si el objeto va o no

# funcion objetivo

maximize Beneficio : sum {i in OBJETOS} x[i] * beneficio[i];

# Restricciones

subject to ResCapacidad {j in ATRIBUTOS}:  sum {i in OBJETOS} x[i] * peso[i,j] <= capacidad[j];

















