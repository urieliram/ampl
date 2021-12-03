#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 4

#var  a >= 0 integer;
#var  b >= 0, integer;

#minimize costo: 150*a + 300*b;
#
#s.t. naranjas: 8*a + 2*b >= 16;
#s.t. platano: a + b >= 5;
#s.t. manzana: 2*a + 7*b >= 20;

#######  CONJUNTOS

set F; # Frutas
set P; #Proveedores

#######  PARAMETROS

#param a {j in P};
#param b; #Frutas
param c {j in F, i in P};
param d {j in F};
param e {i in P};

#######  VARIABLES

var x {i in P};

#######  FUNCION OBJETIVO 

minimize profit: sum {i in P} e[i] * x[i];

######   RESTRICCIONES

subject to demanda {j in F}: sum {i in P} c[j,i]*x[i] >= d[j]; #demanda de frutas

