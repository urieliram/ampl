#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Problema de 	Planificaci�n de la Producci�n \\
## La empresa Sil Computer necesita satisfacer la demanda de computadoras por parte do sus clientes 
## (grandes corporaciones e instituciones educacionales) para los pr�ximos 4 trimestres.
## Actualmente, Sil Computer tiene 5000 computadores en inventario. La demanda esperada 
## para los pr�ximos trimestres son 7000, 15000. 10000 y 8000. Sil Computer tiene el material
## y la capacidad de producir hasta 10000 computadores cada trimestre, a un costo de US\$ 2000 por computador.
## Empleando personal de sobre-tiempo se puede producir hasta 2500 computadores m�s a un costo individual de US\$ 2200.
## Las computadoras producidas en un trimestre pueden ser usados para satisfacer la demanda de ese per�odo,
## o bien quedar en inventario para ser usados posteriormente. 
## Cada computador en inventario tiene un costo adicional de US\$100 por per�odo para reflejar los costos de almacenaje.
## �C�mo puede satisfacer Sil Computer su demanda a costo m�nimo?

###########  CONJUNTOS

set TRIMESTRES;

###########  PARAMETROS

param d{TRIMESTRES}  ; ## demanda de los trimestres

param s0   ; ## inventario_inicial

param c1  ; ## costo de produccion base
param c2  ; ## costo de produccion horas extra
param c3  ; ## costo de inventario

param max1   ; ## maxima producci�n costo base
param max2   ; ## maxima producci�n costo horas extra

###########  VARIABLES

 var   x {TRIMESTRES} >= 0, integer;
 var   y {TRIMESTRES} >= 0, integer;
 var   s {TRIMESTRES} >= 0, integer;

###########  FUNCION OBJETIVO

 minimize Z:
 sum{i in TRIMESTRES}(  x[i] * c1 + y[i] * c2 + s[i] * c3 ) ;

###########  RESTRICCIONES
 
 #Restriccion de balance trimestre 1
  subject to Rest_Balance0 {i in TRIMESTRES:i=1}: #i=1
  s0 + x[ i ] + y[ i ] - d[ i ] = s[ i ];
 
 #Restriccion de balance por trimestre
  subject to Rest_Balance {i in TRIMESTRES : i>1 }: #::i>1
  s[ i - 1 ] + x[ i ] + y[ i ] - d[ i ] = s[ i ];
 
 #Restriccion de maximo de pedidos de producci�n normal
  subject to Rest_Normal {i in TRIMESTRES}:
  x[ i ] <= max1; 
 
 #Restriccion de maximo de pedidos de producci�n extra
  subject to Rest_Extra {i in TRIMESTRES}:
  y[ i ] <= max2;