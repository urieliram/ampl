#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Problema de 	Planificación de la Producción \\
## La empresa Sil Computer necesita satisfacer la demanda de computadoras por parte do sus clientes 
## (grandes corporaciones e instituciones educacionales) para los próximos 4 trimestres.
## Actualmente, Sil Computer tiene 5000 computadores en inventario. La demanda esperada 
## para los próximos trimestres son 7000, 15000. 10000 y 8000. Sil Computer tiene el material
## y la capacidad de producir hasta 10000 computadores cada trimestre, a un costo de US\$ 2000 por computador.
## Empleando personal de sobre-tiempo se puede producir hasta 2500 computadores más a un costo individual de US\$ 2200.
## Las computadoras producidas en un trimestre pueden ser usados para satisfacer la demanda de ese período,
## o bien quedar en inventario para ser usados posteriormente. 
## Cada computador en inventario tiene un costo adicional de US\$100 por período para reflejar los costos de almacenaje.
## ¿Cómo puede satisfacer Sil Computer su demanda a costo mínimo?

###########  CONJUNTOS

set TRIMESTRES;

###########  PARAMETROS

param d{TRIMESTRES}  ; ## demanda de los trimestres

param s0   ; ## inventario_inicial

param c1  ; ## costo de produccion base
param c2  ; ## costo de produccion horas extra
param c3  ; ## costo de inventario

param max1   ; ## maxima producción costo base
param max2   ; ## maxima producción costo horas extra

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
 
 #Restriccion de maximo de pedidos de producción normal
  subject to Rest_Normal {i in TRIMESTRES}:
  x[ i ] <= max1; 
 
 #Restriccion de maximo de pedidos de producción extra
  subject to Rest_Extra {i in TRIMESTRES}:
  y[ i ] <= max2;