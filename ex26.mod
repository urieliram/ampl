#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una fábrica que produce paraguas tiene dos tipos de inspectores (A y B), quienes deben ser asignados
## para control de calidad. La política de la fábrica exige que por lo menos 18000 paraguas sean
## inspeccionados a diario (ocho horas de trabajo). Los inspectores de clase A pueden revisar 250
## paraguas por hora, con una precisión del 98%, mientras que los inspectores clase B pueden revisar 150
## con 95% de precisión. En el mercado actual un inspector clase A cobra 450 u.m. por hora y el B, 350
## u.m. por hora. Cada equivocación del inspector cuesta 100 u.m. a la fábrica. Hay ocho inspectores
## clase A y 10 clase B. El director de la fábrica quiere determinar la asignación óptima del personal de
## inspección.

## INFORMACION
## Inspector  Inspecciones/día   Precisión   Costo/hora   Costo_falla    Disponibilidad
##         A  250*8=2000         98%         450          100            8
##         B  150*8=1200         95%         350          100            10          
##            18000/día

#######  CONJUNTOS

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set INSPECTORES ;      # {i} Tipos de inspectores


#######  PARAMETROS

param salario{INSPECTORES}      >=0 ;  	  # Salario de inspectores
param porcentaje{INSPECTORES}   >=0 ;  	  # Porcentaje de error por inspector
param inspecciones{INSPECTORES} >=0 ;  	  # Cantidad de producto inspeccionado
param contratos{INSPECTORES}    >=0 ;  	  # Cantidad de contratos de los inspectores

param costo_falla    >=0 ;
param horas          >=0 ;
param total_paraguas >=0 ;

#######  VARIABLES

## A Número de inspectores de clase A a utilizar en un día de ocho horas
## B Número de inspectores de clase B a utilizar en un día de ocho horas
var X {INSPECTORES}    >=0 ,integer;   

#######  FUNCION OBJETIVO 

minimize Costo: 
 sum{ i in INSPECTORES } ( salario[i] * X[i] + costo_falla * (1-porcentaje[i]) * horas * inspecciones[i] * X[i] );

######   RESTRICCIONES

##  Restriccion de inspectores 
subject to Res_Contratos {i in INSPECTORES}:
X[i] <= contratos[i];

##  Restriccion de inspecciones 
## por lo menos 18000 paraguas sean inspeccionados a diario (ocho horas de trabajo)
subject to Res_Inspecciones :
sum{i in INSPECTORES} X[i] * horas * inspecciones[i] >= total_paraguas;






