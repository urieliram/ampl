#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS
  
 set TURNOS ;              
 
 #######  PARAMETROS

 param d{TURNOS};  # Demanda por turno
 param s{TURNOS};  # Inventario al final del turno

 param Max; #capacidad maxima del tanque
 param Min; #capacidad minima del tanque
 param cp;  #costo de pipa  
 param cs;  #costo de inventario  
 param y0;  #nivel inicial 
 param pt;  #pipas por turno 
 param p;   #litros por pipa
  
 #######  VARIABLES
 
 var x{TURNOS} >= 0, integer;
 var y{TURNOS} >= 0; 
 var u{TURNOS} >= 0; 
 var total     >= 0;
 
  ######   FUNCION OBJETIVO 
 
 minimize Z:
 sum{i in TURNOS} ( cp * x[ i ] * p + cs * u[ i ] )  ;
 
  ######   RESTRICCIONES
   
 subject to limite_min0 :
 -d[ 1 ] + y0 + x[ 1 ] * p >= Min;

 subject to limite_max0 :
 -d[ 1 ] + y0 + x[ 1 ] * p <= Max;
 
 subject to limite_min {i in TURNOS: i<>1}:
 -d[ i ] + y[ i-1 ] + x[ i ] * p >= Min;
 
 subject to limite_max {i in TURNOS: i<>1}:
 -d[ i ] + y[ i-1 ] + x[ i ] * p <= Max;
 
 subject to balance_1 :
 -d[ 1 ] + y0       + x[ 1 ] * p = y[ 1 ];    
  
 subject to balance_i {i in TURNOS: i<>1 }:
 -d[ i ] + y[ i-1 ] + x[ i ] * p = y[ i ];  
  
 subject to pipas_turno {i in TURNOS}:
 x[ i ] <= pt;

 subject to turnos_prohibidos {i in TURNOS: i=1 or i=18 or i=19 or i=20}:
 x[ i ] = 0;
  
 subject to inventarioCERO {i in TURNOS}:
 u[ i ] >= 0 ;
 
 subject to inventarioPENALIZACION {i in TURNOS}:
 u[ i ] >= s[ i ] - y[ i ];  
  
 subject to calculo_pipas:
 sum{i in TURNOS}  x[ i ] <= total;  