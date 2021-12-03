#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

##  En una encuesta realizada por una televisión local se detectó que un programa con 20 minutos
##  de variedades y un minuto de publicidad capta 30000 espectadores, mientras que otro programa con 10
##  minutos de variedades y 1 minuto de publicidad capta 10000 espectadores.
##  Para un determinado período, la dirección de la red decide dedicar 80 minutos de variedades y los
##  anunciantes 6 minutos de publicidad, ¿Cuántas veces deberá aparecer cada programa con objeto de
##  captar el máximo número de espectadores?

 #######  CONJUNTOS
 
 set PROGRAMAS ;                 # P1, P2 
 set RECURSOS  ;                 # 
 
 #######  PARAMETROS
 
 param A {PROGRAMAS,RECURSOS};  # Cantidad de recurso necesario para el producto
 
 param B {RECURSOS};            # Disponibilidad del Recurso 
 
 param C {PROGRAMAS};           # Beneficio por producto
 
 #######  VARIABLES
 
 var   X {PROGRAMAS} >= 0, integer;
 
 #######  FUNCION OBJETIVO 
 
 maximize Z:
 sum{i in PROGRAMAS} C[i] * X[i];
 
 ######   RESTRICCIONES
 
 #para todo RECURSO
 subject to Recursos {j in RECURSOS}:
 sum{i in PROGRAMAS} A[i,j] * X[i] = B[j];