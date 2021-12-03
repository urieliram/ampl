#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# La Asociación de Estudiantes de Administración de Empresas dispone de 100000 u. m. y ha
# pensado invertirlos en dos negocios. El primero le reporta una utilidad de 25 u. m. mensuales y el
# segundo 40 u. m. por mes en cada 100 u. m. invertidas. Debido a ciertas condiciones impuestas por la
# Asamblea de Socios, se debe invertir al menos el 25% del capital en el primer negocio y no más del
# 50% en el segundo.
# Además, la cantidad invertida en este último no debe ser mayor a 1.5 veces la cantidad invertida
# en el primero. Se pide plantear este problema como un modelo de programación lineal.

# VARIABLES:
# X1 : Cantidad de dinero a invertir en el negocio 1
# X2 : Cantidad de dinero a invertir en el negocio 2
# Z : Funcion de maximizacion de la inversion

# Modelo (primal):
# MAX Z = 0.25 X1 + 0.40 X2

# Sujeta a:
# (1) LIMITES DE RECURSO
#      X1 + X2 < 100000

# (2) CONDICIONES DE LA ASAMBLEA DE SOCIOS
# X1           > 25000
#         + X2 < 50000
# -1.5 X1 + X2 < 0

#      X1 , X2 > 0

 #######  CONJUNTOS

 set NEGOCIOS;   
 set RECURSOS;  
         
 #######  PARAMETROS
 
 param Utilidad      {NEGOCIOS};           # Beneficio por negocio.
  
 param Uso_recurso   {NEGOCIOS,RECURSOS};  # Cantidad de recurso necesario para el negocio.
 
 param Total_recurso {RECURSOS};           # Cantidad de recurso necesario para el negocio. 

 #######  VARIABLES
 
 var X {NEGOCIOS} >= 0;                    # Variables del Problema, cantidad a producir de A y B
 
 #######  FUNCION OBJETIVO
 
  # Funcion objetivo
  maximize Z:
 sum{i in NEGOCIOS} Utilidad[i] * X[i];
 
 ######   RESTRICCIONES
 
 # Restriccion de Recursos
 subject to Recurso {j in RECURSOS}:
 sum{i in NEGOCIOS} Uso_recurso[i,j] * X[i] <= Total_recurso[j]; 

 # Se debe invertir al menos el 25% del capital en el primer negocio 
 subject to Condicion1 {j in RECURSOS}:
 X['A'] >= .25 * Total_recurso[j];
  
 # No se debe invertir más del 50% en el segundo
 subject to Condicion2 {j in RECURSOS}:
 X['B'] <= .50 * Total_recurso[j];  
  
 # La cantidad invertida en este último no debe ser mayor a 1.5 veces la cantidad invertida en el primero
 subject to Condicion3:
 1.5 * X['A'] <= X['B'];  