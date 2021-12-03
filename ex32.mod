#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# MAX Z = 1200 X1 + 2000 X2

# 4 X1 + 8 X2 < 600
# X1          < 120
# X2          < 70
# X1 , X2     > 0
  
#######  CONJUNTOS

 set PRODUCTOS ;               
 set RECURSOS  ;              
 
#######  PARAMETROS
 param Cantidad {PRODUCTOS,RECURSOS};  # Cantidad de recurso necesario para el producto.
 
 param Disponibilidad {RECURSOS};      # Disponibilidad del Recurso.
 
 param Beneficio {PRODUCTOS};          # Beneficio por producto.
 
 param Limite {PRODUCTOS};             # Limite de produccion por producto.
 
#######  VARIABLES
 #Variables del Problema, cantidad a producir de A y B
 var X {PRODUCTOS} >= 0, integer;
 
#######  FUNCION OBJETIVO 
  #Funcion objetivo
  maximize Z:
 sum{i in PRODUCTOS} Beneficio[i] * X[i];
 
######   RESTRICCIONES
  #Restriccion de Recursos
  subject to Recurso {j in RECURSOS}:
 sum{i in PRODUCTOS} Cantidad[i,j] * X[i] <= Disponibilidad[j]; 
 
  #Limite de produccion por producto  
  subject to Produccion{i in PRODUCTOS}:
  X[i] <= Limite[i]; 
  