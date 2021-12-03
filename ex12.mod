#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

##  Una empresa constructora dispone de dos tipos de camiones C, y C2 y quiere transportar 100
##  toneladas de arena a una obra. Sabiendo que dispone de 6 camiones tipo C, con capacidad para 15
##  toneladas y con un coste de 4000 u.m. por viaje y de 10 camiones tipo C2 con una capacidad de 5
##  toneladas y con un coste de 3000 u.m. por viaje.
##  ¿Cuál es el número posible de camiones que debe usar para que el coste sea mínimo?

 set CAMIONES ;                 # Camion1,Camion2 
 set RECURSOS ;                 # 
  
 param A {CAMIONES,RECURSOS};   # Cantidad de recurso necesario para el producto
 
 param B {RECURSOS};            # Disponibilidad del Recurso 
 
 param C {CAMIONES};            #Costo por camion
 
 param D {CAMIONES};            #Cantidad de camiones
 
 var   X {CAMIONES} >= 0, integer;
 
 minimize Z:
 sum{i in CAMIONES} C[i] * X[i];
 
 #para todo RECURSO
 subject to Recursos {j in RECURSOS}:
 sum{i in CAMIONES} A[i,j] * X[i] >= B[j];
 
 subject to Cantidad {i in CAMIONES}: 
 X[i] <= D[i];