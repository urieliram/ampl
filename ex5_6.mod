#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 5.6

#######  CONJUNTOS

set Productos; #{i}
set Trabajadores; #{j}

#######  PARAMETROS

param Ingreso{Productos} >= 0;

param Cantidad_Trabajadores{Trabajadores} >=0;
param Horas_Trabajadas{Trabajadores} >=0;
param Sueldo_Hora{Trabajadores} >=0;

param Gasto_Trabajo{Trabajadores} >=0;

param Mano_Obra{Trabajadores} >= 0;
param Horas_Produccion{Productos, Trabajadores} >= 0;
param MaximoX{Productos};
param MinimoX{Productos};

param Limite_Superior{Trabajadores} >=0;


#######  VARIABLES
var X{Productos}; #1,2,3
var Z{Trabajadores}; #1,2 

#######  FUNCION OBJETIVO

maximize Utilidad:
sum{i in Productos} Ingreso[i]*X[i] -
sum{j in Trabajadores} ((Cantidad_Trabajadores[j]*Horas_Trabajadas[j]*Sueldo_Hora[j])+(1.5*Sueldo_Hora[j]*Z[j]))-
sum{j in Trabajadores} Gasto_Trabajo[j]*((Cantidad_Trabajadores[j]*Horas_Trabajadas[j])+Z[j])- 
800;

######   RESTRICCIONES

subject to ManoObra{j in Trabajadores}:
sum{i in Productos} Horas_Produccion[i,j]*X[i] <= Mano_Obra[j] +Z[j]; 

subject to Maxima{i in Productos}:
X[i] <= MaximoX[i];

subject to Minima{i in Productos}:
X[i] >= MinimoX[i];

subject to HorasMaxima{j in Trabajadores}:
Z[j] <= Limite_Superior[j];

	





                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               