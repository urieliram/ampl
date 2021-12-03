#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

# Ejercicio numero 44

#######  CONJUNTOS

set CONTRATISTAS;
set PROYECTOS;

#######  PARAMETROS

param Costos {PROYECTOS, CONTRATISTAS};
param TotalDeProyectos {PROYECTOS};
param TotalDeProyectosPorContratista {CONTRATISTAS};

#######  VARIABLES

var X {PROYECTOS, CONTRATISTAS} >= 0;

#######  FUNCION OBJETIVO 

minimize TotalCostos:
    sum{i in PROYECTOS, j in CONTRATISTAS}
        Costos[i,j] * X[i,j];
        
######   RESTRICCIONES

subject to Limit_Proyect {i in PROYECTOS}:
    sum{j in CONTRATISTAS} X[i,j] = TotalDeProyectos[i];

subject to Limit_Proyect_Contrat {i in CONTRATISTAS}:
    sum{j in PROYECTOS} X[j,i] = TotalDeProyectosPorContratista[i];