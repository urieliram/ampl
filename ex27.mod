#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS

set TIPO_CALIDAD;
set RECURSOS;

#######  PARAMETROS
param Beneficios {TIPO_CALIDAD};
param RecursosPorCalidad {RECURSOS, TIPO_CALIDAD};
param TotalRecursos {RECURSOS};

#######  VARIABLES
var X {TIPO_CALIDAD} >= 0;

#######  FUNCION OBJETIVO 
maximize Ganancias:
    sum{i in TIPO_CALIDAD} Beneficios[i] * X[i]
;

######   RESTRICCIONES
subject to Limit {i in RECURSOS}:
    sum{j in TIPO_CALIDAD} RecursosPorCalidad[i,j] * X[j] <= TotalRecursos[i]
;