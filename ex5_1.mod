#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

#######  CONJUNTOS

set ALIMENTOS;        # {i} Tipos de alimento
set REQUERIMIENTO;    # {j} Requerimientos que debe contener la dieta

#######  PARAMETROS

param A{ALIMENTOS, REQUERIMIENTO}   >=0 ;  # Aporte x und de alimento
param C{ALIMENTOS}                  >=0 ;  # Precio del alimento
param L{ALIMENTOS}                  >=0 ;  # Limite de und alimento por dia
param B{REQUERIMIENTO}              >=0 ;  # Cantidad de requerimiento solicitada

#######  VARIABLES

## XA Avena
## XP Pollo
## XH Huevo
## XL Leche
## XPA Pastel
## XC Cerdo

var X {ALIMENTOS} >=0 , integer ; #Unidades de alimento tipo {i} a incluir en la dieta. 
                                  #Cada unidad de alimento está compuesta por las porciones definidas en la tabla.

#######  FUNCION OBJETIVO 

minimize Z: 
sum{i in ALIMENTOS} C[i] * X[i];

######   RESTRICCIONES
##  R1: Restriccion de satisfaccion del Requerimiento
subject to R1 {j in REQUERIMIENTO}: sum{i in ALIMENTOS} A[i,j] * X[i] >= B[j];
##  R2: Restriccion limite de und x dia
subject to R2 {i in ALIMENTOS} : X[i] <= L[i];
