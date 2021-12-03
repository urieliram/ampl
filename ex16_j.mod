##Descripcion del conjunto.

set OPERACION ;         # {i} Tipos de operaci�n
set RECURSOS ;          # {j} Tipos de recursos


#######  Parametros

param P{OPERACION}            >=0 ;   # Beneficio unitario por cada operaci�n
param A{OPERACION,RECURSOS}   >=0 ;   # Horas requeridas de los recursos por cada operacion
param B{RECURSOS}             >=0 ;   # Horas totales disponibles de cada recurso
param a;                              # Factor de escala del beneficio

#######  Variables

## XC Operaci�n de captaci�n de dep�sitos
## XP Operaci�n de prestamo dinero.

var X {OPERACION} >=0, integer;   

#######  Funcion objetivo - m�x las ganancias
maximize Z: 
sum{i in OPERACION} P[i] * X[i] * a;

##  Restriccion de disponibilidad de recursos
subject to R1 {j in RECURSOS}: sum{i in OPERACION} A[i,j] * X[i] <= B[j];



