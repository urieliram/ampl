## Una empresa se dedica a la producci�n y distribuci�n de pinturas para interiores y exteriores;
## se emplean dos materias primas MP, y MP, para la producci�n de las pinturas. La disponibilidad
## m�xima de MP1 es de 20 toneladas diarias y la de MP2 es de 9 toneladas por d�a. Los requerimientos
## diarios de materia prima por tonelada son los siguientes:
##                       COMBINACION REQUERIDA
##                       INTERIOR     EXTERIOR   DISPONIBILIDAD
##                 MP1   3            7          20
##                 MP2   4            1          9
## Utilidad por tonelada 100000       300000

## El estudio de mercado ha establecido que la demanda diaria de pintura para interiores no puede
## ser mayor que la pintura para exteriores en m�s de una tonelada. Adem�s, el estudio se�ala que la
## demanda m�xima de pintura para interiores est� limitada a dos toneladas por d�a. �Cu�nta pintura para
## interiores y exteriores debe producir la empresa todos los d�as para maximizar el ingreso bruto?

## X1 : N�mero de toneladas diarias producidas de pintura para interiores
## X2 : Cantidad de toneladas diarias producidas de pintura para exteriores

## MAX Z = 100000 X1 + 300000 X2
## Sujeta a:

## 3 X1 + 7 X2 <= 20
## 4 X1 +   X2 <= 9

##   X1        <= 2

## - X1 +   X2 <= 1

##   X1 ,   X2 >= 0

set TIPOS;
set MATERIAL;

param utilidad{TIPOS};             # INTERIOR  y  EXTERIOR
param combinacion{TIPOS,MATERIAL}; 
param disponibilidad{MATERIAL};    

 # Variables del Problema, cantidad a producir de INTERIOR  y  EXTERIOR
 var X {TIPOS} >= 0 ;
 
 # Funcion objetivo
 maximize Z:
 sum{i in TIPOS} utilidad[i] * X[i];
 
 # Restriccion de Disponibilidad de Recursos
 subject to Recursos {j in MATERIAL}:
 sum{i in TIPOS} combinacion[i,j] * X[i] <= disponibilidad[j]; 
 
## La demanda diaria de pintura para INTERIOR no puede ser mayor que la pintura para EXTERIOR en m�s de una tonelada
 subject to Condicion1:
    #X['EXTERIOR'] <= 1 - X ;
   - X['INTERIOR'] +   X['EXTERIOR'] <= 1;

## demanda m�xima de pintura para INTERIOR est� limitada a dos toneladas por d�a
 subject to Condicion2:
   X['INTERIOR'] <= 2 ;