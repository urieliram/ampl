
!     ******************************************************
!     Se Definen los parametros maximos de dimensionamiento
!     ******************************************************

      integer, parameter:: maxram  = 18000 ! N�mero m�ximo de ramas electricas
      
      integer, parameter::  maxgen  = 100 ! N�mero m�ximo de generadores electricos
       
      integer, parameter:: maxcar  = 10000 ! N�mero m�ximo de cargas
      
      integer, parameter::  maxgruram  = 100 ! N�mero m�ximo de grupos de ramas
      
      integer, parameter:: maxelegruram  = 500 ! N�mero m�ximo de elementos de grupos de ramas
      
      integer, parameter::  maxramnod  = 20 ! N�mero m�ximo de ramas por nodo
      
      integer, parameter::  maxres  = maxnod + 10 ! N�mero m�ximo de restricciones
      
      integer, parameter::  maxvar  = maxnod + 10 ! N�mero m�ximo de variables del problema lineal de flujos de DC
      
      integer, parameter:: maxele  = 10*maxnod*maxramnod ! N�mero m�ximo de elementos no cero en matriz jacobiana de flujos de carga
      
      integer, parameter::  maxeleybus = maxnod*maxramnod ! N�mero m�ximo de elementos de ybus
     
      integer, parameter:: maxisl = 5000 ! N�mero m�ximo de islas el�ctricas

      integer, parameter:: maxregpre = 100 ! N�mero m�ximo de regiones de precios

      integer, parameter:: maxzoncar = 150 ! N�mero m�ximo de zonas de carga

      integer, parameter:: maxnodzoncar = 500 ! N�mero m�ximo de nodos por zonas de carga
      integer, parameter:: maxare = 15 ! N�mero m�ximo de areas
