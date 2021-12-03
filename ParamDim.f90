
!     ******************************************************
!     Se Definen los parametros maximos de dimensionamiento
!     ******************************************************

      integer, parameter:: maxram  = 18000 ! Número máximo de ramas electricas
      
      integer, parameter::  maxgen  = 100 ! Número máximo de generadores electricos
       
      integer, parameter:: maxcar  = 10000 ! Número máximo de cargas
      
      integer, parameter::  maxgruram  = 100 ! Número máximo de grupos de ramas
      
      integer, parameter:: maxelegruram  = 500 ! Número máximo de elementos de grupos de ramas
      
      integer, parameter::  maxramnod  = 20 ! Número máximo de ramas por nodo
      
      integer, parameter::  maxres  = maxnod + 10 ! Número máximo de restricciones
      
      integer, parameter::  maxvar  = maxnod + 10 ! Número máximo de variables del problema lineal de flujos de DC
      
      integer, parameter:: maxele  = 10*maxnod*maxramnod ! Número máximo de elementos no cero en matriz jacobiana de flujos de carga
      
      integer, parameter::  maxeleybus = maxnod*maxramnod ! Número máximo de elementos de ybus
     
      integer, parameter:: maxisl = 5000 ! Número máximo de islas eléctricas

      integer, parameter:: maxregpre = 100 ! Número máximo de regiones de precios

      integer, parameter:: maxzoncar = 150 ! Número máximo de zonas de carga

      integer, parameter:: maxnodzoncar = 500 ! Número máximo de nodos por zonas de carga
      integer, parameter:: maxare = 15 ! Número máximo de areas
