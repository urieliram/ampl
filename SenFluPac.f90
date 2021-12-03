!
! Modulos para el calculo de sensibilidades de flujos de potencia activa
!
! Se utilizan rutinas del CPLEX para resolver el problema lineal
!  
! -----------------------------------------------------------------------
! Subrutina que calcula flujos de potencia activa sin perdidas          *
!                                                                       *
! Instituto de investigaciones Electricas                               *
! Gerencia de analisis de redes                                         *
! Division de sistemas electricos                                       *
!                                                                       *
! Julio del 2015                                                        *
! -----------------------------------------------------------------------

Subroutine SenFluFac ( rama, seflno )

Use ParAUHE, only: maxnod

Use ParGloRed, only: nmnod, inanod, inrnod, pcanod,  &
                     maxres, maxele, pgenod, vecsol

Implicit none

integer ierror

integer i, rama

real*8 seflno ( maxnod )

real*8 objf

! Cambia lado derecho de flujos de potencia activa
call PreLadoDer_SenFluPac ( rama )

! Resulve el problema de flujos con el problema lineal
call EjecutaFluPacb ( objf, ierror )

! Guarda solución de sensibilidades de flujos ante inyecciones nodales
seflno = 0.0
do i = 1, nmnod
   seflno(inanod(i)) = vecsol(i)
enddo

return

end
    
! -----------------------------------------------------------------------
! Subrutina que calcula flujos de potencia activa sin perdidas          *
!                                                                       *
! Instituto de investigaciones Electricas                               *
! Gerencia de analisis de redes                                         *
! Division de sistemas electricos                                       *
!                                                                       *
! Marzo del 2012                                                        *
! -----------------------------------------------------------------------
Subroutine SenfluPacIni

Use ParGloRed, only: nmnod, inanod, inrnod, pcanod,  &
                     maxnod, maxres, maxele, pgenod, vecsol

Implicit none

integer ierror

integer rama

real*8 objf

rama = 1
call PreparaSenFluPAC 

! Prepara lado derecho
call PreLadoDer_SenFluPac ( rama ) 

! Resulve el problema de flujos con el problema lineal
call EjecutaFluPAC ( objf, ierror )

! Guarda solución de angulos de voltaje
!teta = 0.0
!do i = 1, nmnod
!   teta(inanod(i)) = vecsol(i)
!enddo

return

end
    
! ---------------------------------------------------------------------
! Subrutina que prepara informacion del problema flujos optimos de DC *
! caso base                                                           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero del 2009                                                      *
! ---------------------------------------------------------------------

Subroutine PreparaSenFluPAC 

Use ParGloRed

Implicit none

  ! Prepara matriz de restricciones
  call PreMatPAC 

  ! Prepara coeficientes de funcion objetivo y cotas de variables
  call PreCoefCotSenFlu 

return

end


! ---------------------------------------------------------------------
! Subrutina que prepara  coeficientes de la funcion objetivo y cotas  *
! simples del problema lineal                                         *
!                                                                     *
!  - Unicamente potencia activa ( flujos de DC )                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre del 2009                                                    *
! ---------------------------------------------------------------------

Subroutine PreCoefCotSenFlu 

Use ParGloRed

use cplex_cons, only:   CPX_INFBOUND

Implicit none

Integer k, ind


! Inicializa tipo de variables
tipvar = 'C'

! Inicializa coeficientes del la funcion objetivo
coefunobj = 0.0

! Inicializa cotas superiores e inferiores
cotsup = 0.0
cotinf = 0.0

coefunobj = 0.0

! Determina el numero de variables del problema lineal
numvar = nmnod  

! Límites de angulos de voltaje
do k = 1, nmnod
  ind = k 
  cotsup(ind) = CPX_INFBOUND
  cotinf(ind) = - CPX_INFBOUND 
enddo

return

end

! ---------------------------------------------------------------------
! Subrutina que prepara  lados derechos del problema lineal           *
!                                                                     *
!  - Unicamente potencia activa (Flujos de DC)                        *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre del 2009                                                    *
! ---------------------------------------------------------------------

Subroutine PreLadoDer_SenFluPac ( rama )

Use ParAUHE, only: NumNodos

Use ParGloRed

Implicit none

integer i, k, rama

! Inicializa tipo de restricciones
tipres = 'E'

! Inicializa vector de lados derechos
ladder = 0.0
        
i = oriram(rama) 
k = desram(rama) 
if ( inrnod(i) .ne. 0 ) then
   ladder( inrnod(i) ) = suceptram(rama)
endif
if ( inrnod(k) .ne. 0 ) then
   ladder( inrnod(k) ) = -suceptram(rama)
endif

return

end
