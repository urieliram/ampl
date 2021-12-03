!
! Modulos para el calculo de flujos de potencia activa con perdidas en los extremos
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
Subroutine FlujosPac ( teta, per_nod, demtot  )

Use ParGloRed, only: nmnod, inanod, inrnod, pcanod,  &
                     maxnod, maxres, maxele, pgenod, vecsol, maxram

Implicit none

integer ierror

integer i

real*8 teta, per_nod

real*8 demtot

real*8 objf

dimension teta ( maxnod )
dimension per_nod ( maxnod )


! Cambia lado derecho de flujos de potencia activa
call PreLadoDer_PAC ( per_nod )

! Calcula la demanda total
demtot = 0.0
do i = 1, nmnod
  demtot = demtot + pcanod(inanod(i)) 
enddo

! Resulve el problema de flujos con el problema lineal
call EjecutaFluPACb ( objf, ierror )

! Guarda solución de angulos de voltaje
teta = 0.0
do i = 1, nmnod
   teta(inanod(i)) = vecsol(i)
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
! Julio del 2015                                                        *
! -----------------------------------------------------------------------
Subroutine FlujosPacIni ( teta, per_nod, demtot  )

Use ParGloRed, only: nmnod, inanod, inrnod, pcanod,  &
                     maxnod, maxres, maxele, pgenod, vecsol

Implicit none

integer ierror

integer i

real*8 teta, per_nod

real*8 demtot

real*8 objf

dimension teta ( maxnod )
dimension per_nod ( maxnod )

call PreparaFluPAC 

! Prepara lado derecho
call PreLadoDer_PAC ( per_nod ) 

! Calcula la demanda total
demtot = 0.0
do i = 1, nmnod
  demtot = demtot + pcanod(inanod(i)) 
enddo

! Resulve el problema de flujos con el problema lineal
call EjecutaFluPAC ( objf, ierror )

! Guarda solución de angulos de voltaje
teta = 0.0
do i = 1, nmnod
   teta(inanod(i)) = vecsol(i)
enddo

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
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine PreparaFluPAC 

Use ParGloRed

Implicit none

  ! Prepara matriz de restricciones
  call PreMatPAC 

  ! Prepara coeficientes de funcion objetivo y cotas de variables
  call PreCoefCotPAC 

return

end


! ---------------------------------------------------------------------
! Subrutina que prepara matriz de restricciones del problema lineal   *
!                                                                     *
!  - Unicamente potencia activa ( flujos de DC )                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine PreMatPAC 

Use ParAUHE, only: NumNodos

Use ParGloRed

Implicit none

Integer i, j, k, m, rama, rama2, ind

integer  ini, ifin

Integer iele, ires

Integer lk, lk2

real*8 deltakmsum, deltakdiag

integer indnodm( 2*maxramnod )

real*8 bkm ( 2*maxramnod )

! Inicializa contador de restriciones
ires = 0

! Inicializa contador de elementos diferentes de cero
iele = 0

! ..............................................................
! Incluye las restricciones de balance nodal de potencia activa
! ..............................................................
do i = 1, nmnod

     ires = ires + 1
!    * Genera los elementos debidos a ramas conectadas a nodo k *

     ind = 0
     k = inanod(i)

!    Inicializa variables auxiliares de elementos k-k
     deltakdiag = 0.0

     lk = incnod(k)
     do while ( lk .ne. 0 )
       m = inbusm(lk)

!      Inicializa variables auxiliares de elementos k-m
       deltakmsum = 0.0

!      derivadas de inyecciones de potencia activa con respecto
!      al angulo de voltaje
       rama = (lk+1)/2
	   if ( disram(rama) .ge. 1 ) then
       deltakmsum = deltakmsum - suceptram(rama)
	   deltakdiag = deltakdiag + suceptram(rama)

       endif
!      ----------------------------------------------
!      * Incluye la ramas en paralelo a la rama k-m *
!      ----------------------------------------------
       lk2 = siguem(lk)
       do while ( lk2 .ne. 0 )
         if ( inbusm(lk2) .ne. m ) then
           lk2 = 0
         else 
!          derivadas de inyecciones de potencia activa con respecto
!          al angulo de voltaje
           rama2 = (lk2+1)/2
		   if ( disram(rama2) .ge. 1 ) then 
           deltakmsum = deltakmsum - suceptram(rama2)            
           deltakdiag = deltakdiag + suceptram(rama2) 

           endif		   
             
           lk = lk2
           lk2 = siguem(lk)
         endif
       enddo

!      Guarda elementos asociados a nodo m (angulo de voltaje).
       if ( inrnod(m) .ne. 0 ) then
         ind = ind + 1
         Indnodm(ind) = inrnod(m)
	     bkm(ind) = deltakmsum 
       endif

       lk = siguem(lk)
     enddo

!    Elemento de la diagonal k-esima (angulo de voltaje)
     if ( inrnod(k) .ne. 0 ) then
       ind = ind + 1
       Indnodm(ind) = inrnod(k)
	   bkm(ind) = deltakdiag 
     endif

!    Ordena los elementos en orden ascendente de indice de columna
     Call OrdVecEnt ( ind, Indnodm, bkm )


!    Guarda elementos en los arreglos de la matriz de coeficientes
	 matiniren(i) = iele + 1
     do j = 1, ind
	   if ( dabs ( bkm(j) ) .ne. 0.0 ) then
	     iele = iele + 1 
         matval (iele ) = bkm(j)
	     matjcol(iele ) = indnodm(j)
	   endif
	 enddo

enddo 

! Guarda informacion en renglon numres + 1
matiniren(ires+1) = iele + 1

! Determina el numero de restricciones del problema lineal
numres = ires  

! recorre indices para hacelo compatible con cplex 
! ( arreglos inician en cero)

do i = 1, numres
! renumera columnas
  ini = matiniren(i)
  ifin = matiniren(i+1) - 1
  do j = ini, ifin
    matjcol(j) = matjcol(j) - 1
  enddo
! recorre indices de renglon
  matiniren(i) = matiniren(i) - 1
enddo
! Recorre informacion en renglon numres + 1
matiniren(numres+1) = matiniren(numres+1) - 1


! Guarda informacion de numero de elementos distintos de cero
numele = iele 


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
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine PreCoefCotPAC 

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
  cotinf(ind) = -CPX_INFBOUND  
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
! Julio del 2015                                                    *
! ---------------------------------------------------------------------

Subroutine PreLadoDer_PAC ( per_nod )

Use ParAUHE, only: NumNodos

Use ParGloRed

Implicit none

integer i

real*8 per_nod ( maxnod )


! Inicializa tipo de restricciones
tipres = 'E'

! Inicializa vector de lados derechos
ladder = 0.0

! informacion asociada a ecuacion de balance de potencia activa
do i = 1, nmnod
! Incluye la demanda de potencia activa
  ladder(i) = -pcanod(inanod(i)) - per_nod(inanod(i)) + pgenod(inanod(i))
enddo


return

end

! ---------------------------------------------------------------------
! Ejecuta flujos optimos para el caso base                            *
! usa los modulos de optimizacion del cplex                           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine EjecutaFluPAC ( objf, ierror )

Use ParAUHE, only: enb, bmensaje

Use ParGloRed, only: llp, vecsol, dualsol, yacsol, maxres

Use symtypes

!Use cplex_ifaces, only: CPXprimopt  , CPXdualopt, CPXsolution  ,      & !Windows (comentarizar para Linux)
!                        CPXwriteprob, CPXfreeprob  ,      & !Windows (comentarizar para Linux)
!                        CPXchgprobtype, CPXsetintparam      !Windows (comentarizar para Linux)

use cplex_cons, only:   CPX_ON,  CPX_PARAM_SCRIND, CPX_OFF, CPXPROB_LP


implicit none

integer                 CPXprimopt  , CPXdualopt, CPXsolution  ,      & !Linux (comentarizar para Windows)
                        CPXwriteprob, CPXfreeprob  ,      & !Linux (comentarizar para Windows)
                        CPXchgprobtype, CPXsetintparam      !Linux (comentarizar para Windows)
              
!  LP variable pointers

integer status

integer solstat

integer ierror, ibanbit

real*8 objf

real*8 slack ( maxres )

CHARACTER fecha_Ej*19

data status  / 0 /

llp = 0
ibanbit = 1
ierror = 0

!............................................................
! prepara datos de flujos optimos caso base (modelo lineal) :
!............................................................
! 
call InicializaFluPAC ( status )

! Ejecuta flujos optimos caso base (modelo lineal)

! Llamar a cplex

!  Write a copy of the problem to a file
!   status = CPXwriteprob (env , llp , 'fluopt.lp', 'LP')

! Problem is set to continues type
   status = CPXchgprobtype (enb, llp, CPXPROB_LP)
   if ( status .ne. 0 ) then
      write (*,*) ' Failed to set MFO problem to its initial type, ''status'' = ', status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL INICIALIZAR CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

!  Optimize the problem and obtain the solution
   status = CPXprimopt (enb, llp);
   ! status = CPXdualopt (enb, llp);

   if ( status .eq. 0 ) then
      status = CPXsolution ( enb, llp, solstat, objf, vecsol, dualsol, slack, yacsol )
   endif

   if ( status .ne. 0 ) then
      write (*,*) ' Failed to obtain MFO solution, ''status'' = ',  &
                    status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL OBTENER SOLUCION CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

!  Verifica si la solucion obtenida es optima
   if ( status .eq. 0 .and. solstat .eq. 1 ) then
     ierror = 0
   else
     ierror = 1
   endif
 
!  Free up the problem as allocated by CPXcreateprob, if necessary

   ! status = CPXfreeprob (enb, llp)
   if ( status .ne. 0 ) then
      write (*,*) ' Failed to free up the MFO problem, ''status'' = ', & 
               status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL LIBERAR CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

   Continue

!    status = CPXsetintparam (enb, CPX_PARAM_SCRIND, CPX_ON)

return

end
    
! ---------------------------------------------------------------------
! Ejecuta flujos de carga cambiando solo los lados derechos           *
! usa los modulos de optimizacion del cplex                           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine EjecutaFluPACb ( objf, ierror )

Use ParAUHE, only: enb, bmensaje

Use ParGloRed, only: nmnod, llp, numres, ladder, vecsol, dualsol, yacsol, maxres

Use symtypes

!Use cplex_ifaces, only: CPXprimopt  ,CPXdualopt  , CPXsolution  ,      & !Windows (comentarizar para Linux)
!                        CPXwriteprob, CPXfreeprob  ,      & !Windows (comentarizar para Linux)
!                        CPXchgprobtype, CPXchgrhs           !Windows (comentarizar para Linux)

use cplex_cons, only:   CPX_ON,  CPX_PARAM_SCRIND, CPX_OFF, CPXPROB_LP


implicit none

integer                 CPXprimopt  , CPXdualopt  , CPXsolution  ,      & !Linux (comentarizar para Windows)
                        CPXwriteprob, CPXfreeprob  ,      & !Linux (comentarizar para Windows)
                        CPXchgprobtype, CPXchgrhs           !Linux (comentarizar para Windows)
              
!  LP variable pointers

integer status

integer solstat, i

integer ierror, ibanbit

real*8 objf

real*8 slack ( maxres )

CHARACTER fecha_Ej*19

integer indres ( maxres )

data status  / 0 /

ibanbit = 1
ierror = 0

   do i = 1, nmnod
       indres(i) = i -1
   enddo
   
   status = CPXchgrhs ( enb, llp, numres, indres, ladder )

   if ( status .eq. 0 ) then
      !  Optimize the problem and obtain the solution
      status = CPXprimopt (enb, llp);
      ! status = CPXdualopt (enb, llp);
   endif


   if ( status .eq. 0 ) then
      status = CPXsolution ( enb, llp, solstat, objf, vecsol, dualsol, slack, yacsol )
   endif

   if ( status .ne. 0 ) then
      write (*,*) ' Failed to obtain MFO solution, ''status'' = ',  &
                    status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL OBTENER SOLUCION CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

!  Verifica si la solucion obtenida es optima
   if ( status .eq. 0 .and. solstat .eq. 1 ) then
     ierror = 0
   else
     ierror = 1
   endif
 
!  Free up the problem as allocated by CPXcreateprob, if necessary

   ! status = CPXfreeprob (enb, llp)
   if ( status .ne. 0 ) then
      write (*,*) ' Failed to free up the MFO problem, ''status'' = ', & 
               status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL OBTENER SOLUCION CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
 !     algoritmo no termina bien
      call SalidaError
     stop
   end if

   Continue

return

end

! ---------------------------------------------------------------------
! Subrutina que inicilaiza el problema lineal de flujos optimos       *
! para ser resuelto utilizando el modulo de optimizacion "Cplex"      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine InicializaFluPAC ( status )

Use ParAUHE, only: enb, bmensaje

Use ParGloRed 

use symtypes

!use cplex_ifaces, only: CPXcreateprob, CPXsetintparam,             & !Windows (comentarizar para Linux)
!                        CPXwriteprob, CPXchgobjsen, CPXsetdblparam   !Windows (comentarizar para Linux)
!
!use cplex_forman, only: addrowsCPX, newcolsCPX                      !Windows (comentarizar para Linux)

use cplex_cons, only:   CPX_MIN,  CPX_PARAM_SCRIND, CPX_OFF, CPX_PARAM_EPRHS, CPX_ON

Implicit none

integer                 CPXcreateprob, CPXsetintparam,             & !Linux (comentarizar para Windows)
                        CPXwriteprob, CPXchgobjsen   ,  CPXnewcols,& !Linux (comentarizar para Windows)
                        CPXaddrows                                   !Linux (comentarizar para Windows)

character*15 probname

integer status, i, j, ibanbit, ierror

character*1 ctype ( maxvar )
character*1 name_array( maxres )   !Linux (comentarizar para Windows)
integer name( maxres )             !Linux (comentarizar para Windows)

CHARACTER fecha_Ej*19
! character*16 aaux


! Define integer constant for NULL

integer(IL), parameter :: NULL = 0

ibanbit = 1
ierror = 0

!status = CPXsetdblparam (enb, CPX_PARAM_EPRHS, 1e-9)

!  Turn on output to the screen

   status = CPXsetintparam (enb, CPX_PARAM_SCRIND, CPX_OFF)

   if ( status .ne. 0 ) then
      write (*,*) ' Failure to turn on screen indicator - Error = ', &
                    status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL CAMBIAR PARAMETROS CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

!  Fill in the data for the problem.  Note that since the space for
!  the data already exists in local variables, we pass the arrays
!  directly to the routine to fill in the data structures.

!  Create the problem.
!
   probname = 'fluopt'
!
   llp = CPXcreateprob (enb, status, probname)
!
!  A returned pointer of NULL may mean that not enough memory
!  was available or there was some other problem.  In the case of
!  failure, an error message will have been written to the error
!  channel from inside CPLEX.  In this example, the setting of
!  the parameter CPX_PARAM_SCRIND causes the error message to
!  appear on stdout.  Note that most CPLEX routines return
!  an error code to indicate the reason for failure.  An error
!  message is always printed to the error channel in these cases
!  as well.
!
   if ( llp .eq. NULL ) then
      write (*,*) ' Failed to create MFO LP'
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL CREAR PROBLEMA CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

!  Minimization problem
   status =  CPXchgobjsen (enb, llp, CPX_MIN)

   do i = 1, numvar                               !Linux (comentarizar para Windows)
      name_array(i) = name_array(i)//char(0)      !Linux (comentarizar para Windows)
      name(i) = loc( name_array(i) )              !Linux (comentarizar para Windows)
   end do                                         !Linux (comentarizar para Windows)

   ! Tipos de variables
   do j = 1 , numvar
      ctype ( j ) = 'C'
   enddo

!  Columns are added
   status = CPXnewcols (enb, llp, numvar, coefunobj, cotinf, cotsup, ctype, name)  !Linux (comentarizar para Windows)
!   status = newcolsCPX (enb, llp, numvar, coefunobj, cotinf, cotsup, ctype)       !Windows (comentarizar para Linux)
   if ( status .ne. 0 ) then
      write (*,*) ' Failed to add new column MFO, ''status'' = ', status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL AGREGAR COLUMNAS CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

   do i = 1, numres                               !Linux (comentarizar para Windows)
      name_array(i) = name_array(i)//char(0)      !Linux (comentarizar para Windows)
      name(i) = loc( name_array(i) )              !Linux (comentarizar para Windows)
   end do                                         !Linux (comentarizar para Windows)
   
!  Add the constraints
   status = CPXaddrows ( enb, llp, 0, numres, numele, ladder, tipres,   &   !Linux (comentarizar para Windows)
                         matiniren, matjcol, matval, name, name )           !Linux (comentarizar para Windows)
!   status = addrowsCPX ( enb, llp, 0, numres, numele, ladder, tipres, &      !Windows (comentarizar para Linux)
!                         matiniren, matjcol, matval )                        !Windows (comentarizar para Linux)


   if ( status .ne. 0 ) then
      write (*,*) ' Failed to add new rows MFO, ''status'' = ', status
      Call FechaEjecucion (fecha_Ej)
      bmensaje = fecha_Ej//' MDA101 ERROR AL AGREGAR RENGLONES CPLEX'
      Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      write(*,*) '1'
!     algoritmo no termina bien
      call SalidaError
      stop
   end if

!  Write a copy of the problem to a file
!   status = CPXwriteprob (enb, llp, 'fluopt_1.lp', 'LP')


  Continue

Return

End

