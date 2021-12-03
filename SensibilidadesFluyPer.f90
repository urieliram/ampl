
!//////////////////////////////////////////////////////////////////////////////// 
!                                                                               !
!                                                                               !
!                                                                               !
!                                                                               !
!                                                                               !
!                                                                               !
!                                                                               !
!                                                                               !
!//////////////////////////////////////////////////////////////////////////////// 
! *******************************
! Inicializa variables globales
! *******************************
SUBROUTINE IniVarGlo

Use ParAUHE, only: rut_dat_1, BASE 
Use ParGloRed, only: path_result,basmva, banfactorizacion, banfactorizacionp

Implicit none

basmva = BASE

banfactorizacion = .true.
banfactorizacionp = .true.

END SUBROUTINE IniVarGlo
    
! ********************************************************************************
! Se preparan las sensibilidades de flujo en ramas debidas a inyecciones nodales *
! ********************************************************************************
Subroutine CalculaSensibilidadesFlujos(isistema, intervalo, bandera, ite)
 
Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, enb
Use ParGloRed, only: maxnod, maxres, maxele, basMva, nmgruram, llp, Banfactorizacion
!Use cplex_ifaces, only: CPXfreeprob  !Windows (comentarizar para Linux)
                       

Implicit none

integer     CPXfreeprob     !Linux (comentarizar para Windows)

integer intervalo, isistema, icambia, bandera, ite, status
    
character*19 fecha_Ej

    Call FechaEjecucion (fecha_Ej)
    ! write ( 888, * ) "inicia intervalo" ,intervalo, fecha_Ej
!   Verifica cuantos grupos de ramas hay en el subsistema
    call PreparaGruposRamasIsla 
    
    if ( nmgruram .gt. 0 ) then
       
       ! Prepara red por intervalo
       call PreparaRedIntervalo ( intervalo, icambia )
       
       ! Prepara sensibilidades de flujos de grupos de ramas por intervalo
       if ( icambia .ne. 0 .or. intervalo .eq. 1) then
          call CalSnsFluGruRam 
       endif
       
       ! si se desea actualizar el lado derecho de las restricciones de flujo
       if ( bandera .eq. 1 .or. ( bandera .eq. 0 .and. ite .eq. 0 ) ) then
           ! Actualiza el termino constante asociados a restricciones de transmisión
           call CalculaTerConstanteFluGruRam ( intervalo )
       endif
       
    endif

    if ( .not. Banfactorizacion ) then
        ! Libera el problema de programación lineal del ambiente CPLEX
        status = CPXfreeprob (enb, llp)
    endif

    Call FechaEjecucion (fecha_Ej)
    ! write ( 888, * ) "termina intervalo" , intervalo, fecha_Ej
           
return
end
    
    
! ---------------------------------------------------------------------
! Subrutina que prepara informacion de restricciones de grupos de     *
! ramas restringidas por subsistema                                   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! ---------------------------------------------------------------------

Subroutine PreparaGruposRamasIsla 

Use ParAUHE, only: EstadoIsla

Use ParGloRed, only: numgruram, nmgruram, inagruram, ApuEleGruRam, LisEleGruRam, oriram, sisnod

Implicit none

integer i, j, rama

nmgruram = 0
do i = 1, numgruram
    ! Verifica que el primer elemento esta en la isla a evaluar
    do j = ApuEleGruRam(i), ApuEleGruRam(i)
        rama = LisEleGruRam(j)
        if ( rama .ne. 0 ) then
            if ( EstadoIsla(sisnod(oriram(rama))) .eq. 1 ) then
               nmgruram = nmgruram + 1
               inagruram(nmgruram) = i
            endif
        endif
    enddo
enddo


return

end
    
! ---------------------------------------------------------------------
! Subrutina que calcula las sensibilidades de flujos con respectoa a  *
! a inyencciones nodales  a partir de la matriz de flujos de DC       *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Junio del 2010                                                      *
! ---------------------------------------------------------------------
Subroutine CalSnsFluGruRam 

Use ParAUHE, only: NumNodos

Use ParGloRed, only: numgruram, nmgruram, inagruram, ApuEleGruRam, LisEleGruRam, SentEleGruRam, &
                     SnsGruRarInyNod,  &
                     SnsEleGruRamInyNod,  &
                     maxres, maxele, &
                     numram, disram, maxnod, inrnod, inrednodsis, nomram, disram, nomnod, disnod, Banfactorizacion
                     

Implicit none

integer nren, i, j, k

integer nodo

integer igru

integer rama

integer iren, jcol

real*8 matdc, diagdc

integer iu,ju

real*8 umat, dimat

real*8 seflno ( maxnod )

! Matriz sin factorizar
dimension iren ( maxres )
dimension jcol ( maxele )
dimension diagdc ( maxres )
dimension matdc ( maxele )

! Matriz factorizada
dimension iu ( maxres )
dimension ju ( maxele )
dimension umat ( maxele )
dimension dimat ( maxres )

logical falfac

integer diagcer

if ( Banfactorizacion ) then
    nren = 0; iren = 0; jcol = 0; matdc = 0.0; diagdc = 0.0

    ! Prepar matriz de admitancias de flujos de DC
    call PreMatFluDC ( nren, iren, jcol, matdc, diagdc )

    ! Factoriza matriz de admitancias de flujos de DC
    call facriz ( nren, iren, jcol, matdc, diagdc, iu, ju, umat, dimat, falfac, diagcer )

else
    call SenfluPacIni 
endif



! Inicializa sensisbilidades y termino constante asociados a grupos de ramas y elementos
! de grupos de ramas
do i = 1, nmgruram

   igru = inagruram(i)
   !............................................................
   ! Sensibilidades asociadas a grupos de ramas restringidas
   !............................................................

   do k = 1, NumNodos
     if ( inrednodsis(k) .ne. 0 ) then
        SnsGruRarInyNod ( igru, k ) = 0.0
     endif
   enddo
  
 
   !......................................................................................
   ! Sensibilidaes asociadas a elementos que forman parte de grupos de ramas restringidas
   !.......................................................................................
   do j = ApuEleGruRam(igru), ApuEleGruRam(igru+1)-1

        do k = 1, NumNodos
           if ( inrednodsis(k) .ne. 0 ) then
              SnsEleGruRamInyNod ( j, k ) = 0.0
           endif
        enddo

   enddo
  
enddo

!...................................................................................
! Calcula las sensibilidades de las ramas que forman parte de grupos de ramas
! restringidas y grupos de ramas restringidas
!...................................................................................
do i = 1, nmgruram
    igru = inagruram(i)
    do j = ApuEleGruRam(igru), ApuEleGruRam(igru+1)-1
        rama = LisEleGruRam(j)
        if ( disram(rama) .eq. 1 ) then
           call snsflu ( iu, ju, umat, dimat, rama, seflno )

           ! Guarda información de sensibilidades de nodos de generación de oferta de rango simple
           do nodo = 1, NumNodos
              if ( inrnod(nodo) .ne. 0 ) then
	              SnsEleGruRamInyNod ( j, nodo ) = seflno(nodo)
     	          SnsGruRarInyNod ( igru, nodo ) = SnsGruRarInyNod ( igru, nodo ) + &
                                                   seflno(nodo) * SentEleGruRam(j)
              endif
           enddo
        endif
    enddo

enddo
 
100 format ( 3(x, f9.5), 2(x, a20) )

return

end
    
! ---------------------------------------------------------------------
! Subrutina que calcula el termino constante de las restricciones de  *
! transmision de potencia activa en grupos de enlaces                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Junio del 2010                                                      *
! ---------------------------------------------------------------------
Subroutine CalculaTerConstanteFluGruRam ( intervalo )

Use ParAUHE, only: NumUniNPR, nodonpr, PotNPR,   &
                  NumOferDem, nodocar, DemFija, NumNodos, NumNodInt, &
                  NodoInt, PotNodInt, Tempnodocar, facdistcar, &
                  NoNodDisCar, NoNodDisNPR, Tempnodonpr, facdistnpr

Use ParGloRed, only: numgruram, nmgruram, inagruram, ApuEleGruRam, LisEleGruRam, SentEleGruRam, &
                     SnsGruRarInyNod, CoeSnsGruRar, &
                     SnsEleGruRamInyNod, CoeSnsRar, &
                     maxres, maxele, &
                     numram, disram, maxnod, inrnod, perpacnod
                     
Implicit none

integer intervalo 

integer i, j, k, rama, NoDi

integer nodo

integer igru

!...................................................................................
! Calcula termino constante  de sensibilidades de las ramas que forman 
! parte de grupos de ramas restringidas
!...................................................................................
do i = 1, nmgruram
    
    igru = inagruram(i)
    
    do j = ApuEleGruRam(igru), ApuEleGruRam(igru+1)-1
        rama = LisEleGruRam(j)
        ! Inicializa coeficiente
        CoeSnsRar ( j, intervalo ) = 0.0
        if ( disram(rama) .eq. 1 ) then


        !   Agrega termino asociado a generaciones no programables
            do k = 1, NumUniNPR ! JLC
                do NoDi = 1, NoNodDisNPR ( k, intervalo )
                    nodo = Tempnodonpr ( k, NoDi, intervalo )
                    if ( inrnod(nodo) .ne. 0 .and. abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1.0e-10  ) then
	                    CoeSnsRar ( j, intervalo ) = CoeSnsRar ( j, intervalo ) - &
                                          SnsEleGruRamInyNod ( j, nodo ) * PotNPR ( k, intervalo )*facdistnpr ( k, NoDi, intervalo )
                    endif
                end do
            enddo
        
            ! Agrega termino asociado a ofertas de demanda ( componente fija )
            do k = 1, NumOferDem  ! JLC
                do NoDi = 1, NoNodDisCar ( k, intervalo )
                    nodo = Tempnodocar ( k, NoDi, intervalo )
                    if ( inrnod(nodo) .ne. 0  .and. abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1.0e-10 ) then
                        CoeSnsRar ( j, intervalo ) = CoeSnsRar ( j, intervalo ) + SnsEleGruRamInyNod ( j, nodo )*DemFija ( k, intervalo )*facdistcar ( k, NoDi, intervalo )
                    endif
                enddo
            enddo

            !   Agrega termino asociado a potencias de intercambio 
            do k = 1, NumNodInt
                nodo = NodoInt(k, intervalo)
                if ( inrnod(nodo) .ne. 0  .and. abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1.0e-10 ) then
	                CoeSnsRar ( j, intervalo ) = CoeSnsRar ( j, intervalo ) + &
	                                  SnsEleGruRamInyNod ( j, nodo ) * PotNodInt ( k, intervalo )
                endif
            enddo

            ! Agrega termino asociado a perdidas de potencia activa en los nodos extremos
            do nodo = 1, NumNodos
                if ( inrnod(nodo) .ne. 0 .and.  abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1.0e-10 ) then
    	            CoeSnsRar ( j, intervalo ) = CoeSnsRar ( j, intervalo ) + &
    	                              SnsEleGruRamInyNod ( j, nodo ) * perpacnod ( nodo, intervalo )
                endif
            enddo
        endif

    enddo

enddo

!...................................................................................
! Calcula lado derecho de los grupos de ramas restringidos
!...................................................................................
do i = 1, nmgruram
        igru = inagruram(i)
    
        CoeSnsGruRar ( igru, intervalo  ) = 0.0
    
    !   Agrega termino asociado a generaciones no programables ( generación fija )
        do k = 1, NumUniNPR  ! JLC
            do NoDi = 1, NoNodDisNPR ( k , intervalo)
                nodo = Tempnodonpr ( k, NoDi, intervalo )
                if ( inrnod(nodo) .ne. 0 .and.  abs(SnsGruRarInyNod ( igru, nodo )) .gt. 1.0e-10 ) then
	               CoeSnsGruRar ( igru, intervalo  ) = CoeSnsGruRar ( igru, intervalo  ) - &
	                                        SnsGruRarInyNod ( igru, nodo ) * PotNPR ( k, intervalo )*facdistnpr ( k, NoDi, intervalo ) 
                endif
            enddo
	    enddo

    !   Agrega termino asociado a ofertas de demanda ( componente fija )
        do k = 1, NumOferDem
            do NoDi = 1, NoNodDisCar ( k, intervalo )
                nodo = Tempnodocar ( k, NoDi, intervalo )
                if ( inrnod(nodo) .ne. 0 .and. abs(SnsGruRarInyNod ( igru, nodo )) .gt. 1.0e-10) then
                    CoeSnsGruRar ( igru, intervalo  ) = CoeSnsGruRar ( igru, intervalo ) + SnsGruRarInyNod ( igru, nodo ) * DemFija ( k, intervalo )*facdistcar ( k, NoDi, intervalo )
                endif
            enddo
        enddo
        
        ! Agrega termino asociado a potencias de intercambio 
        do k = 1, NumNodInt
            nodo = NodoInt(k, intervalo)
            if ( inrnod(nodo) .ne. 0 .and. abs(SnsGruRarInyNod ( igru, nodo )) .gt. 1.0e-10 ) then
	           CoeSnsGruRar ( igru, intervalo  ) = CoeSnsGruRar ( igru, intervalo ) + &
	                                    SnsGruRarInyNod ( igru, nodo ) * PotNodInt ( k, intervalo ) 
            endif
        enddo

        ! Agrega termino asociado a perdidas de potencia activa en los nodos extremos 
        do nodo = 1, NumNodos
            if ( inrnod(nodo) .ne. 0 .and. abs(SnsGruRarInyNod ( igru, nodo )) .gt. 1.0e-10 ) then
	           CoeSnsGruRar ( igru, intervalo  ) = CoeSnsGruRar ( igru, intervalo ) + &
	                                    SnsGruRarInyNod ( igru, nodo ) * perpacnod ( nodo, intervalo ) 
            endif
	    enddo

enddo


100 format ( 3(x, f9.5), 2(x, a20) )

return

end

! **************************************************************
! *                                                            *
! * Instituto de investigaciones electricas                    *
! * Gerencia de analisis de redes                              *
! * Division de sistemas eléctricos                            *
! *                                                            *
! * objetivo:                                                  *
! *                                                            *
! * - calcula sensibilidad de flujo en lineas restringidas     *
! *   ante cambios de la potencia nodal                        *
! *                                                            *
! *                                                            *
! **************************************************************

Subroutine snsflu ( iu, ju, un, di, rama, seflno )
        
Use ParAUHE, only: maxnod, NumNodos

Use ParGloRed, only: oriram, desram, suceptram,  nmnod, inrnod, inanod, numisl, &
                             maxres, maxele, Banfactorizacion

implicit none

integer i, k, nren

integer rama

real*8 seflno

real*8 vlder, vsol

dimension seflno ( maxnod )

dimension vlder(maxres), vsol(maxres)

! Matriz factorizada
integer iu, ju

real*8 un, di

real*8 uno, cero

dimension iu ( maxres )
dimension ju ( maxele )

dimension un ( maxele )
dimension di ( maxres )

cero = 1.0D-07
uno = (1.0 - cero)

i = oriram(rama) 
k = desram(rama) 
    
! Se protege para cuando los dos nodos estan en otra isla
if ( inrnod(i) .eq. 0  .and. inrnod(k) .eq. 0 ) then
   seflno = 0.0; return
endif

if ( Banfactorizacion ) then
    ! ---------------------------
    ! * inicializa lado derecho *
    ! ---------------------------
    nren = nmnod

    vlder = 0.0
        
    i = oriram(rama) 
    k = desram(rama) 
    
    ! Se protege para cuando los dos nodos estan en otra isla
    if ( inrnod(i) .eq. 0  .and. inrnod(k) .eq. 0 ) then
        seflno = 0.0; return
    endif
    
    ! Verifica si alguno de ellos es el nodo slack
    if ( inrnod(i) .ne. 0 ) then
       vlder( inrnod(i) ) = suceptram(rama)
    endif
    if ( inrnod(k) .ne. 0 ) then
       vlder( inrnod(k) ) = -suceptram(rama)
    endif

    ! -------------------------------------------
    ! * resuelve sistema de ecuaciones lineales *
    ! -------------------------------------------
    call solfac ( nren, iu, ju, un, di, vlder, vsol )

    ! ------------------------------------------
    ! * guarda informacion de sensibilidad de  *
    ! * flujos por nodo                        *
    ! ------------------------------------------
    seflno = 0.0
    do k = 1, nmnod
       i = inanod(k)
       seflno(i) = vsol(k)
    !  ---------------------------------------------------
    !  * redondea valores de sensibilidades para reducir *
    !  * problemas de precision                          *
    !  ---------------------------------------------------     
       if ( seflno(i) .gt. 0.999999 ) then
        !  seflno(i) = 1.0
       else if ( seflno(i) .lt. -0.999999 ) then
        !  seflno(i) = -1.0
       else if ( abs(seflno(i)) .lt. 0.000001 ) then
        !  seflno(i) = 0.0
       endif
    enddo

else

    call SenFluFac ( rama, seflno )

endif

!  ---------------------------------------------------
!  * redondea valores de sensibilidades para reducir *
!  * problemas de precision                          *
!  ---------------------------------------------------     
do k = 1, nmnod
    i = inanod(k)
    if ( seflno(i) .gt. uno ) then
!        seflno(i) = 1.0
    else if ( seflno(i) .lt. -uno ) then
!        seflno(i) = -1.0
    else if ( abs(seflno(i)) .lt. cero ) then
!         seflno(i) = 0.0 
    endif
    
enddo

return

end

! -----------------------------------------------------------------------
! Subrutina que calcula las sensibilidades de perdidas con respecto  a  *
! a inyencciones nodales  a partir de la matriz de flujos de DC         *
!                                                                       *
! Instituto de investigaciones Electricas                               *
! Gerencia de analisis de redes                                         *
! Division de sistemas electricos                                       *
!                                                                       *
! Marzo del 2012                                                        *
! -----------------------------------------------------------------------
Subroutine CalSnsPerIntervalo ( sistema, ite )

Use ParAUHE, only: NTINTR, NumNodos, NumUniRC, NumUniRD, NumUNiHid, NumUniRE, NumUniNPR, NumNodInt, NumOferDem,  &
                  GENUNRC, GENUNRD, GENUNH, GENUNRE, PotNPR, PotNodInt, DemFija, NumModRD, NumCompXModo, &
                  NumCompRD, CompXModo, ListCompURD, ApunCompURD, GenCompXModo, nodorc, nodocompurd, nodocar, &
                  nodoh, nodounre, nodonpr, NodoInt, NumBloDem,  maxdem, maxint, Tempnodorc, facdistgen, &
                  Tempnodocar, NoNodDisCar, facdistcar, NoNodDisRC, enb, maxram

use ProblemaAUHE, only: INDDE, IDF, IEXC, xMILP, IARD, INIURDI, IADARD, IDBC

Use ParGloRed, only: nmnod, inanod, inrnod, NumNodSis, InAumNodSis, pcanod, pgenod, fluramint, numram, resram, perdramint, angvolnodint, &
                     maxnod, maxres, maxele, SnsPerNod, perpacnod, PerIntervalo, SnsPerIntIny, Banfactorizacion, Banfactorizacionp, llp, nomnod

!use cplex_ifaces, only: CPXfreeprob !Windows (comentarizar para Linux)


Implicit none

 integer   CPXfreeprob  !Linux (comentarizar para Windows)

integer sistema, intervalo, icambia, modo, NoDi, ite, ierror

integer nren, kd, i, u, nodo, d, s, n, status

integer iren, jcol, componente, componente_1

real*8 matdc, diagdc, coeficiente

integer iu,ju

real*8 umat, dimat

real*8 teta

real*8 snsperiny 

real*8 per_nod ( maxnod ), flu_pac ( maxram )

logical falfac

integer diagcer

real*8 perdidas, demanda, demtot

CHARACTER fecha_Ej*19

! Matriz sin factorizar
dimension iren ( maxres )
dimension jcol ( maxele )
dimension matdc ( maxele )
dimension diagdc ( maxres )

! Matriz factorizada
dimension iu ( maxres )
dimension ju ( maxele )
dimension umat ( maxele )
dimension dimat ( maxres )

! Sensibilidades
dimension snsperiny ( maxnod )

dimension teta ( maxnod )

! demandas
dimension demanda ( maxdem, maxint )


! Demanda total aceptada
! demandas
kd = IDBC
do d = 1 , NumOferDem
!   Para todos los intervalos
    do intervalo = 1 , NTINTR
        demanda ( d, intervalo ) = 0.0
!       para todos los segmentos de curva de ofertas de compra
        do s = 1, NumBloDem( d, intervalo )
!           coeficiente de segmento de compra
            demanda ( d, intervalo )  = demanda ( d, intervalo ) + xMILP ( kd )
            kd = kd + 1
        enddo
    enddo
enddo

Call FechaEjecucion (fecha_Ej)
! write ( 888, * ) "inicia sensibilidad de perdidas" , ",", fecha_Ej

do intervalo = 1, NTINTR
    
    ! Prepara red por intervalo
    call PreparaRedIntervalo ( intervalo, icambia )
    
    ! Prepara generación y demanda del intervalo
    call PreparaGenIntervalo ( sistema, intervalo )
       
    ! Prepara sensibilidades de flujos de grupos de ramas por intervalo
    if ( icambia .ne. 0 .or. intervalo .eq. 1) then
       ! Libera el problema lineal
       if ( intervalo .ne. 1 .and. .not. Banfactorizacionp ) then
          status = 0
          status = CPXfreeprob (enb, llp)
       endif
       if ( Banfactorizacionp ) then
           ! Prepar matriz de admitancias de flujos de DC
           call PreMatFluDC ( nren, iren, jcol, matdc, diagdc )

           ! Factoriza matriz de admitancias de flujos de DC
           call facriz ( nren, iren, jcol, matdc, diagdc, iu, ju, umat, dimat, falfac, diagcer )
       else
           call FlujosPacIni ( teta, per_nod, demtot  )
       endif
    endif

    !...................................................................................
    ! Ejecuta flujos de carga con perdidas
    !...................................................................................
    if ( Banfactorizacionp ) then
         call fludcper ( nren, iu, ju, umat, dimat, teta, perdidas, per_nod, flu_pac )
    else
        call fludcpera ( teta, perdidas, per_nod, flu_pac )
    endif      
    ! Asigna valores de perdidas de la isla y de los nodos a variables globales
    do i = 1, NumNodos
       perpacnod (i, intervalo) = per_nod(i)
       angvolnodint ( i, intervalo ) = teta (i)
    enddo
    PerIntervalo ( intervalo ) = perdidas

    ! Guarda valores de flujo de potencia activa de las ramas por intervalo
    do i = 1, numram
       fluramint (i, intervalo) = flu_pac(i)
       perdramint ( i, intervalo ) = resram(i)*flu_pac(i)**2
    enddo
    !...................................................................................
    ! Calcula las sensibilidades de perdidas ante inyecciones nodales
    !...................................................................................
    if ( Banfactorizacionp ) then
        call SnsPer ( iu, ju, umat, dimat, teta, snsperiny )
    else
        call SnsPera ( teta, snsperiny )
    endif
    
    ! Guarda sensibilidades y perdidas totales en variables globales
    do i = 1, Numnodos
       SnsPerNod ( i, intervalo ) = snsperiny(i)
       ! Escribe sensibilidades de perdidas de nodos por iteracion e intervalo
       write ( 876, 100, iostat = ierror ) ite+ 1, i, nomnod(i),  intervalo, snsperiny(i)
100    format ( i2, "," ,i5,  "," , a12, ",", i3,  "," , f19.16 ","   )
    enddo
    
    ! Calcula terminos constantes de la ecuación de balance por isla
    SnsPerIntIny(intervalo) = 0.0
    
    ! Incluye generacion de rango continuo
    do u = 1 , NumUniRC  ! JLC
!       para todos los nodos distribuidos
        do NoDi = 1, NoNodDisRC ( u, intervalo )
            nodo = Tempnodorc ( u, NoDi, intervalo )
            SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) + SnsPerNod ( nodo, intervalo )*facdistgen ( u, NoDi, intervalo )*GENUNRC ( u, intervalo )
        enddo
    enddo

    ! Incluye generacion de rango discontinuo
    do u = 1 , NumUniRD
        coeficiente = 0.0
!       para todos los modos de operacion
        do modo = 2, NumModRD(u)
!           para el modo de operacion asignado
            if ( xMILP(IARD + INIURDI ( u, intervalo ) + modo - 1) .gt. 0.8 .or. xMILP(IADARD + INIURDI ( u, intervalo ) + modo - 1) .gt. 0.8 ) then
                do componente = 1, NumCompXModo ( u, modo )
    !               para todas la componentes de la unida de rango discontinuo
                    do componente_1 = 0, NumCompRD ( u ) - 1
                        if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                            nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, intervalo )
                            coeficiente = coeficiente + SnsPerNod ( nodo, intervalo )*GenCompXModo  ( u, modo, componente )
                            exit
                        endif
                    enddo
                enddo
                exit
            endif
        enddo
        SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) + coeficiente*GENUNRD ( u, intervalo )
    enddo

    ! Incluye generacion hidro
    do u = 1 , NumUniHid
      nodo = nodoh ( u, intervalo )
      SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) + SnsPerNod ( nodo, intervalo )*GENUNH ( u, intervalo )
    enddo

    ! Incluye generacion renovables
    do u = 1 , NumUniRE
       nodo = nodounre ( u, intervalo )
       SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) + SnsPerNod ( nodo, intervalo )*GENUNRE ( u, intervalo )
    enddo
        
    ! Incluye nivel de demanda y corte de carga
    do d = 1 , NumOferDem  ! JLC
        do NoDi = 1, NoNodDisCar ( d, intervalo )
            nodo = Tempnodocar ( d, NoDi, intervalo )
            SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) - SnsPerNod ( nodo, intervalo )*demanda ( d, intervalo )*facdistcar ( d, NoDi, intervalo )
	        SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) + SnsPerNod ( nodo, intervalo )*xMILP ( IDF + d + (intervalo-1)*NumOferDem - 1 )*facdistcar ( d, NoDi, intervalo )
        enddo
    enddo
        
    ! Incluye excedentes
    do n = 1, NumNodSis ( sistema )
       nodo = inaumnodsis ( n )
       SnsPerIntIny(intervalo) = SnsPerIntIny(intervalo) - SnsPerNod ( nodo, intervalo )*xMILP ( IEXC + n + (intervalo-1)*NumNodSis ( sistema ) - 1 )
    enddo

    ! Libera el problema de programación lineal del ambiente CPLEX
    if (  intervalo .eq. NTINTR .and. .not. Banfactorizacionp ) then
       status = 0
       status = CPXfreeprob (enb, llp)
    endif
    
enddo

Call FechaEjecucion (fecha_Ej)
!write ( 888, * ) "Termina sensibilidad de perdidas" , ",", fecha_Ej
return

end
    
! **********************************************************************************
! *                                                                                *
! * Instituto de investigaciones electricas                                        *
! * Gerencia de analisis de redes                                                  *
! * Division de sistemas de control                                                *
! *                                                                                *
! * Objetivo:                                                                      *
! *                                                                                *
! * - Calcula sensibilidad de perdidas con respecto                                *
! *   todos los nodos                                                              *
! *                                                                                *
! *    Entradas:                                                                   *
! *       >  Matriz de flujos de DC factorizada (iu, ju, umat, dimat               *
! *       >  Solución de flujos (teta)                                             *
! *       >  Tpopologia de la red (numnod, incnod, inbusm, siguem )                *
! *       >  Identifica nodos activos.                                             *
! *                                                                                *
! *    Salidas:                                                                    *
! *       >  Sensibilidad de perdidas nodales ante inyecciones nodales (sepeno)    *
! *                                                                                *
! *                                                                 Marzo de 2002  *
! *                                                                                *
! **********************************************************************************

Subroutine SnsPer ( iu, ju, umat, dimat, teta, sepeno )

Use ParAUHE, only: NumNodos
Use ParGloRed, only: nmnod, inanod, incnod, inbusm, disram, siguem, &
                     maxnod, maxres, maxele, conductram

Implicit none

integer i, k, nren

integer ind, lk, lk2, m, rama, rama2

integer iu, ju

real*8 sepeno

real*8 vlder, vsol

real*8 umat, dimat

real*8 teta

dimension  sepeno(maxnod), vlder(maxres), vsol(maxres)

dimension teta (maxnod)

! Matriz factorizada
dimension iu ( maxres )
dimension ju ( maxele )
dimension umat ( maxele )
dimension dimat ( maxres )

nren = 0
vlder = 0.0
do i = 1, nmnod

       k = inanod(i)
       
       nren = nren + 1

!      * Genera los elementos debidos a ramas conectadas a nodo k *
       ind = 0

       lk = incnod(k)
       do while ( lk .ne. 0 )
         m = inbusm(lk)

!        Acumula la derivadas de rama
!        al angulo de voltaje
         rama = (lk+1)/2
	     if ( disram(rama) .ge. 1 ) then
           vlder(nren) = vlder(nren) + 2.0*conductram(rama) * ( teta(k) - teta(m) )
         endif
!        ----------------------------------------------
!        * Incluye la ramas en paralelo a la rama k-m *
!        ----------------------------------------------
         lk2 = siguem(lk)
         do while ( lk2 .ne. 0 )
           if ( inbusm(lk2) .ne. m ) then
             lk2 = 0
           else 
!            derivadas de inyecciones de potencia activa con respecto
             rama2 = (lk2+1)/2
		     if ( disram(rama2) .ge. 1 ) then 
               vlder(nren) = vlder(nren) + 2.0*conductram(rama2) * ( teta(k) - teta(m) )           
             endif		   
             lk = lk2
             lk2 = siguem(lk)
           endif
         enddo

         lk = siguem(lk)
       enddo

 enddo 

! -------------------------------------------
! * resuelve sistema de ecuaciones lineales *
! -------------------------------------------
call solfac ( nren, iu, ju, umat, dimat, vlder, vsol )

! --------------------------------------------------
! * guarda informacion de sensibilidad de perdidas *
! --------------------------------------------------
sepeno = 0
do i = 1, nmnod
  k = inanod(i)
  sepeno(k) = vsol(i)
enddo


Return

End    

    
! **********************************************************************************
! *                                                                                *
! * Instituto de investigaciones electricas                                        *
! * Gerencia de analisis de redes                                                  *
! * Division de SISTEMAS ELÉCTRICOS                                                *
! *                                                                                *
! * Objetivo:                                                                      *
! *                                                                                *
! * - Calcula sensibilidad de perdidas con respecto                                *
! *   todos los nodos                                                              *
! *                                                                                *
! *    Entradas:                                                                   *
! *       >  Matriz de flujos de DC cplex                                          *
! *       >  Solución de flujos (teta)                                             *
! *       >  Tpopologia de la red (numnod, incnod, inbusm, siguem )                *
! *       >  Identifica nodos activos.                                             *
! *                                                                                *
! *    Salidas:                                                                    *
! *       >  Sensibilidad de perdidas nodales ante inyecciones nodales (sepeno)    *
! *                                                                                *
! *                                                                 Julio del 2014 *
! *                                                                                *
! **********************************************************************************

Subroutine SnsPera ( teta, sepeno )

Use ParAUHE, only: NumNodos
Use ParGloRed, only: nmnod, inanod, incnod, inbusm, disram, siguem, &
                     maxnod, maxres, maxele, conductram, llp, ladder, vecsol

Implicit none

integer i, k, nren, ierror

integer ind, lk, lk2, m, rama, rama2

real*8 sepeno

real*8 teta

real*8 objf

dimension  sepeno(maxnod)

dimension teta (maxnod)

nren = 0
ladder = 0.0
do i = 1, nmnod

       k = inanod(i)
       
       nren = nren + 1

!      * Genera los elementos debidos a ramas conectadas a nodo k *
       ind = 0

       lk = incnod(k)
       do while ( lk .ne. 0 )
         m = inbusm(lk)

!        Acumula la derivadas de rama
!        al angulo de voltaje
         rama = (lk+1)/2
	     if ( disram(rama) .ge. 1 ) then
           ladder(nren) = ladder(nren) + 2.0*conductram(rama) * ( teta(k) - teta(m) )
         endif
!        ----------------------------------------------
!        * Incluye la ramas en paralelo a la rama k-m *
!        ----------------------------------------------
         lk2 = siguem(lk)
         do while ( lk2 .ne. 0 )
           if ( inbusm(lk2) .ne. m ) then
             lk2 = 0
           else 
!            derivadas de inyecciones de potencia activa con respecto
             rama2 = (lk2+1)/2
		     if ( disram(rama2) .ge. 1 ) then 
               ladder(nren) = ladder(nren) - 2.0*conductram(rama2) * ( teta(k) - teta(m) )           
             endif		   
             lk = lk2
             lk2 = siguem(lk)
           endif
         enddo

         lk = siguem(lk)
       enddo

 enddo 

! -------------------------------------------
! * resuelve sistema de ecuaciones lineales *
! -------------------------------------------
! Resulve el problema de flujos con el problema lineal
ierror = 0
call EjecutaFluPACb ( objf, ierror )

! --------------------------------------------------
! * guarda informacion de sensibilidad de perdidas *
! --------------------------------------------------
sepeno = 0
do i = 1, nmnod
  k = inanod(i)
  sepeno(k) = vecsol(i)
enddo

Return

End    