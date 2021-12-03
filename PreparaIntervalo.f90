! ---------------------------------------------------------------------
! Subrutina que prepara informacion de red eléctrica por intervalo    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! ---------------------------------------------------------------------

Subroutine PreparaRedIntervalo ( intervalo, icambia )

Use ParAUHE, only: NumNodos, SlackIsla, EstadoIsla, maxnod

Use ParGloRed, only: nmnod, inanod, inrnod, disnodini, sisnod, disnod, apnois, linois, &
                     numram, disram, disramini, oriram, desram, numisl, islnod, &
                     lisnoddisint, apunoddisint, estnoddisint, &
                     lisramdisint, apuramdisint, estramdisint, nomnod, maxisl

Implicit none

integer i, j, k, icuenta, intervalo, nodo, rama, icambia, slackisl(maxisl), tiposlack(maxnod), imax

data imax / 3 / ! umbral que indica el numero de nodos por isla, para eliminar informacion de islas muertas

icambia = 0

! Verifica que nodos cambian de disponibilidad durante el intervalo
if ( intervalo .eq. 1 ) then
    do i = 1, Numnodos
        disnod(i) = disnodini(i)
    enddo
else
    do i = apunoddisint(intervalo), apunoddisint(intervalo+1) -1
        nodo = lisnoddisint(i)
        if ( EstadoIsla(sisnod(nodo)) .eq. 1  ) then
           disnod(nodo) = estnoddisint(i)
           icambia = icambia + 1
        endif
    enddo
endif

! Prepara información de nodos activos en el intervalo
do i = 1, NumNodos
    if ( EstadoIsla(sisnod(i)) .eq. 0 ) then
       disnod(i) = 0
    endif
enddo


if ( intervalo .eq. 1 ) then
    ! Guarda disponibilidad de ramas en intervalo de trabajo
    do i = 1, numram
       disram(i) = disramini(i)
    enddo
else
    ! Verifica que ramas cambian de disponibilidad durante el intervalo
    do i = apuramdisint(intervalo), apuramdisint(intervalo+1) -1
        rama = lisramdisint(i)
        if ( EstadoIsla( sisnod( oriram(rama) ) ) .eq. 1) then
            disram(rama) = estramdisint(i)
            icambia = icambia + 1
        endif
    enddo
endif

if ( intervalo .eq. 1 .or. icambia .ne. 0 ) then

    ! prepara configurador de red eléctrica
    call ConfiguraRedElectrica

    ! Forma lista de nodos por isla
    apnois(1) = 1; icuenta = 0
    do i = 1, numisl
        do j = 1, Numnodos
            if ( islnod(j) .eq. i ) then
                icuenta = icuenta + 1
                linois(icuenta) = j
            endif
        enddo
        apnois(i+1) = icuenta + 1
    enddo

    ! Identifica nodos slack de la isla
    slackisl = 0; tiposlack = 0
    do i = 1, numisl
        if ( apnois(i+1) - apnois(i) .gt. imax ) then
           j = apnois(i)
           if ( EstadoIsla(sisnod(linois(j))) .eq. 1 ) then
              do j = apnois(i), apnois(i+1) -1
                  k = linois(j)
                  if ( disnod(k) .eq. 1 .and. SlackIsla(sisnod(k)) .eq. k ) then
                      slackisl(i) = k; tiposlack(k) = 1; exit
                  endif
              enddo
              ! Si no encontro el nodo slack define como slack el primer nodo disponible 
              ! de la isla
              if ( slackisl(i) .eq. 0 ) then
                 do j = apnois(i), apnois(i+1) -1
                     k = linois(j)
                     if ( disnod(k) .eq. 1  ) Then
                        slackisl(i) = k; tiposlack(k) = 1; exit
                     endif
                 enddo
              endif
           endif
        else
           do j = apnois(i), apnois(i+1) -1
               k = linois(j)
               tiposlack(k) = 1
           enddo
        endif
    enddo

    ! Forma lista de nodos activos para todas las islas eléctricas
    icuenta = 0; inanod = 0; inrnod = 0
    do k = 1, numnodos
        if ( tiposlack(k) .eq. 0 .and. disnod(k) .eq. 1 ) then
           icuenta = icuenta + 1
           inanod(icuenta) = k
           inrnod(k) = icuenta
         endif            
    enddo
    
    nmnod = icuenta
    
endif

return

end Subroutine PreparaRedIntervalo

    
! ---------------------------------------------------------------------
! Subrutina que prepara informacion de red eléctrica por intervalo    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! ---------------------------------------------------------------------

Subroutine PreparaRedIntervaloOLd ( intervalo, icambia )

Use ParAUHE, only: NumNodos, SlackIsla, EstadoIsla

Use ParGloRed, only: nmnod, inanod, inrnod, disnodini, sisnod, disnod, apnois, linois, &
                     numram, disram, disramini, oriram, desram, numisl, islnod, &
                     lisnoddisint, apunoddisint, estnoddisint, &
                     lisramdisint, apuramdisint, estramdisint, nomnod

Implicit none

integer i, j, k, icuenta, intervalo, nodo, rama, icambia, sihay, islack

icambia = 0

! Verifica que nodos cambian de disponibilidad durante el intervalo
if ( intervalo .eq. 1 ) then
    do i = 1, Numnodos
        disnod(i) = disnodini(i)
    enddo
else
    do i = apunoddisint(intervalo), apunoddisint(intervalo+1) -1
        nodo = lisnoddisint(i)
        if ( EstadoIsla(sisnod(nodo)) .eq. 1  ) then
           disnod(nodo) = estnoddisint(i)
           icambia = icambia + 1
        endif
    enddo
endif

! Prepara información de nodos activos en el intervalo
do i = 1, NumNodos
    if ( EstadoIsla(sisnod(i)) .eq. 0 ) then
       disnod(i) = 0
    endif
enddo


if ( intervalo .eq. 1 ) then
    ! Guarda disponibilidad de ramas en intervalo de trabajo
    do i = 1, numram
       disram(i) = disramini(i)
    enddo
else
    ! Verifica que ramas cambian de disponibilidad durante el intervalo
    do i = apuramdisint(intervalo), apuramdisint(intervalo+1) -1
        rama = lisramdisint(i)
        if ( EstadoIsla( sisnod( oriram(rama) ) ) .eq. 1) then
            disram(rama) = estramdisint(i)
            icambia = icambia + 1
        endif
    enddo
endif

if ( intervalo .eq. 1 .or. icambia .ne. 0 ) then

    ! prepara configurador de red eléctrica
    call ConfiguraRedElectrica

    ! Forma lista de nodos por isla
    apnois(1) = 1; icuenta = 0
    do i = 1, numisl
        do j = 1, Numnodos
            if ( islnod(j) .eq. i ) then
                icuenta = icuenta + 1
                linois(icuenta) = j
            endif
        enddo
        apnois(i+1) = icuenta + 1
    enddo

    ! Identifica la isla a evaluar
    icuenta = 0; inanod = 0; inrnod = 0; sihay = 0
    do i = 1, numisl
        if ( apnois(i+1) - apnois(i) .gt. 5 ) then
           j = apnois(i)
           if ( EstadoIsla(sisnod(linois(j))) .eq. 1 ) then
              do j = apnois(i), apnois(i+1) -1
                  k = linois(j)
                  if ( disnod(k) .eq. 1 .and. SlackIsla(sisnod(k)) .eq. k ) then
                      sihay = 1
                  endif
                  if ( disnod(k) .eq. 1 .and. SlackIsla(sisnod(k)) .ne. k ) then
                    icuenta = icuenta + 1
                    inanod(icuenta) = k
                    inrnod(k) = icuenta
                  endif
              enddo
             exit ! Hey solo procesa primer isla activa por límitaciones del problema de despacho
           endif
        endif
    enddo
    nmnod = icuenta

    ! Verifica si no hay nodo slack en la isla
    if ( sihay .eq. 0 ) then
        icuenta = 0; inanod = 0; inrnod = 0; islack = 0
        do i = 1, numisl
            if ( apnois(i+1) - apnois(i) .gt. 5 ) then
               j = apnois(i)
               if ( EstadoIsla(sisnod(linois(j))) .eq. 1 ) then
                  do j = apnois(i), apnois(i+1) -1
                      k = linois(j)
                      if ( disnod(k) .eq. 1 .and. islack .eq. 0 ) Then
                         islack = k
                      endif
                      if ( disnod(k) .eq. 1 .and. islack .ne. k ) then
                         icuenta = icuenta + 1
                         inanod(icuenta) = k
                         inrnod(k) = icuenta
                      endif
                  enddo
               endif
            endif
        enddo
        nmnod = icuenta
    endif
    
endif



return

end Subroutine PreparaRedIntervaloOld 


! ---------------------------------------------------------------------
! Subrutina que prepara informacion de generacíón por intervalo       *
!                                                                     *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! ---------------------------------------------------------------------

Subroutine PreparaGenIntervalo ( sistema, intervalo )

Use ParAUHE, only: NumUniRC, NumUniRD, NumUNiHid, NumUniRE, NumUniNPR, NumNodInt, NumOferDem,  &
                  GENUNRC, GENUNRD, GENUNH, GENUNRE, PotNPR, PotNodInt, DemFija, &
                  nodorc, nodocompurd, nodocar, nodoh, nodounre, nodonpr, NodoInt, NumBloDem, &
                  CompXModo, ListCompURD, ApunCompURD, GenCompXModo, NumCompRD, NumModRD, &
                  NumCompXModo, Tempnodorc, facdistgen, Tempnodocar, facdistcar, NoNodDisRC, &
                  NoNodDisCar, NoNodDisNPR, Tempnodonpr, facdistnpr
                  
use ProblemaAUHE, only: IDBC, INDDE, IDF, IEXC, xMILP, IADARD, INIURDI, IARD

Use ParGloRed, only: inaumnodsis, numnodsis, pgenod, pcanod, inrnod

Implicit none

integer u, d, k, s, n, intervalo, nodo, sistema
integer componente, componente_1, modo, NoDi

! Inicializa vector de trabajo
pgenod = 0.0
pcanod = 0.0

! Incluye generacion de rango continuo
do u = 1 , NumUniRC  ! JLC
!   para todos los nodos distribuidos
    do NoDi = 1, NoNodDisRC ( u, intervalo )
        nodo = Tempnodorc ( u, NoDi, intervalo )
        pgenod(nodo) = pgenod(nodo) + facdistgen ( u, NoDi, intervalo )*GENUNRC ( u, intervalo )
    enddo
enddo

! Incluye generacion de rango discontinuo
do u = 1 , NumUniRD
!   para todos los modos de operacion
    do modo = 2, NumModRD(u)
!       para el modo de operacion asignado
        if ( xMILP(IARD + INIURDI ( u, intervalo ) + modo - 1) .gt. 0.8 .or. xMILP(IADARD + INIURDI ( u, intervalo ) + modo - 1) .gt. 0.8 ) then
            do componente = 1, NumCompXModo ( u, modo )
    !           para todas la componentes de la unidad de rango discontinuo
                do componente_1 = 0, NumCompRD ( u ) - 1
                    if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                        nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, intervalo )
                        pgenod(nodo) = pgenod(nodo) + GENUNRD ( u, intervalo )*GenCompXModo  ( u, modo, componente )
                        exit
                    endif
                enddo
            enddo
            exit
        endif
    enddo
enddo

! Incluye generacion hidro
do u = 1 , NumUniHid
  nodo = nodoh ( u, intervalo )
  pgenod(nodo) = pgenod(nodo) +GENUNH ( u, intervalo )
enddo

! Incluye generacion renovables
do u = 1 , NumUniRE
   nodo = nodounre ( u, intervalo )
   pgenod(nodo) = pgenod(nodo) +  GENUNRE ( u, intervalo )
enddo
        
! Incluye nivel de demanda y corte de carga
do d = 1 , NumOferDem   ! JLC
!   para todos los nodos distribuidos
    do NoDi = 1, NoNodDisCar ( d, intervalo )
        nodo = Tempnodocar ( d, NoDi, intervalo )
!       para todos los segmentos de curva de ofertas de compra
        do s = 1, NumBloDem( d, intervalo )
	       pcanod(nodo) = pcanod(nodo) + xMILP ( IDBC + s + INDDE ( d, intervalo ) - 1 )*facdistcar ( d, NoDi, intervalo )
        enddo
!       corte de carga
	    pgenod(nodo) = pgenod(nodo) + xMILP ( IDF + d + (intervalo-1)*NumOferDem - 1 )*facdistcar ( d, NoDi, intervalo )
    enddo
enddo
        
! Incluye excedentes
do n = 1, NumNodSis ( sistema )
   nodo = inaumnodsis ( n )
   pgenod(nodo) = pgenod(nodo) - xMILP ( IEXC + n + (intervalo-1)*NumNodSis ( sistema ) - 1 )
enddo

!   Agrega termino asociado a generaciones no programables
do k = 1, NumUniNPR ! JLC
    do NoDi = 1, NoNodDisNPR ( k, intervalo )
        nodo = Tempnodonpr ( k, NoDi, intervalo )
        if ( inrnod(nodo) .ne. 0 ) then
	        pgenod(nodo) = pgenod(nodo) + PotNPR ( k, intervalo )*facdistnpr ( k, NoDi, intervalo )
        endif
    enddo
enddo

! Agrega termino asociado a ofertas de demanda ( componente fija )
do k = 1, NumOferDem  ! JLC
!   para todos los nodos distribuidos
    do NoDi = 1, NoNodDisCar ( k, intervalo )
        nodo = Tempnodocar ( k, NoDi, intervalo )
        if ( inrnod(nodo) .ne. 0 ) then
            pcanod(nodo) = pcanod(nodo) +  DemFija ( k, intervalo )*facdistcar ( k, NoDi, intervalo )
        endif
    enddo
enddo

!   Agrega termino asociado a potencias de intercambio 
do k = 1, NumNodInt
   nodo = NodoInt(k, intervalo)
   if ( inrnod(nodo) .ne. 0 ) then
       pcanod(nodo) = pcanod(nodo) + PotNodInt ( k, intervalo )
   endif
enddo

return

end Subroutine PreparaGenIntervalo

! ------------------------------------------------------------------------
! Subrutina que prepara informacion de cortes y excedentes por intervalo *
! y generacion nodal                                                     *
!                                                                        *
! Instituto de investigaciones Electricas                                *
! Gerencia de analisis de redes                                          *
! Division de sistemas electricos                                        *
!                                                                        *
!  Mayo 2019                                                             *
!                                                                        *
! ------------------------------------------------------------------------

Subroutine PreparaCorteExcedenteIntervalo ( sistema, intervalo, cornod, excenod, potgennod, demnod )

Use ParAUHE, only: maxnod, NumOferDem,  Tempnodocar, facdistcar, NoNodDisCar, &
                   NumUniRC, NoNodDisRC, Tempnodorc, facdistgen, GENUNRC, &
                   NumUniRD, NumModRD, NumCompXModo, NumCompRD, CompXModo, ListCompURD, ApunCompURD, nodocompurd, GENUNRD, GenCompXModo, &
                   NumUniHid,  GENUNH, nodoh, NumUniRE, GENUNRE, nodounre, NumUniNPR,  NoNodDisNPR, Tempnodonpr, PotNPR, facdistnpr, &
                   NumBloDem,  DemFija
                  
use ProblemaAUHE, only: IDF, IEXC, xMILP, IARD, INIURDI,  IADARD, IDBC, INDDE

Use ParGloRed, only: inaumnodsis, numnodsis, inrnod

Implicit none

integer  d, n, u, k, s, intervalo, nodo, sistema, NoDi, modo, componente, componente_1

real*8 cornod ( maxnod ), excenod ( maxnod ), potgennod ( maxnod ), demnod ( maxnod )

! Inicializa vector de trabajo
cornod = 0.0
excenod = 0.0
potgennod = 0.0
demnod = 0.0

! Incluye nivel de demanda y corte de carga
do d = 1 , NumOferDem   
!   para todos los nodos distribuidos
    do NoDi = 1, NoNodDisCar ( d, intervalo )
        nodo = Tempnodocar ( d, NoDi, intervalo )
!       para todos los segmentos de curva de ofertas de compra
        do s = 1, NumBloDem( d, intervalo )
	       demnod(nodo) = demnod(nodo) + xMILP ( IDBC + s + INDDE ( d, intervalo ) - 1 )*facdistcar ( d, NoDi, intervalo )
        enddo
!       corte de carga
	    cornod(nodo) = cornod(nodo) + xMILP ( IDF + d + (intervalo-1)*NumOferDem - 1 )*facdistcar ( d, NoDi, intervalo )
    enddo
enddo

! Agrega termino asociado a ofertas de demanda ( componente fija )
do k = 1, NumOferDem  ! JLC
!   para todos los nodos distribuidos
    do NoDi = 1, NoNodDisCar ( k, intervalo )
        nodo = Tempnodocar ( k, NoDi, intervalo )
        if ( inrnod(nodo) .ne. 0 ) then
            demnod(nodo) = demnod(nodo) +  DemFija ( k, intervalo )*facdistcar ( k, NoDi, intervalo )
        endif
    enddo
enddo


! Incluye excedentes
do n = 1, NumNodSis ( sistema )
   nodo = inaumnodsis ( n )
   excenod(nodo) = excenod(nodo) + xMILP ( IEXC + n + (intervalo-1)*NumNodSis ( sistema ) - 1 )
enddo

! Incluye generacion de rango continuo
do u = 1 , NumUniRC  ! JLC
!   para todos los nodos distribuidos
    do NoDi = 1, NoNodDisRC ( u, intervalo )
        nodo = Tempnodorc ( u, NoDi, intervalo )
        potgennod(nodo) = potgennod(nodo) + facdistgen ( u, NoDi, intervalo )*GENUNRC ( u, intervalo )
    enddo
enddo

! Incluye generacion de rango discontinuo
do u = 1 , NumUniRD
!   para todos los modos de operacion
    do modo = 2, NumModRD(u)
!       para el modo de operacion asignado
        if ( xMILP(IARD + INIURDI ( u, intervalo ) + modo - 1) .gt. 0.8 .or. xMILP(IADARD + INIURDI ( u, intervalo ) + modo - 1) .gt. 0.8 ) then
            do componente = 1, NumCompXModo ( u, modo )
    !           para todas la componentes de la unidad de rango discontinuo
                do componente_1 = 0, NumCompRD ( u ) - 1
                    if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                        nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, intervalo )
                        potgennod(nodo) = potgennod(nodo) + GENUNRD ( u, intervalo )*GenCompXModo  ( u, modo, componente )
                        exit
                    endif
                enddo
            enddo
            exit
        endif
    enddo
enddo

! Incluye generacion hidro
do u = 1 , NumUniHid
  nodo = nodoh ( u, intervalo )
  potgennod(nodo) = potgennod(nodo) + GENUNH ( u, intervalo )
enddo

! Incluye generacion renovables
do u = 1 , NumUniRE
   nodo = nodounre ( u, intervalo )
   potgennod(nodo) = potgennod(nodo) +  GENUNRE ( u, intervalo )
enddo

!   Agrega termino asociado a generaciones no programables
do k = 1, NumUniNPR 
    do NoDi = 1, NoNodDisNPR ( k, intervalo )
        nodo = Tempnodonpr ( k, NoDi, intervalo )
        if ( inrnod(nodo) .ne. 0 ) then
	        potgennod(nodo) = potgennod(nodo)  + PotNPR ( k, intervalo )*facdistnpr ( k, NoDi, intervalo )
        endif
    enddo
enddo

return

end Subroutine PreparaCorteExcedenteIntervalo

