
! ---------------------------------------------------------------------
! Imprime resultados de asignaciones, despachos y variables duales    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2020                                                       *
! ---------------------------------------------------------------------
Subroutine ResulMILP ( ite, sistema, imprime, y )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, PerIntervalo, nomnod, NumNodSis, &
                     SnsPerIntIny, regnod, nomregpre

Implicit none

INTEGER d, dia, i, j, l, s, u, inicio, m, n, ite, k, kr10, k10, ks, kr, r

INTEGER ibanbit, ierror, kv, ka, kd, inicioa, modo, sis, sistema, &
        UniCSTOACSV, uunidad, UniRESCARCSV, UniBalance, intervalo

integer imprime

Real*8  demanda, generacion, NoProgra, corte, CostoGenRC ( maxsis ), &
        CostoArrRC (maxsis), CostoGenRD ( maxsis ), CostoArrRD (maxsis), &
        TotInter, CostoTotGen, CostoTotArr, CostoTotal, demelas, demFij, &
        demC10m, demCSup, perdida, CostoGenH( maxsis ), CostoGenRI ( maxsis ), &
        ctgen ( maxurc, maxint ), ctarr ( maxurc, maxint ), DemEla ( maxint ), &
        cosres ( maxsis ), ingrres ( maxsis ), ingrdem ( maxsis ), y (maxresMILP), excede

Real*8  aux, cortnod ( maxnod, maxint )
Real*8  tempor

CHARACTER fecha_Ej*19
character*20 aaux4
character*1 ssistema
character*5 aaux1
character*15 aaux3
character*20 aaux2

ibanbit = 1
ierror = 0
cortnod = 0.0

! si el sistema contiene unidades de rango continuo
if ( NumUniRC .gt. 0 ) then
!   se escribe solucion de asignacion y despacho para unidades de rango continuo
    call AsigDesRC
endif

! si el sistema contiene unidades de rango discontinuo
if ( NumUniRD .gt. 0 ) then
!   se escribe solucion de asignacion y despacho para unidades de rango discontinuo
    call AsigDesRD ( sistema )
endif

! si el sistema contiene unidades hidro
if ( NumUniHid .gt. 0 ) then
!   se escribe solucion de asignacion y despacho para unidades hidro
    call AsigDesH
endif

! si el sistema contiene unidades renovables
if ( NumUniRE .gt. 0 ) then
!   se escribe solucion de asignacion y despacho para unidades hidro
    call AsigDesRE
endif

CostoTotGen = 0.0
CostoTotArr = 0.0
CostoTotal = 0.0
sis = 0

ctgen = 0.0
ctarr = 0.0

! Se abre archivo para escribir cortes y excedentes en la solucion
OPEN ( UNIT = 889, FILE = RUT_RES//'Cortes.res', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 30024 )

! Se abre archivo para escribir cortes en los nodos
OPEN (UNIT = 13, FILE = trim(RUT_DAT_1)//'CORTENODAL_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

! Se abre archivo para escribir cortes en los nodos
OPEN (UNIT = 14, FILE = trim(RUT_DAT_1)//'EXCEDNODAL_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

! para cada subsistema
do l = 1, NumSis
    kv = IGABRC - 1
    ka = IBOARC - 1
    CostoGenRC ( l ) = 0.0
    CostoArrRC ( l ) = 0.0
    CostoGenRD ( l ) = 0.0
    CostoArrRD ( l ) = 0.0
    CostoGenH ( l ) = 0.0
    CostoGenRI ( l ) = 0.0
    cosres ( l ) = 0.0
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
        sis = sis + 1
        Write( ssistema, '(I1)' )  l
!       imprime resultados de flujos en grupos de ramas restringidos
        call ImprimeFujos ( ite, l, imprime )
        !Abre archivos csv de resultados de costos de generacion y arranque por unidad por hora por subsitema
        uunidad = 118 + l
        UniCSTOACSV = uunidad
        OPEN ( UNIT = UniCSTOACSV, FILE = trim(rut_dat_1)//'RESCSTOPARR_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        !Abre archivos csv de resultados de balance del sistema
        uunidad = 128 + 5
        UniBalance = uunidad
        OPEN ( UNIT = UniBalance, FILE = trim(rut_dat_1)//'BALANCES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 350 )
        
        write ( Unirdes, * ) ' '
        write ( Unirdes, * ) 'Isla :', l, nomsis(l)
        write ( Unirdes, * ) ' '
        write ( Unirdes, * ) 'intervalo  demFija   demElas     demC10m    demCSup  potencia   no progra  intercamb         dualbal      corte    perdidaM   perdidaE     dualperd'
        write ( Unirdes, * ) '            (MW)       (MW)        (MW)       (MW)     (MW)       (MW)        (MW)            ($/MW)       (MW)      (MW)       (MW)        ($/MW)'
        write ( Unirdes, * ) ' '
!       para cada intervalo
        GAPperdidas = 0	! URIEL	
        do i = 1, NTINTR
            demFij = 0.0
            demelas = 0.0
            generacion = 0.0
            NoProgra = 0.0
            TotInter = 0.0
            demC10m = 0.0
            demCSup = 0.0
!           para todas las unidades de rango continuo
            do u = 1, NumUniRC
!               si el generador esta en la isla
                if ( IslaGenRC ( u ) .eq. l ) then
                    generacion = generacion + GENUNRC ( u, i )
                endif
            enddo
!           para todas las unidades de rango discontinuo
            do u = 1, NumUniRD
!               si el generador esta en la isla
                if ( IslaGenRD ( u ) .eq. l ) then
                    generacion = generacion + GENUNRD ( u, i )
                endif
            enddo
!           para todas las unidades hidro
            do u = 1, NumUniHid
!               si el generador esta en la isla
                if ( IslaGenH ( u ) .eq. l ) then
                    generacion = generacion + GENUNH ( u, i )
                endif
            enddo
!           para todas las unidades renovables
            do u = 1, NumUniRE
!               si el generador esta en la isla
                if ( IslaGenRE ( u ) .eq. l ) then
                    generacion = generacion + GENUNRE ( u, i )
                endif
            enddo
!           para todas las cargas
            do d = 1, NumOferDem
!               si el generador esta en la isla
                if ( IslaDem ( d ) .eq. l ) then
!                   para todos los segmentos de curva de ofertas de compra
                    do s = 1, NumBloDem( d, i )
!                       total demanda en el bloque
                        demelas = demelas + xMILP ( IDBC + s + INDDE ( d, i ) - 1 )
                    enddo
                    demFij = demFij + DemFija(d,i)
!                    demC10m = demC10m + xMILP ( ICC10 + d + (i-1)*NumOferDem - 1 )
!                    demCSup = demCSup + xMILP ( ICCS + d + (i-1)*NumOferDem - 1 )
                endif
            enddo
!           para todas las unidades no programables
            do u = 1, NumUniNPR
!               si el generador esta en la isla
                if ( IslaGenNPR ( u ) .eq. l ) then
                    NoProgra = NoProgra + PotNPR ( u, i )
                endif
            enddo
!           para todos los intercambios
            do u = 1 , NumNodInt
!               si la unidad pertenece a la isla
                if (IslaNodInt ( u ) .eq. l ) then
                    TotInter = TotInter + PotNodInt ( u, i )
                endif
            enddo
            perdida = xMILP ( IPERD + i + (sis-1)*numsis_act - 1 ) ! PerIntervalo(i)
!           demanda total
            demanda = demelas + demFij
!            corte = demanda - generacion - NoProgra + TotInter + perdida
!            perdida = PerIntervalo(i)
            corte = 0.0
            do d = 1, NumOferDem
                corte = corte + xMILP(IDF + d + (i-1)*NumOferDem - 1)
                cortnod ( Tempnodocar(d,1,i), i ) = xMILP(IDF + d + (i-1)*NumOferDem - 1)*Base
            enddo
            if ( abs(corte) .lt. 5.0E-5 ) then
                corte = 0.0
            endif

            excede = 0.0
            do n = 1, NumNodSis ( l )
                excede = excede + xMILP(IEXC + n + (i-1)*NumNodSis ( l ) - 1)
            enddo
            if ( abs(excede) .lt. 5.0E-5 ) then
                excede = 0.0
            endif

            if ( corte .gt. 0.0 .and. ( ite .ge. IterPerdidas .or. (SiPerdidas .eq. 0 .and. Sitransmision .eq. 0) &
                     .or. (SiPerdidas .eq. 0 .and. Siviolacion .eq. 0) ) .or. imprime .eq. 1 ) then
!                solucion con corte de energia
                 SemBandera ( 1 ) = 1
                 Call FechaEjecucion (fecha_Ej)
                 bmensaje = fecha_Ej//' '//NomEjecu//'100 CORTE DE ENERGIA'
                 Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                 write ( 889, 5104 ) BMensaje
                 write ( aaux1, 5100 ) i 
                 write ( aaux2, 5102 ) nomsis(l)
                 Call FechaEjecucion (fecha_Ej)
                 BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV SISTEMA: '//aaux1//aaux2
                 call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                 write ( 889, 5104 ) BMensaje
                 aux = corte*Base
                 write ( aaux3, 5101 )  aux
                 Call FechaEjecucion (fecha_Ej)
                 BMensaje = fecha_Ej//' '//NomEjecu//'100 CORTE (MW)  '//aaux3
                 call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                 write ( 889, 5104 ) BMensaje
!                se identifica el(los) participante(s) con corte de carga
                 do d = 1, NumOferDem*SiEscCorExc
                     if ( xMILP(IDF + d + (i-1)*NumOferDem - 1) .gt. 1.0e-4 ) then
                         write ( aaux1, 5100 ) d 
                         write ( aaux2, 5102 ) nombcar(d)
                         Call FechaEjecucion (fecha_Ej)
                         BMensaje = fecha_Ej//' '//NomEjecu//'100 CARGA PARTICIPANTE: '//aaux1//aaux2
!                         call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                         write ( 889, 5104 ) BMensaje
                         aux = xMILP(IDF + d + (i-1)*NumOferDem - 1)*Base
                         write ( aaux3, 5101 )  aux
                         Call FechaEjecucion (fecha_Ej)
                         BMensaje = fecha_Ej//' '//NomEjecu//'100 CORTE (MW)  '//aaux3
!                         call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                         write ( 889, 5104 ) BMensaje
                         BMensaje = fecha_Ej//' '//NomEjecu//'100 NODO: '//trim(nomnod(Tempnodocar(d,1,i)))//' AREA: '//trim(nodo_area(Tempnodocar(d,1,i)))//' REGION: '//trim(nomregpre(regnod(Tempnodocar(d,1,i))))
!                         call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                         write ( 889, 5104 ) BMensaje
                     endif
                 enddo
                 write ( 889, * )
            endif
            if ( excede .gt. 0.0 .and. ( ite .ge. IterPerdidas .or. (SiPerdidas .eq. 0 .and. Sitransmision .eq. 0) &
                        .or. (SiPerdidas .eq. 0 .and. Siviolacion .eq. 0) ) .or. imprime .eq. 1 ) then
!                solucion con excedente de energia
                 SemBandera ( 2 ) = 1
                 Call FechaEjecucion (fecha_Ej)
                 bmensaje = fecha_Ej//' '//NomEjecu//'100 EXCEDENTE DE ENERGIA'
                 Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                 write ( 889, 5104 ) BMensaje
                 write ( aaux1, 5100 ) i 
                 write ( aaux2, 5102 ) nomsis(l)
                 Call FechaEjecucion (fecha_Ej)
                 BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV SISTEMA: '//aaux1//aaux2
                 call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                 write ( 889, 5104 ) BMensaje
                 aux = excede*Base
                 write ( aaux3, 5101 )  aux
                 Call FechaEjecucion (fecha_Ej)
                 BMensaje = fecha_Ej//' '//NomEjecu//'100 EXCEDENTE (MW)  '//aaux3
                 call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                 write ( 889, 5104 ) BMensaje
!                se identifica el(los) nodo(s) con excedente de energia
                 do n = 1, NumNodSis ( l )*SiEscCorExc
                     if ( xMILP(IEXC + n + (i-1)*NumNodSis ( l ) - 1) .gt. 1.0e-4 ) then
                         write ( aaux1, 5100 ) n 
                         write ( aaux2, 5102 ) nomnod(n)
                         Call FechaEjecucion (fecha_Ej)
                         BMensaje = fecha_Ej//' '//NomEjecu//'100 NODO NOMBRE: '//aaux1//aaux2
!                         call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                         write ( 889, 5104 ) BMensaje
                         aux = xMILP(IEXC + n + (i-1)*NumNodSis ( l ) - 1)*Base
                         write ( aaux3, 5101 )  aux
                         Call FechaEjecucion (fecha_Ej)
                         BMensaje = fecha_Ej//' '//NomEjecu//'100 EXCEDENTE (MW)  '//aaux3
!                         call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                         write ( 889, 5104 ) BMensaje
                     endif
                 enddo
                 write ( 889, * )
            endif
            demC10m = 0.0
            demCsup = 0.0
            if ( DualPerdidas (l, i) .gt. ( (dualbalance (l, i) + CostoExced)*1.10 ) ) then
!                DualPerdidas (l, i) = dualbalance (l, i)
            endif
            write ( Unirdes, 200 ) i, demFij*Base, demelas*Base, demC10m*Base, demCSup*Base, generacion*Base, &
                                   NoProgra*Base, TotInter*Base, dualbalance(l,i)/Base, (corte-excede)*Base, &
                                   perdida*Base, (PerIntervalo(i))*Base, dualperdidas(l,i)/Base
								   
								   ! URIEL MODIFICACIÓN PARA CAPTURAR VALORES DE PERDIDASM Y PERDIDASE
								   !PRINT *,(perdida*Base),(PerIntervalo(i))*Base
								   tempor = ABS(( perdida*Base - (PerIntervalo(i))*Base ) / ((PerIntervalo(i))*Base))
								   !print *,"tempor",tempor,GAPperdidas								   
								   if (tempor.gt.GAPperdidas) then
										GAPperdidas=tempor
										!print *,"GAPperdidas",GAPperdidas
								   endif 
								   ! URIEL MODIFICACIÓN PARA CAPTURAR VALORES DE PERDIDASM Y PERDIDASE
								   
								   
            write ( UniBalance, 300 ) generacion*Base, NoProgra*Base, TotInter*Base, (corte-excede)*Base, &
                                      perdida*Base, (demFij+demelas)*Base, dualbalance(l,i)/Base
        enddo

!       se escriben cortes y excedentes por nodo
        do n = 1, NumNodSis ( l )*SiEscResRed
            write ( 13, 100, IOSTAT = IERROR ) n, nomnod(n), ( cortnod ( n, i ),  i = 1, NTINTR )
            write ( 14, 100, IOSTAT = IERROR ) n, nomnod(n), ( xMILP(IEXC + n + (i-1)*NumNodSis ( l ) - 1)*Base,  i = 1, NTINTR )
        enddo

!       calculo de costos
!       si son ofertas de costo a generacion minima
        if ( TipoOferta .eq. 1 ) then
    !       para todas las unidades de rango continuo
            do u = 1, NumUniRC
                inicio = 0
                do j = 1, NTINTR
                    do s = 1, NumBloVRC( u, j )
                        inicio = inicio + 1
                    enddo
                enddo
                inicioa = NmBloArrURC ( u )*NTINTR
    !           si el generador esta en la isla
                if ( IslaGenRC ( u ) .eq. l ) then
    !               para cada intervalo
                    do i = 1, NTINTR
    !                   si la unidad esta asignada
                        if ( xMILP ( IARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
    !                       costo minimo
                            CostoGenRC ( l ) = CostoGenRC ( l ) + CostoMinGRC(u,i)
                            ctgen ( u, i ) = CostoMinGRC(u,i)
    !                       para todos los segmentos de curva de ofertas de venta
                            do s = 1, NumBloVRC( u, i )
    !                           precio de segmento de venta
                                kv = kv + 1
                                CostoGenRC ( l ) = CostoGenRC ( l ) + PreVenEnerRC(u,s,i) * xMILP ( kv )
                                !Costo de generacion por unidad
                                ctgen ( u, i ) = ctgen ( u, i ) + PreVenEnerRC(u,s,i) * xMILP ( kv )
                            enddo
                        else
                            kv = kv + NumBloVRC( u, i )
                        endif
    !                   si la unidad arranco
                        if ( xMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
                            if ( NmBloArrURC ( u ) .gt. 0 ) then 
    !                           para todos los segmentos de la curva de arranque
                                do s = 1, NmBloArrURC( u )
    !                               coeficiente de segmento de arranque
                                    ka = ka + 1
                                    CostoArrRC ( l ) = CostoArrRC ( l ) + xMILP ( ka ) * CostoArrRCS ( u, s )
                                    !Costo de arranque por unidad
                                    ctarr ( u, i ) = ctarr ( u, i ) + xMILP ( ka ) * CostoArrRCS ( u, s )
                                enddo
                            else
    !                           coeficiente de costo unico de arranque
                                CostoArrRC ( l ) = CostoArrRC ( l ) + xMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) * CostArrUniURC ( u )
                                !Costo de arranque por unidad
                                ctarr ( u, i ) = xMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) * CostArrUniURC ( u )
                            endif
                        else
                            ka = ka + NmBloArrURC ( u )
                        endif
    !                   si la unidad esta en sincronizacion
                        if ( xMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
    !                       costo en sincronizacion
                            CostoGenRC ( l ) = CostoGenRC ( l ) + xMILP ( IGDARC + u + (i-1)*NumUniRC - 1 )*(CostoMinGRC(u,i)/PotMinGRC ( u, i ))
    !                       Costo de sincronizacion por unidad
                            ctgen ( u, i ) = ctgen ( u, i ) + xMILP ( IGDARC + u + (i-1)*NumUniRC - 1 )*(CostoMinGRC(u,i)/PotMinGRC ( u, i ))
                        endif
                    enddo
                    !Imprime resultados de costos de generacion y arranque por unidad y por hora por subsitema
                    write ( UniCSTOACSV, 700 ) nombunirc(u), 0, ( ctgen ( u, i ), ctarr ( u, i ), i=1,NTINTR )
                else
                    kv = kv + inicio
                    ka = ka + inicioa
                endif
    !           Para todos los intervalos
                do i = 1 , NTINTR*SiOferRes
    !               costo de reserva rodante de 10 minutos
                    cosres ( l ) = cosres ( l ) + PreVenResR10RC(u,i)*xMILP(IRR10RC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva no rodante de 10 minutos
                    cosres ( l ) = cosres ( l ) + PreVenResNR10RC(u,i)*xMILP(IRNR10RC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva rodante suplementaria
                    cosres ( l ) = cosres ( l ) + PreVenResRxRC(u,i)*xMILP(IRRSRC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva no rodante suplementaria
                    cosres ( l ) = cosres ( l ) + PreVenResNRxRC(u,i)*xMILP(IRNRSRC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva de regulacion secundaria
                    cosres ( l ) = cosres ( l ) + PreVenResRegRC(u,i)*xMILP(IRRERC + u + (i-1)*NumUniRC - 1)
                enddo
            enddo        
        
            kv = IGABRD - 1
            ka = IBOARD - 1
    !       
            ctgen = 0.0
            ctarr = 0.0
            k = 1
    !       para todas las unidades de rango discontinuo
            do u = 1, NumUniRD
    !           si el generador esta en la isla
                if ( IslaGenRD ( u ) .eq. l ) then
    !               para cada intervalo
                    do i = 1, NTINTR
    !                   para todos los modos
                        do modo = 1, NumModRD ( u )
    !                       si la unidad esta asignada
                            if ( xMILP ( IARD + INIURDI ( u, i ) + modo - 1  ) .gt. 0.9 ) then
    !                           costo minimo
                                CostoGenRD ( l ) = CostoGenRD ( l ) + CostoMinGRD ( u, modo, i )
                                ctgen ( u, i ) = CostoMinGRD ( u, modo, i )
    !                           para todos los segmentos de curva de ofertas de venta
                                do s = 1, NumBloVRD( u, modo, i )
    !                               precio de segmento de venta
                                    CostoGenRD ( l ) = CostoGenRD ( l ) + PreVenEnerRD ( u, modo, s, i ) * &
                                                                          xMILP ( kv + INBURD (u, modo, i) + s )
                                    ctgen ( u, i ) = ctgen ( u, i ) + PreVenEnerRD ( u, modo, s, i ) * xMILP ( kv + INBURD (u, modo, i) + s )
                                enddo
                            endif
    !                       si la unidad arranco
                            if ( xMILP ( IARRD + INIURDI ( u, i ) + modo - 1  ) .gt. 0.9 ) then
                                if ( NmBloArrURD ( u, modo ) .gt. 0 ) then 
    !                               para todos los segmentos de la curva de arranque
                                    do s = 1, NmBloArrURD ( u, modo )
    !                                   coeficiente de segmento de arranque
                                        CostoArrRD ( l ) = CostoArrRD ( l ) + xMILP ( ka + INBAURD (u, modo, i) + s ) * &
                                                                              CostoArrRDS ( u, modo, s )
                                        ctarr ( u, i ) = ctarr ( u, i ) + xMILP ( ka + INBAURD (u, modo, i) + s ) * CostoArrRDS ( u, modo, s )
                                    enddo
                                else
    !                               coeficiente de costo unico de arranque
                                    CostoArrRD ( l ) = CostoArrRD ( l ) + xMILP ( IARRD + INIURDI ( u, i ) + modo - 1  ) * &
                                                                          CostoTrans ( u, 1, modo )
                                    ctarr ( u, i ) = xMILP ( IARRD + INIURDI ( u, i ) + modo - 1  ) * CostoTrans ( u, 1, modo )
                                endif
                            endif
    !                       si la unidad esta en sincronizacion
                            if ( xMILP ( IGDARD + INIURDI ( u, i ) + modo - 1 ) .gt. 0.0 ) then
    !                           costo en sincronizacion
                                CostoGenRD ( l ) = CostoGenRD ( l ) + xMILP (IGDARD + INIURDI ( u, i ) + modo - 1) * &
                                                                    ( CostoMinGRD ( u, modo, i )/PotMinGRD ( u, modo, i ) )
    !                           Costo de sincronizacion por unidad
                                ctgen ( u, i ) = ctgen ( u, i ) + xMILP (IGDARD + INIURDI ( u, i ) + modo - 1) * &
                                                                    ( CostoMinGRD ( u, modo, i )/PotMinGRD ( u, modo, i ) )
                            endif
                        enddo
                    enddo
                    !Imprime resultados de costos de generacion y arranque por unidad y por hora por subsitema
                    write ( UniCSTOACSV, 700 ) nombunird(u), 1, ( ctgen ( u, i ), ctarr ( u, i ), i=1,NTINTR )
                endif
    !           Para todos los intervalos
                do i = 1 , NTINTR*SiOferRes
    !               para todos los modos de la unidad
                    do m = 1, NumModRD(u)
    !                   costo de reserva rodante de 10 minutos
                        cosres ( l ) = cosres ( l ) + PreVenResR10RD(u,m,i)*xMILP(IRR10RD + k - 1)
    !                   costo de reserva no rodante de 10 minutos
                        cosres ( l ) = cosres ( l ) + PreVenResNR10RD(u,m,i)*xMILP(IRNR10RD + k - 1)
    !                   costo de reserva rodante suplementaria
                        cosres ( l ) = cosres ( l ) + PreVenResRxRD(u,m,i)*xMILP(IRRSRD + k - 1)
    !                   costo de reserva no rodante suplementaria
                        cosres ( l ) = cosres ( l ) + PreVenResNRxRD(u,m,i)*xMILP(IRNRSRD + k - 1)
    !                   costo de reserva de regulacion secundaria
                        cosres ( l ) = cosres ( l ) + PreVenResRegRD(u,m,i)*xMILP(IRRERD + k - 1)
                        k = k + 1
                    enddo
                enddo
            enddo
!       si son ofertas de costo en vacio
        else
    !       para todas las unidades de rango continuo
            do u = 1, NumUniRC
                inicio = 0
                do j = 1, NTINTR
                    do s = 1, NumBloVRC( u, j )
                        inicio = inicio + 1
                    enddo
                enddo
                inicioa = NmBloArrURC ( u )*NTINTR
    !           si el generador esta en la isla
                if ( IslaGenRC ( u ) .eq. l ) then
    !               para cada intervalo
                    do i = 1, NTINTR
    !                   si la unidad esta asignada o en sincronizacion
                        if ( xMILP ( IARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 .or. xMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
    !                       si la unidad esta asignada
                            if ( xMILP ( IARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
    !                           costo en vacio mas costo de primer segmento
                                CostoGenRC ( l ) = CostoGenRC ( l ) + CostoMinGRC(u,i) + PotMinGRC ( u, i )*PreVenEnerRC(u,1,i)
                                ctgen ( u, i ) = CostoMinGRC(u,i) + PotMinGRC ( u, i )*PreVenEnerRC(u,1,i)
                                if ( NumBloVRC( u, i ) .gt. 0 ) then
                                    kv = kv + 1
                                endif
    !                           para todos los ssiguientes segmentos de curva de ofertas de venta
                                do s = 2, NumBloVRC( u, i )
    !                               precio de segmento de venta
                                    kv = kv + 1
                                    CostoGenRC ( l ) = CostoGenRC ( l ) + PreVenEnerRC(u,s,i) * xMILP ( kv )
    !                               Costo de generacion por unidad
                                    ctgen ( u, i ) = ctgen ( u, i ) + PreVenEnerRC(u,s,i) * xMILP ( kv )
                                enddo
                            endif
    !                       si la unidad esta en sincronizacion
                            if ( xMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
    !                           costo en vacio mas costo de primer segmento
                                CostoGenRC ( l ) = CostoGenRC ( l ) + CostoMinGRC(u,i) + xMILP ( IGDARC + u + (i-1)*NumUniRC - 1 )*PreVenEnerRC(u,1,i)
                                ctgen ( u, i ) = CostoMinGRC(u,i) + xMILP ( IGDARC + u + (i-1)*NumUniRC - 1 )*PreVenEnerRC(u,1,i)
                                kv = kv + NumBloVRC( u, i )
                            endif
                        else
                            kv = kv + NumBloVRC( u, i )
                        endif
    !                   si la unidad arranco
                        if ( xMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
                            if ( NmBloArrURC ( u ) .gt. 0 ) then 
    !                           para todos los segmentos de la curva de arranque
                                do s = 1, NmBloArrURC( u )
    !                               coeficiente de segmento de arranque
                                    ka = ka + 1
                                    CostoArrRC ( l ) = CostoArrRC ( l ) + xMILP ( ka ) * CostoArrRCS ( u, s )
                                    !Costo de arranque por unidad
                                    ctarr ( u, i ) = ctarr ( u, i ) + xMILP ( ka ) * CostoArrRCS ( u, s )
                                enddo
                            else
    !                           coeficiente de costo unico de arranque
                                CostoArrRC ( l ) = CostoArrRC ( l ) + xMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) * CostArrUniURC ( u )
                                !Costo de arranque por unidad
                                ctarr ( u, i ) = xMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) * CostArrUniURC ( u )
                            endif
                        else
                            ka = ka + NmBloArrURC ( u )
                        endif
                    enddo
                    !Imprime resultados de costos de generacion y arranque por unidad y por hora por subsitema
                    write ( UniCSTOACSV, 700 ) nombunirc(u), 0, ( ctgen ( u, i ), ctarr ( u, i ), i=1,NTINTR )
                else
                    kv = kv + inicio
                    ka = ka + inicioa
                endif
    !           Para todos los intervalos
                do i = 1 , NTINTR*SiOferRes
    !               costo de reserva rodante de 10 minutos
                    cosres ( l ) = cosres ( l ) + PreVenResR10RC(u,i)*xMILP(IRR10RC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva no rodante de 10 minutos
                    cosres ( l ) = cosres ( l ) + PreVenResNR10RC(u,i)*xMILP(IRNR10RC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva rodante suplementaria
                    cosres ( l ) = cosres ( l ) + PreVenResRxRC(u,i)*xMILP(IRRSRC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva no rodante suplementaria
                    cosres ( l ) = cosres ( l ) + PreVenResNRxRC(u,i)*xMILP(IRNRSRC + u + (i-1)*NumUniRC - 1)
    !               costo de reserva de regulacion secundaria
                    cosres ( l ) = cosres ( l ) + PreVenResRegRC(u,i)*xMILP(IRRERC + u + (i-1)*NumUniRC - 1)
                enddo
            enddo        
        
            kv = IGABRD - 1
            ka = IBOARD - 1
    !       
            ctgen = 0.0
            ctarr = 0.0
            k = 1
    !       para todas las unidades de rango discontinuo
            do u = 1, NumUniRD
    !           si el generador esta en la isla
                if ( IslaGenRD ( u ) .eq. l ) then
    !               para cada intervalo
                    do i = 1, NTINTR
    !                   para todos los modos excepto el apagado
                        do modo = 2, NumModRD ( u )
    !                       si la unidad esta asignada o en sincronizacion
                            if ( xMILP ( IARD + INIURDI ( u, i ) + modo - 1  ) .gt. 0.9 .or. xMILP ( IGDARD + INIURDI ( u, i ) + modo - 1 ) .gt. 0.9 ) then
    !                           si la unidad esta asignada
                                if ( xMILP ( IARD + INIURDI ( u, i ) + modo - 1  ) .gt. 0.9 ) then
    !                               costo en vacio mas costo de primer segmento
                                    CostoGenRD ( l ) = CostoGenRD ( l ) + CostoMinGRD ( u, modo, i ) + PotMinGRD ( u, modo, i )*PreVenEnerRD (u, modo, 1, i )
                                    ctgen ( u, i ) = CostoMinGRD ( u, modo, i ) + PotMinGRD ( u, modo, i )*PreVenEnerRD (u, modo, 1, i )
    !                               para todos los siguientes segmentos de curva de ofertas de venta
                                    do s = 2, NumBloVRD( u, modo, i )
    !                                   precio de segmento de venta
                                        CostoGenRD ( l ) = CostoGenRD ( l ) + PreVenEnerRD ( u, modo, s, i ) * &
                                                                          xMILP ( kv + INBURD (u, modo, i) + s )
                                        ctgen ( u, i ) = ctgen ( u, i ) + PreVenEnerRD ( u, modo, s, i ) * xMILP ( kv + INBURD (u, modo, i) + s )
                                    enddo
                                endif
    !                           si la unidad esta en sincronizacion
                                if ( xMILP ( IGDARD + INIURDI ( u, i ) + modo - 1 ) .gt. 0.0 ) then
    !                               costo en sincronizacion
                                    CostoGenRD ( l ) = CostoGenRD ( l ) + CostoMinGRD ( u, modo, i ) + PreVenEnerRD (u, modo, 1, i )*xMILP (IGDARD + INIURDI ( u, i ) + modo - 1)
    !                               Costo de sincronizacion por unidad
                                    ctgen ( u, i ) = ctgen ( u, i ) + CostoMinGRD ( u, modo, i ) + PreVenEnerRD (u, modo, 1, i )*xMILP (IGDARD + INIURDI ( u, i ) + modo - 1)
                                endif
                            endif
    !                       si la unidad arranco
                            if ( xMILP ( IARRD + INIURDI ( u, i ) + modo - 1  ) .gt. 0.9 ) then
                                if ( NmBloArrURD ( u, modo ) .gt. 0 ) then 
    !                               para todos los segmentos de la curva de arranque
                                    do s = 1, NmBloArrURD ( u, modo )
    !                                   coeficiente de segmento de arranque
                                        CostoArrRD ( l ) = CostoArrRD ( l ) + xMILP ( ka + INBAURD (u, modo, i) + s ) * &
                                                                              CostoArrRDS ( u, modo, s )
                                        ctarr ( u, i ) = ctarr ( u, i ) + xMILP ( ka + INBAURD (u, modo, i) + s ) * CostoArrRDS ( u, modo, s )
                                    enddo
                                else
    !                               coeficiente de costo unico de arranque
                                    CostoArrRD ( l ) = CostoArrRD ( l ) + xMILP ( IARRD + INIURDI ( u, i ) + modo - 1  ) * &
                                                                          CostoTrans ( u, 1, modo )
                                    ctarr ( u, i ) = xMILP ( IARRD + INIURDI ( u, i ) + modo - 1  ) * CostoTrans ( u, 1, modo )
                                endif
                            endif
                        enddo
                    enddo
                    !Imprime resultados de costos de generacion y arranque por unidad y por hora por subsitema
                    write ( UniCSTOACSV, 700 ) nombunird(u), 1, ( ctgen ( u, i ), ctarr ( u, i ), i=1,NTINTR )
                endif
    !           Para todos los intervalos
                do i = 1 , NTINTR*SiOferRes
    !               para todos los modos de la unidad
                    do m = 1, NumModRD(u)
    !                   costo de reserva rodante de 10 minutos
                        cosres ( l ) = cosres ( l ) + PreVenResR10RD(u,m,i)*xMILP(IRR10RD + k - 1)
    !                   costo de reserva no rodante de 10 minutos
                        cosres ( l ) = cosres ( l ) + PreVenResNR10RD(u,m,i)*xMILP(IRNR10RD + k - 1)
    !                   costo de reserva rodante suplementaria
                        cosres ( l ) = cosres ( l ) + PreVenResRxRD(u,m,i)*xMILP(IRRSRD + k - 1)
    !                   costo de reserva no rodante suplementaria
                        cosres ( l ) = cosres ( l ) + PreVenResNRxRD(u,m,i)*xMILP(IRNRSRD + k - 1)
    !                   costo de reserva de regulacion secundaria
                        cosres ( l ) = cosres ( l ) + PreVenResRegRD(u,m,i)*xMILP(IRRERD + k - 1)
                        k = k + 1
                    enddo
                enddo
            enddo
        endif
        ctgen = 0.0
        ctarr = 0.0
!       para todas las unidades hidro
!       Para todos los intervalos
        do i = 1 , NTINTR
!           para todas las unidades 
            do u = 1 , NumUniHid
!               coeficiente de generacion
                CostoGenH ( l ) = CostoGenH ( l ) + xMILP ( IGH + u + (i-1)*NumUniHid - 1 )*CostoOporUH(u,i)
                ctgen ( u, i ) = ctgen ( u, i ) + xMILP ( IGH + u + (i-1)*NumUniHid - 1 )*CostoOporUH(u,i)
            enddo
        enddo
        do u = 1 , NumUniHid
            write ( UniCSTOACSV, 700 ) nombunih(u), 2, ( ctgen ( u, j ), ctarr ( u, j ), j=1,NTINTR )
!           Para todos los intervalos
            do i = 1 , NTINTR*SiOferRes
!               costo de reserva rodante de 10 minutos
                cosres ( l ) = cosres ( l ) + PreVenResR10H(u,i)*xMILP(IRR10H + u + (i-1)*NumUniHid - 1)
!               costo de reserva no rodante de 10 minutos
                cosres ( l ) = cosres ( l ) + PreVenResNR10H(u,i)*xMILP(IRNR10H + u + (i-1)*NumUniHid - 1)
!               costo de reserva rodante suplementaria
                cosres ( l ) = cosres ( l ) + PreVenResRxH(u,i)*xMILP(IRRSH + u + (i-1)*NumUniHid - 1)
!               costo de reserva no rodante suplementaria
                cosres ( l ) = cosres ( l ) + PreVenResNRxH(u,i)*xMILP(IRNRSH + u + (i-1)*NumUniHid - 1)
!               costo de reserva de regulacion secundaria
                cosres ( l ) = cosres ( l ) + PreVenResRegH(u,i)*xMILP(IRREH + u + (i-1)*NumUniHid - 1)
            enddo
        enddo
        
        ctgen = 0.0
!       para todas las unidades renovables
        kv = IGABRE
        do u = 1 , NumUniRE
!           Para todos los intervalos
            do i = 1 , NTINTR
!               para todos los segmentos de curva de ofertas de venta
                do s = 1, NumBloVRE( u, i )
!                   coeficiente de segmento de venta
                    CostoGenRI ( l ) = CostoGenRI ( l ) + xMILP ( kv )*PreVenEnerRE(u,s,i)
                    ctgen ( u, i ) = ctgen ( u, i ) + xMILP ( kv )*PreVenEnerRE(u,s,i)
                    kv = kv + 1
                enddo
            enddo
            write ( UniCSTOACSV, 700 ) nombunire(u), 3, ( ctgen ( u, i ), ctarr ( u, i ), i=1,NTINTR )
        enddo

        CostoTotGen = CostoTotGen + CostoGenRC ( l ) + CostoGenRD ( l ) + CostoGenH ( l ) + CostoGenRI ( l )
        CostoTotArr = CostoTotArr + CostoArrRC ( l ) + CostoArrRD ( l )
        CostoTotal  = CostoTotal  + CostoTotGen + CostoTotArr

!       calculo de ingresos (reserva y carga)
        ingrdem ( l ) = 0.0
!       demandas
        kd = IDBC
        do d = 1 , NumOferDem*SiOferDem
!       Para todos los intervalos
            do i = 1 , NTINTR
!               para todos los segmentos de curva de ofertas de compra
                do s = 1, NumBloDem( d, i )
!                   coeficiente de segmento de compra
                    ingrdem ( l ) = ingrdem ( l ) + PreComEner ( d, s, i ) * xMILP ( kd )
                    kd = kd + 1
                enddo
            enddo
        enddo

!       Ofertas de compra de reserva del CENACE por zona
        ingrres ( l ) = 0.0
!       para los grupos de reserva
        kr10 = ICARR10G
        k10 = ICAR10G
        ks = ICARSG
        kr = ICARRG
        do r = 1, NumGruRes*SiOferComResZona
!           para todos los intervalos
            do i = 1, NTINTR
!               para los requerimientos de reserva rodante de 10 minutos
                do s = 1, NumBloRR10( i )
!                   ingreso en la funcion objetivo de un segmento de la curva
                    ingrres ( l ) = ingrres ( l ) + PreResRR10 (r, s, i)*xMILP(kr10)
                    kr10 = kr10 + 1
                enddo
!               para los requerimientos de reserva de 10 minutos
                do s = 1, NumBloR10( i )
!                   ingreso en la funcion objetivo de un segmento de la curva
                    ingrres ( l ) = ingrres ( l ) + PreResR10 (r, s, i)*xMILP(k10)
                    k10 = k10 + 1
                enddo
!               para los requerimientos de reserva suplementaria
                do s = 1, NumBloRSu( i )
!                   ingreso en la funcion objetivo de un segmento de la curva
                    ingrres ( l ) = ingrres ( l ) + PreResSup (r, s, i)*xMILP(ks)
                    ks = ks + 1
                enddo
!               para los requerimientos de reserva de regulacion secundaria
                do s = 1, NumBloRReg( i )
!                   ingreso en la funcion objetivo de un segmento de la curva
                    ingrres ( l ) = ingrres ( l ) + PreResReg (r, s, i)*xMILP(kr)
                    kr = kr + 1
                enddo
            enddo
        enddo

!       para el sistema
        kr10 = ICARR10S
        k10 = ICAR10S
        ks = ICARSS
        kr = ICARRS
!       para todos los intervalos
        do i = 1, NTINTR*SiOferComResSis
!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRR10( i )
!               ingreso en la funcion objetivo de un segmento de la curva
                ingrres ( l ) = ingrres ( l ) + PreResRR10S (sistema, s, i)*xMILP(kr10)
                kr10 = kr10 + 1
            enddo
!           para los requerimientos de reserva de 10 minutos
            do s = 1, NumBloR10( i )
!               ingreso en la funcion objetivo de un segmento de la curva
                ingrres ( l ) = ingrres ( l ) + PreResR10S (sistema, s, i)*xMILP(k10)
                k10 = k10 + 1
            enddo
!           para los requerimientos de reserva suplementaria
            do s = 1, NumBloRSu( i )
!               ingreso en la funcion objetivo de un segmento de la curva
                ingrres ( l ) = ingrres ( l ) + PreResSupS (sistema, s, i)*xMILP(ks)
                ks = ks + 1
            enddo
!           para los requerimientos de reserva de regulacion secundaria
            do s = 1, NumBloRReg( i )
!               ingreso en la funcion objetivo de un segmento de la curva
                ingrres ( l ) = ingrres ( l ) + PreResRegS (sistema, s, i)*xMILP(kr)
                kr = kr + 1
            enddo
        enddo

	    write ( Unirdes, * ) '========================================================'
	    write ( Unirdes, * ) '==========Solucion Final para la Isla==================='
	    write ( Unirdes, * ) '========================================================'
	    write ( Unirdes, * ) ' '
	    write ( Unirdes, * ) ' '
	    write ( Unirdes, * ) '========================================================'
        write ( Unirdes, * ) '=============== Excedente Economico    : ', ingrdem ( l ) + ingrres ( l ) - CostoTotal - cosres ( l ),'============='
	    write ( Unirdes, * ) ' '
        write ( Unirdes, * ) '========== Costo Total                 : ', CostoTotal + cosres ( l ),'============='
        write ( Unirdes, * ) '========== Costo Generacion            : ', CostoTotGen,'============='
        write ( Unirdes, * ) '===========Costo de Arranque           : ', CostoTotArr,'============='
        write ( Unirdes, * ) '===========Costo de Reservas           : ', cosres ( l )     ,'============='
!	    write ( Unirdes, * ) ' '
!        write ( Unirdes, * ) '========== Costo Generacion RC         : ', CostoGenRC ( l ),'============='
!        write ( Unirdes, * ) '===========Costo de Arranque RC        : ', CostoArrRC ( l ),'============='
!        write ( Unirdes, * ) '========== Costo Generacion RD         : ', CostoGenRD ( l ),'============='
!        write ( Unirdes, * ) '===========Costo de Arranque RD        : ', CostoArrRD ( l ),'============='
	    write ( Unirdes, * ) ' '
        write ( Unirdes, * ) '========== Ingreso Total               : ', ingrdem ( l ) + ingrres ( l ),'============='
        write ( Unirdes, * ) '===========Ingreso demanda             : ', ingrdem ( l ),'============='
        write ( Unirdes, * ) '========== Ingreso Reservas            : ', ingrres ( l ),'============='
	    write ( Unirdes, * ) '========================================================'  

        close ( UniCSTOACSV )
        close ( UniBalance )

        if ( ( ite .ge. IterPerdidas .or. (SiPerdidas .eq. 0 .and. Sitransmision .eq. 0) &
               .or. (SiPerdidas .eq. 0 .and. Siviolacion .eq. 0) ) .or. imprime .eq. 1 ) then    
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            write ( aaux4, 5103 ) ingrdem ( l ) + ingrres ( l ) - CostoTotal - cosres ( l )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'056 EXC ECON(COMPRA-VENTA)'//aaux4
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            write ( aaux4, 5103 ) CostoTotal + cosres ( l )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'055 VENTA      '//aaux4
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux4, 5103 ) CostoTotGen
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'055   ENERGIA  '//aaux4
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux4, 5103 ) cosres ( l )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'055   CONEXOS  '//aaux4
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux4, 5103 ) CostoTotArr
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'055   ARRANQUE '//aaux4
            Call FechaEjecucion (fecha_Ej)
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux4, 5103 ) ingrdem ( l ) + ingrres ( l )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'054 COMPRA     '//aaux4
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux4, 5103 ) ingrdem ( l )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'054   ENERGIA  '//aaux4
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux4, 5103 ) ingrres ( l )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'054   CONEXOS  '//aaux4
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//'         '
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        endif

    endif
enddo

! se escriben resultados de reservas por zona aceptadas por CENACE
call ResZonCENACE ( ite, sistema, imprime )

! se escriben resultados de reservas por sistema aceptadas por CENACE
call ResSisCENACE ( ite, sistema, imprime )

! si el sistema contiene unidades de rango continuo
if ( NumUniRC .gt. 0 ) then

!   se escriben resultados de reservas por zona aceptadas por unidad de rango continuo
    call ResZonUniRC ( y )

!   si existen reservas por sistema
!    if ( SiOferComResSis .eq. 1 ) then
!       se escriben resultados de reservas por sistema aceptadas por unidad de rango continuo
        call ResSisUniRC
!    endif
!   Se escriben los resultados de curva de costos y costo de operacion a potencia minima modificada
!    call ImprimeCurvaCostosURC    
   
endif

! si el sistema contiene unidades de rango discontinuo
if ( NumUniRD .gt. 0 ) then

!   se escriben resultados de reservas por zona aceptadas por unidad de rango discontinuo
    call ResZonUniRD

!   si existen reservas por sistema
!    if ( SiOferComResSis .eq. 1 ) then
!       se escriben resultados de reservas por sistema aceptadas por unidad de rango discontinuo
        call ResSisUniRD
!    endif

endif
    
! si el sistema contiene unidades hidro
if ( NumUniHid .gt. 0 ) then
    
!   se escriben resultados de reservas por zona aceptadas por unidad hidro
    call ResZonUniH

!   si existen reservas por sistema
!    if ( SiOferComResSis .eq. 1 ) then
!       se escriben resultados de reservas por sistema aceptadas por unidad hidro
        call ResSisUniH
!    endif

!   se escriben resultados de energia en embalses
    call ImpEnerEH ( sistema )

endif

! si hay grupos de unidades termo con limitaciones de energia
if ( NumGruUTer .gt. 0 ) then
!if ( NResEner .gt. 0 ) then

!   se escriben resultados de unidades termo con limitaciones de energia
    call ImpEnerT ( sistema )
!    call ImpEnerT_new ( sistema )
    
endif

! Escribe solucion de asignacion y despacho

write (UnirCar,*) 'Cargas aceptadas'
write (UnirCar,*)

do l = 1, NumSis
    if ( EstadoIsla(l) .eq. 1 ) then
        sis = sis + 1
        Write( ssistema, '(I1)' )  l
!       Abre archivos csv de resultados de costos de generaciony arranque por unidad por hora por subsitema
        uunidad = 190
        UniRESCARCSV = uunidad
        OPEN ( UNIT = UniRESCARCSV, FILE = trim(rut_dat_1)//'RESCARGAS_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        kd = IDBC
!       Escribe solucion de cargas aceptadas
        do d = 1, NumOferDem
!           para todos los intervalos de planeacion
            do i = 1 , NTINTR
                DemEla ( i ) = 0.0
!               para todos los segmentos de curva de ofertas de compra
                do s = 1, NumBloDem( d, i )
!                   segmento de compra
                    DemEla ( i ) = DemEla ( i ) + xMILP ( kd )
                    kd = kd + 1
                enddo
            enddo
            j = 1
            do dia = 1, durdia
                write ( UnirCar, 600 ) d, nombcar(d), ( (DEmEla(intervalo) + DemFija(d,intervalo) - xMILP(IDF + d + (intervalo-1)*NumOferDem - 1) )*Base, &
                                                        intervalo = j, j + intdia ( dia ) - 1 )
                j = j + intdia ( dia )
            enddo
!           Imprime resultados de demnda fija aceptada ( considerando cortes ) y demanda variable aceptada por carga y por hora por subsitema
            write ( UniRESCARCSV, 701 ) nombcar(d), ( ( DemFija(d,i) - xMILP(IDF + d + (i-1)*NumOferDem - 1) ) * Base, ( DemEla ( i ) ) * Base,  i=1,NTINTR )
!           write ( UniCSTOACSV, 700 ) nombunirc(u), 0, ( ctgen ( u, i ), ctarr ( u, i ), i=1,NTINTR )
        enddo
        close ( UniRESCARCSV )
    end if
end do

close ( 889 )
close ( 13 )
close ( 14 )

100 format (i5, ',', '"', a20,'"', ',', 169(f10.2,',') )
200 format ( x, i4, 5x,f9.2, 6(2x,f9.2), f15.2, 2x,f9.2, 2(2x,f9.2), f15.2 )
300 format ( 6(f10.2, ','), f17.5, ','  )    
600 format ( i4, x, a12, 169(f9.2) )
700 format ( a12, ',', i2, ',', 338(f14.2, ',')  )    
701 format ( a12, ',', 338(f15.7, ',')  )    
5103 FORMAT (F16.2)
5100 FORMAT (I4)
5101 FORMAT (F8.2)
5102 FORMAT (A20)
5104 FORMAT (A225)

return
end
    


Subroutine ResZonCENACE ( ite, sistema, imprime )
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por zona aceptadas por CENACE    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2019                                                  *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, r, s, u, iniciou, unidad, modo, d
integer   sistema, uunidad, ierror, ibanbit, imprime

real*8    TotUniReg, TotUniRR10, TotUniRNR10, TotUniRRSup, TotUniRNRSup
real*8    TotCENACEReg, TotCENACERR10, TotCENACER10, TotCENACERSup, &
          TotDem10, TotDemS, ResCENACER10 ( 3, maxgrure, maxint ), &
          ResCENACE10 ( 3, maxgrure, maxint ), ResCENACESU ( 3, maxgrure, maxint ), &
          ResCENACERE ( 3, maxgrure, maxint )

real*8    aux, DifR10, Dif10, DifSup, DifRe
character*5 aaux1
character*15 aaux3
character*20 aaux2
character*1 ssistema
integer UniRESRO10, UniRES10, UniRESSU, UniRESRE, ite
CHARACTER fecha_Ej*19

ibanbit = 1
ierror = 0

! Se concatena el numero de subsistema al nombre de los archivos de debugger
Write( ssistema, '(I1)' )  sistema
! Resultados CSV de reservas rodantes de 10
uunidad = 130 + sistema
UniRESRO10 = uunidad
OPEN ( UNIT = UniRESRO10, FILE = trim(rut_dat_1)//'RESRERO10Z_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
! Resultados CSV de reservas de 10
uunidad = 135 + sistema
UniRES10 = uunidad
OPEN ( UNIT = UniRES10, FILE = trim(rut_dat_1)//'RESRE10Z_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
! Resultados CSV de reservas suplementaria
uunidad = 140 + sistema
UniRESSU = uunidad
OPEN ( UNIT = UniRESSU, FILE = trim(rut_dat_1)//'RESRESUZ_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
! Resultados CSV de reservas de regulacion
uunidad = 145 + sistema
UniRESRE = uunidad
OPEN ( UNIT = UniRESRE, FILE = trim(rut_dat_1)//'RESRERESEZ_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

ResCENACER10 = 0
ResCENACE10 = 0
ResCENACESU = 0
ResCENACERE = 0
DifRe = 0.0
DifR10 = 0.0
Dif10 = 0.0
DifSup = 0.0

write (Unirzn,*) 'Requerimientos del CENACE de Reserva por zona'
write (Unirzn,*)
! para todo los grupos de reserva
do r = 1, NumGruRes*SiOferComResZona
    write ( Unirzn, * )
    write ( Unirzn, * ) 'Zona : ', NomZonaRes ( r )
    write ( Unirzn, * )
    write ( Unirzn, * ) 'intervalo  ReqRR10         AcepRR10         DualRR10          ReqR10           AcepR10         DualR10          ReqRSup          AcepRSup          DualRS           ReqRReg         AcepRReg        DualRRe'
    write ( Unirzn, * ) '            (MW)             (MW)             (MW)             (MW)              (MW)            (MW)             (MW)             (MW)             (MW)              (MW)             (MW)           (MW)'
    write ( Unirzn, * ) 
!   para todos los intervalos
    do i = 1, NTINTR
        TotUniRR10 = 0.0
        TotUniRNR10 = 0.0
        TotUniRRSup = 0.0
        TotUniRNRSup = 0.0
        TotUniReg = 0.0
        TotCENACERR10 = 0.0
        TotCENACER10 = 0.0
        TotCENACERSup = 0.0
        TotCENACEReg = 0.0
        TotDem10 = 0.0
        TotDemS = 0.0
!       para las unidades de rango continuo que estan en ese grupo (zona)
        iniciou = ApunURCxZona ( r )
        do unidad = 1 , NumURCxZona ( r )*SiUniRC
            u = UniRCxZona ( iniciou )
!           total de reserva rodante de 10
            if ( SiRelRod .eq. 1 ) then
                TotUniRR10 = TotUniRR10 + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) - xMILP ( IREUSO10 + u + (i-1)*NumUniRC - 1 )
            else
                TotUniRR10 = TotUniRR10 + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 )
            endif
!           total de reserva no rodante de 10
            TotUniRNR10 = TotUniRNR10 + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )
!           total de reserva rodante suplementaria
            TotUniRRSup = TotUniRRSup + xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )
!           total de reserva no rodante suplementaria
            TotUniRNRSup = TotUniRNRSup + xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )
!           total de reserva de regulacion
            if ( TipoEjecu .le. 1 ) then
                TotUniReg = TotUniReg + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) - xMILP ( IREUSORE + u + (i-1)*NumUniRC - 1 )
            else
                TotUniReg = TotUniReg + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )
            endif
            iniciou = iniciou + 1
        enddo
!       para las unidades de rango discontinuo que estan en ese grupo (zona)
        iniciou = ApunURDxZona ( r )
        do unidad = 1 , NumURDxZona ( r )*SiUniRD
            u = UniRDxZona ( iniciou )
!           para todos los modos de operacion
            do modo = 1, NumModRD(u)
!               total de reserva rodante de 10
                TotUniRR10 = TotUniRR10 + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 )
!               total de reserva no rodante de 10
                TotUniRNR10 = TotUniRNR10 + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )
!               total de reserva rodante suplementaria
                TotUniRRSup = TotUniRRSup + xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 )
!               total de reserva no rodante suplementaria
                TotUniRNRSup = TotUniRNRSup + xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )
!               total de reserva de regulacion
                TotUniReg = TotUniReg + xMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 )
            enddo
            iniciou = iniciou + 1
        enddo
!       para las unidades hidro que estan en ese grupo (zona)
        iniciou = ApunUHxZona ( r )*SiUniH
        do unidad = 1 , NumUHxZona ( r )
            u = UniHxZona ( iniciou )
!           total de reserva rodante de 10
            TotUniRR10 = TotUniRR10 + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 )
!           total de reserva no rodante de 10
            TotUniRNR10 = TotUniRNR10 + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )
!           total de reserva rodante suplementaria
            TotUniRRSup = TotUniRRSup + xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 )
!           total de reserva no rodante suplementaria
            TotUniRNRSup = TotUniRNRSup + xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )
!           total de reserva de regulacion
            if ( TipoEjecu .le. 1 ) then
                TotUniReg = TotUniReg + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) - xMILP ( IREUSOREH + u + (i-1)*NumUniHid - 1 )
            else
                TotUniReg = TotUniReg + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 )
            endif
            iniciou = iniciou + 1
        enddo
!       para las las demandas controlables de 10 minutos y suplementaria que estan en ese grupo (zona)
        iniciou = ApunCarxZona ( r )
        do unidad = 1 , NumCarxZona ( r )*0
            d = CarxZona ( iniciou )
!           demanda controlable de 10
            TotDem10 = TotDem10 + xMILP ( ICC10 + d + (i-1)*NumOferDem - 1 )
!           demanda controlable suplementaria
            TotDemS = TotDemS + xMILP ( ICCS + d + (i-1)*NumOferDem - 1 )
            iniciou = iniciou + 1
        enddo
        TotDem10 = 0.0
        TotDemS = 0.0
!       para los requerimientos de reserva rodante de 10 minutos
        do s = 1, NumBloRR10( i )!maxsegce !
!           si la reserva de regulacion no contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 0 ) then
                TotCENACERR10 = TotCENACERR10 + ReqResR10 (r, s, i)
            else
                TotCENACERR10 = TotCENACERR10 + ReqResR10 (r, s, i) + ReqResReg (r, s, i)
            endif
        enddo
!       para los requerimientos de reserva de 10 minutos
        do s = 1, NumBloR10( i ) !maxsegce !
!           si la reserva de regulacion no contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 0 ) then
                TotCENACER10 = TotCENACER10 + ReqRes10 (r, s, i) + ReqResR10 (r, s, i)
            else
                TotCENACER10 = TotCENACER10 + ReqRes10 (r, s, i) + ReqResR10 (r, s, i) + ReqResReg (r, s, i)
            endif
        enddo
!       para los requerimientos de reserva suplementaria
        do s = 1, NumBloRSu( i ) !maxsegce !
!           si la reserva de regulacion no contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 0 ) then
                TotCENACERSup = TotCENACERSup + ReqResSup (r, s, i) + ReqRes10 (r, s, i) + ReqResR10 (r, s, i)
            else
                TotCENACERSup = TotCENACERSup + ReqResSup (r, s, i) + ReqRes10 (r, s, i) + ReqResR10 (r, s, i) + ReqResReg (r, s, i)
            endif
        enddo
!       para los requerimientos de reserva de regulacion secundaria
        do s = 1, NumBloRReg( i ) !maxsegce !
            TotCENACEReg = TotCENACEReg + ReqResReg (r, s, i)
        enddo
!       si la reserva de regulacion no contribuye a la reserva rodante
        if ( SiRegEnRod .eq. 0 ) then
            write ( Unirzn, 200 ) i, TotCENACERR10*Base, (TotUniRR10)*Base, (dualresr10z(r,i))/base, &
                                  TotCENACER10*Base, (TotUniRR10+TotUniRNR10+TotDem10)*Base, (dualres10z(r,i))/base, &
                                  TotCENACERSup*Base, (TotUniRR10+TotUniRNR10+TotUniRRSup+TotUniRNRSup+TotDem10+TotDemS)*Base, &
                                  dualressz(r,i)/base, TotCENACEReg*Base, TotUniReg*Base, dualresrez(r,i)/base
        else
            write ( Unirzn, 200 ) i, TotCENACERR10*Base, (TotUniReg+TotUniRR10)*Base, (dualresr10z(r,i))/base, &
                                  TotCENACER10*Base, (TotUniReg+TotUniRR10+TotUniRNR10+TotDem10)*Base, (dualres10z(r,i))/base, &
                                  TotCENACERSup*Base, (TotUniReg+TotUniRR10+TotUniRNR10+TotUniRRSup+TotUniRNRSup+TotDem10+TotDemS)*Base, &
                                  dualressz(r,i)/base, TotCENACEReg*Base, TotUniReg*Base, dualresrez(r,i)/base
        endif
        ResCENACER10 ( 1, r, i ) = TotCENACERR10*Base
!       si la reserva de regulacion no contribuye a la reserva rodante
        if ( SiRegEnRod .eq. 0 ) then
            ResCENACER10 ( 2, r, i ) = (TotUniRR10)*Base
        else
            ResCENACER10 ( 2, r, i ) = (TotUniReg+TotUniRR10)*Base
        endif
        ResCENACER10 ( 3, r, i ) = ( dualresr10zesc(r,i) ) /base
        ResCENACE10 ( 1, r, i ) = TotCENACER10*Base
!       si la reserva de regulacion no contribuye a la reserva rodante
        if ( SiRegEnRod .eq. 0 ) then
            ResCENACE10 ( 2, r, i ) = (TotUniRR10+TotUniRNR10+TotDem10)*Base
        else
            ResCENACE10 ( 2, r, i ) = (TotUniReg+TotUniRR10+TotUniRNR10+TotDem10)*Base
        endif
        ResCENACE10 ( 3, r, i ) = ( dualres10zesc(r,i) ) /base
        ResCENACESU ( 1, r, i ) = TotCENACERSup*Base
!       si la reserva de regulacion no contribuye a la reserva rodante
        if ( SiRegEnRod .eq. 0 ) then
            ResCENACESU ( 2, r, i ) = (TotUniRR10+TotUniRNR10+TotUniRRSup+TotUniRNRSup+TotDem10+TotDemS)*Base
        else
            ResCENACESU ( 2, r, i ) = (TotUniReg+TotUniRR10+TotUniRNR10+TotUniRRSup+TotUniRNRSup+TotDem10+TotDemS)*Base
        endif
        ResCENACESU ( 3, r, i ) = dualresszesc(r,i)/base
        ResCENACERE ( 1, r, i ) = TotCENACEReg*Base
        ResCENACERE ( 2, r, i ) = TotUniReg*Base
        ResCENACERE ( 3, r, i ) = dualresrezesc(r,i)/base
    enddo
    write ( Unirzn, * ) 
enddo

! escribe CSV para todos los intervalos, zonas y tipos de reservas
do i = 1, NTINTR
    write ( UniRESRO10, 10 ) ( ( ResCENACER10 ( 1, r, i ),  ResCENACER10 ( 2, r, i ), ResCENACER10 ( 3, r, i ) ) , r = 1, NumGruRes )
    write ( UniRES10, 10 ) ( ( ResCENACE10 ( 1, r, i ),  ResCENACE10 ( 2, r, i ), ResCENACE10 ( 3, r, i ) ) , r = 1, NumGruRes )
    write ( UniRESSU, 10 ) ( ( ResCENACESU ( 1, r, i ),  ResCENACESU ( 2, r, i ), ResCENACESU ( 3, r, i ) ) , r = 1, NumGruRes )
    write ( UniRESRE, 10 ) ( ( ResCENACERE ( 1, r, i ),  ResCENACERE ( 2, r, i ), ResCENACERE ( 3, r, i ) ) , r = 1, NumGruRes )
enddo

if ( ( ite .ge. IterPerdidas .or. imprime .eq. 1 .or. (SiPerdidas .eq. 0 .and. Sitransmision .eq. 0) &
     .or. (SiPerdidas .eq. 0 .and. Siviolacion .eq. 0) ) .and. SiOferComResZona .gt. 0 ) then
    
!   se detecta alguna escasez en reserva de zonas
    do i = 1, NTINTR
!       para todas las zonas
        do r = 1, NumGruRes
!           Si existe escasez de reserva de regulacion
            DifRe = ResCENACERE ( 1, r, i ) - ResCENACERE ( 2, r, i )
            if ( ReqResReg ( r, 1, i ) .gt. 0.0 .and. DifRe .gt. 1.0d-2 ) then
                SemBandera ( 3 ) = 1
                Call FechaEjecucion (fecha_Ej)
                bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA DE REGULACION'
                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                write ( aaux1, 5100 ) i 
                write ( aaux2, 5102 ) NomZonaRes(r)
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV ZONA: '//aaux1//aaux2
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                aux = DifRe
                write ( aaux3, 5101 )  aux
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            endif
!           Si existe escasez de reserva rodante de 10 minutos
!           si la reserva de regulacion no contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 0 ) then
                DifR10 = ResCENACER10 ( 1, r, i ) - ResCENACER10 ( 2, r, i )
            else
                DifR10 = (ResCENACER10 ( 1, r, i ) - ResCENACERE ( 1, r, i )) - (ResCENACER10 ( 2, r, i ) - ResCENACERE ( 2, r, i )) + DifRe
            endif
            if ( ReqResR10 (r, 1, i) .gt. 0.0 .and. DifR10 .gt. 1.0d-2 ) then
                SemBandera ( 3 ) = 1
                Call FechaEjecucion (fecha_Ej)
                bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA RODANTE DE 10'
                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                write ( aaux1, 5100 ) i 
                write ( aaux2, 5102 ) NomZonaRes(r)
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV ZONA: '//aaux1//aaux2
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                aux = DifR10
                write ( aaux3, 5101 )  aux
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                if ( DifR10 .lt. 0.0 ) then
                    DifR10 = -DifR10
                else
                    DifR10 = 0.0
                endif
            endif
!           Si existe escasez de reserva de 10 minutos
            Dif10 = (ResCENACE10 ( 1, r, i ) - ResCENACER10 ( 1, r, i )) - (ResCENACE10 ( 2, r, i ) - ResCENACER10 ( 2, r, i )) + DifR10
            if ( ReqRes10 (r, 1, i) .gt. 0.0 .and. Dif10 .gt. 1.0d-2 ) then
                SemBandera ( 3 ) = 1
                Call FechaEjecucion (fecha_Ej)
                bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA DE 10'
                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                write ( aaux1, 5100 ) i 
                write ( aaux2, 5102 ) NomZonaRes(r)
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV ZONA: '//aaux1//aaux2
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                aux = Dif10
                write ( aaux3, 5101 )  aux
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                if ( Dif10 .lt. 0.0 ) then
                    Dif10 = -Dif10
                else
                    Dif10 = 0.0
                endif
            endif
!           Si existe escasez de reserva suplementaria
            DifSup = (ResCENACESU ( 1, r, i ) - ResCENACE10 ( 1, r, i )) - (ResCENACESU ( 2, r, i ) - ResCENACE10 ( 2, r, i )) + Dif10
            if ( ReqResSup (r, 1, i) .gt. 0.0 .and. DifSup .gt. 1.0d-2 ) then
                SemBandera ( 3 ) = 1
                Call FechaEjecucion (fecha_Ej)
                bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA SUPLEMENTARIA'
                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                write ( aaux1, 5100 ) i 
                write ( aaux2, 5102 ) NomZonaRes(r)
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV ZONA: '//aaux1//aaux2
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                aux = DifSup
                write ( aaux3, 5101 )  aux
                Call FechaEjecucion (fecha_Ej)
                BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            endif
        enddo
    enddo
endif
     
close ( UniRESRO10 )
close ( UniRES10 )
close ( UniRESSU )
close ( UniRESRE )

!close ( Unirzn )

200 format ( x, i3, 5x,f9.2, 11(2x,f15.2) )
10  format ( 60 (f10.2, ',',f10.2, ',',f15.2, ',')  )
5100 FORMAT (I3)
5101 FORMAT (F8.2)
5102 FORMAT (A20)

return
end

    
    
Subroutine ResSisCENACE ( ite, sistema, imprime )
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por sistema aceptadas por CENACE *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2018                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, kr10, k10, ks, kr, r, s, u, l, modo, d, sistema
real*8    TotUniReg, TotUniRR10, TotUniRNR10, TotUniRRSup, TotUniRNRSup
real*8    TotCENACEReg, TotCENACERR10, TotCENACER10, TotCENACERSup, &
          TotDem10, TotDemS, ResCENACE ( 4, 3, maxint )

real*8    aux, DifR10, Dif10, DifSup 
character*5 aaux1
character*15 aaux3
character*20 aaux2

character*1 ssistema
integer UniRESSIS, uunidad, ierror, ibanbit, ite, imprime
CHARACTER fecha_Ej*19

ibanbit = 1
ierror = 0

! Se concatena el numero de subsistema al nombre de los archivos de debugger
Write( ssistema, '(I1)' )  sistema
! Resultados CSV de reservas de sistema
uunidad = 130 + sistema
UniRESSIS = uunidad
OPEN ( UNIT = UniRESSIS, FILE = trim(rut_dat_1)//'RESRESSIS_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

ResCENACE = 0
DifR10 = 0.0
Dif10 = 0.0
DifSup = 0.0

kr10 = ICARR10S
k10 = ICAR10S
ks = ICARSS
kr = ICARRS
l = 0

write (Unirsn,*) 'Requerimientos del CENACE de Reserva por sistema'
write (Unirsn,*)
! para todo los sistemas
do r = 1, numsis*SiOferComResSis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        l = l + 1
        write ( Unirsn, * )
        write ( Unirsn, * ) 'Sistema : ', nomsis ( r )
        write ( Unirsn, * )
        write ( Unirsn, * ) 'intervalo  ReqRR10   AcepRR10    DualRR10    ReqR10    AcepR10    DualR10    ReqRSup    AcepRSup   DualRS     ReqRReg    AcepRReg  DualRRe'
        write ( Unirsn, * ) '            (MW)       (MW)       (MW)       (MW)        (MW)       (MW)       (MW)       (MW)      (MW)       (MW)       (MW)      (MW) '
        write ( Unirsn, * ) 
    !   para todos los intervalos
        do i = 1, NTINTR
            TotUniRR10 = 0.0
            TotUniRNR10 = 0.0
            TotUniRRSup = 0.0
            TotUniRNRSup = 0.0
            TotUniReg = 0.0
            TotCENACERR10 = 0.0
            TotCENACER10 = 0.0
            TotCENACERSup = 0.0
            TotCENACEReg = 0.0
            TotDem10 = 0.0
            TotDemS = 0.0
!           para las unidades de rango continuo que estan en el sistema
            do u = 1 , NumUniRC
                if ( IslaGenRC ( u ) .eq. r ) then
!                   total de reserva rodante de 10
!                    TotUniRR10 = TotUniRR10 + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )
                    TotUniRR10 = TotUniRR10 + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 )
!                   total de reserva no rodante de 10
                    TotUniRNR10 = TotUniRNR10 + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )
!                   total de reserva rodante suplementaria
                    TotUniRRSup = TotUniRRSup + xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )
!                   total de reserva no rodante suplementaria
                    TotUniRNRSup = TotUniRNRSup + xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )
!                   total de reserva de regulacion
                    TotUniReg = TotUniReg + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )
                endif
            enddo
!           para las unidades de rango discontinuo que estan en el sistema
            do u = 1 , NumUniRD
                if ( IslaGenRD ( u ) .eq. r ) then
!                   para todos los modos de operacion
                    do modo = 1, NumModRD(u)
!                       total de reserva rodante de 10
!                        TotUniRR10 = TotUniRR10 + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 )
                        TotUniRR10 = TotUniRR10 + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 )
!                       total de reserva no rodante de 10
                        TotUniRNR10 = TotUniRNR10 + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )
!                       total de reserva rodante suplementaria
                        TotUniRRSup = TotUniRRSup + xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 )
!                       total de reserva no rodante suplementaria
                        TotUniRNRSup = TotUniRNRSup + xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )
!                       total de reserva de regulacion
                        TotUniReg = TotUniReg + xMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 )
                    enddo
                endif
            enddo
!           para las unidades hidro que estan en el sistema
            do u = 1 , NumUniHid
                if ( IslaGenH ( u ) .eq. r ) then
!                   total de reserva rodante de 10
!                    TotUniRR10 = TotUniRR10 + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 )
                    TotUniRR10 = TotUniRR10 + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 )
!                   total de reserva no rodante de 10
                    TotUniRNR10 = TotUniRNR10 + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )
!                   total de reserva rodante suplementaria
                    TotUniRRSup = TotUniRRSup + xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 )
!                   total de reserva no rodante suplementaria
                    TotUniRNRSup = TotUniRNRSup + xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )
!                   total de reserva de regulacion
                    TotUniReg = TotUniReg + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 )
                endif
            enddo
!           para las demandas controlables de 10 minutos y suplementaria que estan en el sistema 
            do d = 1 , NumOferDem*0
!               si la demanda pertenece al sistema
                if ( IslaDem ( d ) .eq. r ) then
!                   demanda controlable de 10
                    TotDem10 = TotDem10 + xMILP ( ICC10 + d + (i-1)*NumOferDem - 1 )
!                   demanda controlable suplementaria
                    TotDemS = TotDemS + xMILP ( ICCS + d + (i-1)*NumOferDem - 1 )
                endif
            enddo
            TotDem10 = 0.0
            TotDemS = 0.0
!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRR10( i )
                TotCENACERR10 = TotCENACERR10 + ReqResR10S (l, s, i)
                kr10 = kr10 + 1
            enddo
!           para los requerimientos de reserva de 10 minutos
            do s = 1, NumBloR10( i )
                TotCENACER10 = TotCENACER10 + ReqRes10S (l, s, i) + ReqResR10S (l, s, i)
                k10 = k10 + 1
            enddo
!           para los requerimientos de reserva suplementaria
            do s = 1, NumBloRSu( i )
                TotCENACERSup = TotCENACERSup + ReqResSupS (l, s, i) + ReqRes10S (l, s, i) + ReqResR10S (l, s, i)
                ks = ks + 1
            enddo
!           para los requerimientos de reserva de regulacion secundaria
            do s = 1, NumBloRReg( i )
                TotCENACEReg = TotCENACEReg + ReqResRegS (l, s, i)
                kr = kr + 1
            enddo
!            write ( Unirsn, 200 ) i, TotCENACERR10*Base, TotUniRR10*Base, (dualresr10s(r,i)+dualres10s(r,i)+dualresss(r,i))/base, &
!                                  TotCENACER10*Base, (TotUniRR10+TotUniRNR10+TotDem10)*Base, (dualres10s(r,i)+dualresss(r,i))/base, &
            write ( Unirsn, 200 ) i, TotCENACERR10*Base, TotUniRR10*Base, (dualresr10s(r,i))/base, &
                                  TotCENACER10*Base, (TotUniRR10+TotUniRNR10+TotDem10)*Base, (dualres10s(r,i))/base, &
                                  TotCENACERSup*Base, (TotUniRR10+TotUniRNR10+TotUniRRSup+TotUniRNRSup+TotDem10+TotDemS)*Base, &
                                  dualresss(r,i)/base, TotCENACEReg*Base, TotUniReg*Base, dualresres(r,i)/base
            ResCENACE ( 1, 1, i ) = TotCENACERR10*Base
            ResCENACE ( 1, 2, i ) = TotUniRR10*Base
!            ResCENACE ( 1, 3, i ) = ( dualresr10s(r,i) + dualres10s(r,i) + dualresss(r,i) ) /base
            ResCENACE ( 1, 3, i ) = ( dualresr10s(r,i) ) /base
            ResCENACE ( 2, 1, i ) = TotCENACER10*Base
            ResCENACE ( 2, 2, i ) = (TotUniRR10+TotUniRNR10+TotDem10)*Base
!            ResCENACE ( 2, 3, i ) = ( dualres10s(r,i) + dualresss(r,i) ) /base
            ResCENACE ( 2, 3, i ) = ( dualres10s(r,i) ) /base
            ResCENACE ( 3, 1, i ) = TotCENACERSup*Base
            ResCENACE ( 3, 2, i ) = (TotUniRR10+TotUniRNR10+TotUniRRSup+TotUniRNRSup+TotDem10+TotDemS)*Base
            ResCENACE ( 3, 3, i ) = dualresss(r,i)/base
            ResCENACE ( 4, 1, i ) = TotCENACEReg*Base
            ResCENACE ( 4, 2, i ) = TotUniReg*Base
            ResCENACE ( 4, 3, i ) = dualresres(r,i)/base
        enddo
        write ( Unirsn, * ) 
    endif
enddo

! escribe CSV para todos los intervalos, zonas y tipos de reservas
do i = 1, NTINTR
    write ( UniRESSIS, 10 ) ( ( ResCENACE ( r, 1, i ),  ResCENACE ( r, 2, i ), ResCENACE ( r, 3, i ) ) , r = 1, 4 )
enddo

if ( ( ite .ge. IterPerdidas .or. imprime .eq. 1 .or. (SiPerdidas .eq. 0 .and. Sitransmision .eq. 0) &
     .or. (SiPerdidas .eq. 0 .and. Siviolacion .eq. 0) ) .and. SiOferComResSis .gt. 0 ) then

!   se detecta alguna escasez en reserva del sistema
    do i = 1, NTINTR
!       Si existe escasez de reserva rodante de 10 minutos
        DifR10 = ResCENACE ( 1, 1, i ) - ResCENACE ( 1, 2, i )
        if ( ReqResR10S (1, 1, i) .gt. 0.0 .and.  DifR10 .gt. 1.0d-2 ) then
            SemBandera ( 3 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA RODANTE DE 10'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux1, 5100 ) i 
            write ( aaux2, 5102 ) nomsis(sistema)
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV SISTEMA: '//aaux1//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            aux = DifR10 ! ResCENACE ( 1, 1, i ) - ResCENACE ( 1, 2, i )
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            if ( DifR10 .lt. 0.0 ) then
                DifR10 = -DifR10
            else
                DifR10 = 0.0
            endif
        endif
!       Si existe escasez de reserva de 10 minutos
!        if ( ReqRes10S (1, 1, i) .gt. 0.0 .and. (ResCENACE ( 2, 1, i ) - ResCENACE ( 2, 2, i )) .gt. 1.0d-2 ) then
        Dif10 = (ResCENACE ( 2, 1, i ) - ResCENACE ( 1, 1, i )) - (ResCENACE ( 2, 2, i ) - ResCENACE ( 1, 2, i )) + DifR10
        if ( ReqRes10S (1, 1, i) .gt. 0.0 .and. Dif10 .gt. 1.0d-2 ) then
            SemBandera ( 3 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA DE 10'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux1, 5100 ) i 
            write ( aaux2, 5102 ) nomsis(sistema)
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV SISTEMA: '//aaux1//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            aux = Dif10 ! ResCENACE ( 2, 1, i ) - ResCENACE ( 2, 2, i )
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            if ( Dif10 .lt. 0.0 ) then
                Dif10 = -Dif10
            else
                Dif10 = 0.0
            endif
        endif
!       Si existe escasez de reserva suplementaria
!        if ( ReqResSupS (1, 1, i) .gt. 0.0 .and. (ResCENACE ( 3, 1, i ) - ResCENACE ( 3, 2, i )) .gt. 1.0d-2 ) then
        DifSup = (ResCENACE ( 3, 1, i ) - ResCENACE ( 2, 1, i )) - (ResCENACE ( 3, 2, i ) - ResCENACE ( 2, 2, i )) + Dif10
        if ( ReqResSupS (1, 1, i) .gt. 0.0 .and. DifSup .gt. 1.0d-2 ) then
            SemBandera ( 3 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA SUPLEMENTARIA'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux1, 5100 ) i 
            write ( aaux2, 5102 ) nomsis(sistema)
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV SISTEMA: '//aaux1//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            aux = DifSup ! ResCENACE ( 3, 1, i ) - ResCENACE ( 3, 2, i )
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        endif
!       Si existe escasez de reserva de regulacion
        if ( ResCENACE ( 4, 1, i ) - ResCENACE ( 4, 2, i ) .gt. 1.0d-2 ) then
            SemBandera ( 3 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ EN RESERVA DE REGULACION'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux1, 5100 ) i 
            write ( aaux2, 5102 ) nomsis(sistema)
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV SISTEMA: '//aaux1//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            aux = ResCENACE ( 4, 1, i ) - ResCENACE ( 4, 2, i )
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 ESCASEZ (MW)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        endif
    enddo
endif

close ( UniRESSIS )
!close ( Unirsn )

200 format ( x, i3, 5x,f9.2, 11(2x,f9.2) )
10  format ( 4 (f10.2, ',',f10.2, ',',f10.3, ',')  )
5100 FORMAT (I3)
5101 FORMAT (F8.2)
5102 FORMAT (A20)

return
end


! ---------------------------------------------------------------------
! Imprime resultados de despachos para unidades renovables.           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine AsigDesRE

use ParAUHE
use ProblemaAUHE

Implicit none

INTEGER   i, u, k, bloque


! Escribe solucion de despacho

write (UnichauRE,*) 'Generacion de unidades renovables'
write (UnichauRE,*)

! Escribe solucion de generacion de unidades renovables
do u = 1, NumUniRE
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque
       write ( UnichauRE, 600 ) u, nombunire(u), ( GENUNRE(u,i)*Base, i=k*24-23,k*24 )
    enddo
    do i = 1, NTINTR
!        if ( GENUNRE(u,i) .gt. 0 ) then
        if ( xMILP ( IARE + u + (i-1)*NumUniRE - 1 ) .gt. 0.9 .and. DispoURE ( u, i) .gt.0 ) then
            RESMODO ( u+NumUniRC+NumUniRD+NumUniHid, i ) = 1
        endif
    enddo
enddo

600 format ( i4, x, a8, 169(f9.2) )

return
end
    
    
! ---------------------------------------------------------------------
! Imprime resultados de flujos en grupos de ramas restringidas.       *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine ImprimeFujos ( ite, sistema, imprime )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, dualgrurams, dualgrurami, &
                     nomgruram, potmaxgruram, potmingruram, &
                     inagruram, bangruram, bangruramcopy, &
                     ApuEleGruRam, LisEleGruRam, &
                     SentEleGruRam, oriram, desram, nomnod, nomram, &
                     RamActiva, nomgruram
Implicit none

INTEGER   i, j, rama, grupo, sistema, br, noviola, uunidad, ConjuntoActivo ( maxgruram )
real*8    flujo ( maxgruram, maxint ), aux, violacions, violacioni, &
          dual ( maxint ), flurar( maxelegruram, maxint ), sumflujo

integer UniFlujoCSV, UniFlujosRamas, ite, imprime

integer   ibanbit, ierror
CHARACTER fecha_Ej*19
character*5 aaux1
character*15 aaux3
character*20 aaux2
character*1 ssistema

ibanbit = 1
ierror = 0
noviola = 0
SiViolacion = 1

! Se concatena el numero de subsistema al nombre de los archivos de debugger
Write( ssistema, '(I1)' )  sistema
! Resultados CSV de flujos de grupos de ramas regtringidas
uunidad = 130 + sistema
UniFlujoCSV = uunidad
UniFlujosRamas = 131 + sistema
OPEN ( UNIT = UniFlujoCSV, FILE = trim(rut_dat_1)//'GRUPOSRAMASRES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

OPEN ( UNIT = UniFlujosRamas, FILE = trim(rut_dat_1)//'RAMASENLRES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

! si existen grupos de ramas restringidos en el subsistema
if ( NumGruRamSis ( sistema ) .gt. 0 ) then
!   Se calculan flujos para imprimir resultados
    call CalFlujos ( ite, flujo, flurar, sistema )
endif

! Se imprimen flujos en grupos de ramas restringidas
do grupo = 1, NumGruRamSis ( sistema )
    br = inagruram ( grupo )
    write ( UniFlujo, * ) ' '
    write ( UniFlujo, * ) 'Numero de grupo de ramas :', grupo
    write ( UniFlujo, * ) ' '
    write ( UniFlujo, * ) 'inter           Grupo               Flujo      L Superior  L Inferior   Excedente    Deficit         dual sup        dual inf'
    write ( UniFlujo, * ) '                                     (MW)         (MW)        (MW)        (MW)        (MW)'
    write ( UniFlujo, * ) ' '
!   para todos los intervalos
    do i = 1, NTINTR
        write ( UniFlujo, 300 ) i, nomgruram(grupo), flujo ( grupo, i )*Base, potmaxgruram ( br, i )*Base,  &
                          potmingruram ( br, i )*Base , xMILP (IAEF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1)*Base, &
                          xMILP (IACF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1)*Base,  &
                          dualgrurams ( br, i )/Base, dualgrurami ( br, i )/Base
        dual ( i ) = dualgrurams ( br, i )/Base + dualgrurami ( br, i )/Base
!       se identifican los grupos de ramas en los que se viola el limite de transmision
        violacions = flujo ( grupo, i ) - potmaxgruram ( br, i ) - xMILP (IAEF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1) + &
                                                                   xMILP (IACF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1)
        violacioni = flujo ( grupo, i ) - potmingruram ( br, i ) - xMILP (IAEF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1) + &
                                                                   xMILP (IACF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1)
        if ( (violacions .gt. 1.0e-6 .or. violacioni .lt. -1.0e-6) .and. bangruramcopy ( grupo ) .eq. 1 ) then
!           si se viola una nueva rama
            if ( RamActiva ( grupo ) .eq. 0 ) then
!               se incorpora un grupo de rama al conjunto activo
                bangruram ( grupo ) = 1
                noviola = noviola + 1
            endif
        endif
!       Si existe infactibilidad en el enlace
        if ( ( (xMILP (IAEF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1) .gt. 0.0001 .or. &
             xMILP (IACF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1) .gt. 0.0001) .and. &
             (ite .ge. IterPerdidas .or. imprime .eq. 1 ) ) .and. bangruramcopy ( grupo ) .eq. 1 .and. SiTransmision .gt. 0 ) then
            SemBandera ( 4 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD EN GRUPOS DE RAMAS'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux1, 5100 ) i 
            write ( aaux2, 5102 ) nomgruram(grupo)
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INTERV GPORAM: '//aaux1//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            aux = ( xMILP (IAEF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1) + &
                    xMILP (IACF + grupo + (i-1)*NumGruRamSis ( sistema ) - 1) )*Base
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD (MW)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        endif
             
        ! Imprime resultados de elementos de enlaces
        sumflujo = 0.0
        do j = ApuEleGruRam(br), ApuEleGruRam(br+1)-1
            rama = LisEleGruRam(j)
            write ( UniFlujosRamas, 500, iostat=ierror ) i, nomgruram(br), nomram(rama), nomnod(oriram(rama)), nomnod(desram(rama)),&
                                   SentEleGruRam(j), flurar(j,i)*base
500         format ( I3, ',', 2('"', A25, '"',','), 2('"', A25, '"',','), I2, ',', f16.9, ','  )
                                   
            sumflujo = sumflujo + flurar(j,i)*SentEleGruRam(j)
        enddo
        
    enddo
    
    write ( UniFlujoCSV, 102 ) ( ( flujo ( grupo, i )*Base,  dual(i) ) , i = 1, NTINTR )
    

enddo

! si no hubo grupos de ramas violadas o no se desea cuidar transmision
if ( noviola .eq. 0 .or. SiTransmision .eq. 0) then
    SiViolacion = 0
else
! Se imprime el conjunto activo
    write ( UniFlujo, * ) ' '
    write ( UniFlujo, * ) 'Conjunto Activo'
    write ( UniFlujo, * ) ' '
    i = 0
    do grupo = 1, NumGruRamSis ( sistema )
        if ( bangruram ( grupo ) .ne. 0 ) then
            i = i + 1
            ConjuntoActivo ( i ) = grupo
            write ( UniFlujo, * ) 'Grupo:', grupo, nomgruram(grupo)
            write ( 55, 400 )  grupo, nomgruram(grupo) 
        endif
    enddo
endif

!close ( UniFlujo )
close ( UniFlujoCSV )
close ( UniFlujosRamas )

102 format ( 169 (f10.2, ',',f16.2, ',') )
300 format ( x, i3, 3x, a25, 5(3x,f9.2), 2(2x,f16.2) )
400  format ( I3, ',', '"',A25,'"',',' )
5100 FORMAT (I3)
5101 FORMAT (F8.2)
5102 FORMAT (A20)

return
end

    
    
! ---------------------------------------------------------------------
! Calculo de flujos en grupos de ramas restringidas.                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine CalFlujos ( ite, flujo, flurar, sistema )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, SnsGruRarInyNod, inagruram, &
                     CoeSnsGruRar, dualgrurams, dualgrurami, &
                     inaumnodsis, NumNodSis, &
                     SnsEleGruRamInyNod, CoeSnsRar, ApuEleGruRam, LisEleGruRam, disram, SentEleGruRam

Implicit none

INTEGER   i, j , igru, grupo, rama, sistema, br, nodo, u, n, d, s, componente, componente_1
integer   ite, modo, NoDi
real*8    flujo ( maxgruram, maxint ), flurar( maxelegruram, maxint ), coeficiente


! para todos los intervalos
do i = 1, NTINTR
!   se calcuan sensibilidades
    call CalculaSensibilidadesFlujos ( sistema, i, 0, ite )
!   se calcula flujo en grupos de ramas restringidas
    do grupo = 1, NumGruRamSis ( sistema )
        br = inagruram ( grupo )
        flujo ( grupo, i ) = 0.0
!       generacion de rango continuo
        do u = 1 , NumUniRC
!           para todos los nodos distribuidos
            do NoDi = 1, NoNodDisRC ( u, i )
                nodo = Tempnodorc ( u, NoDi, i )
                if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURC ( u, i ) .ne. 0 ) then
                    flujo ( grupo, i ) = flujo ( grupo, i ) + SnsGruRarInyNod ( br, nodo )*facdistgen ( u, NoDi, i )*GENUNRC ( u, i )
                endif
            enddo
        enddo
!       coeficientes de variables de generacion de rango discontinuo
        do u = 1 , NumUniRD
            coeficiente = 0.0
!           para todos los modos de operacion
            do modo = 2, NumModRD(u)
!               para el modo de operacion asignado
                if ( xMILP(IARD + INIURDI ( u, i ) + modo - 1) .gt. 0.8 .or. xMILP(IADARD + INIURDI ( u, i ) + modo - 1) .gt. 0.8 ) then
                    do componente = 1, NumCompXModo ( u, modo )
!                       para todas la componentes de la unida de rango discontinuo
                        do componente_1 = 0, NumCompRD ( u ) - 1
                            if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                                nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
                                if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURD ( u, modo, i ) .ne. 0 ) then
                                    coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*GenCompXModo  ( u, modo, componente )
                                endif
                                exit
                            endif
                        enddo
                    enddo
                    exit
                endif
            enddo
            flujo ( grupo, i ) = flujo ( grupo, i ) + coeficiente * GENUNRD ( u, i )
        enddo
!       generacion hidro
        do u = 1 , NumUniHid
            nodo = nodoh ( u, i )
            if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoUH ( u, i ) .ne. 0 ) then
	            flujo ( grupo, i ) = flujo ( grupo, i ) + SnsGruRarInyNod ( br, nodo ) * GENUNH ( u, i )
            endif
        enddo
!       generacion renovables
        do u = 1 , NumUniRE
            nodo = nodounre ( u, i )
            if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURE ( u, i ) .ne. 0 ) then
	            flujo ( grupo, i ) = flujo ( grupo, i ) + SnsGruRarInyNod ( br, nodo ) * GENUNRE ( u, i )
            endif
        enddo
!       nivel de demanda y corte de carga
        do d = 1 , NumOferDem
            coeficiente = 0.0
!           para todos los nodos distribuidos
            do NoDi = 1, NoNodDisCar ( d, i )
                nodo = Tempnodocar ( d, NoDi, i )
                if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
                    coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*facdistcar ( d, NoDi, i )
                endif
            enddo
!           para todos los segmentos de curva de ofertas de compra
            do s = 1, NumBloDem( d, i )
                flujo ( grupo, i ) = flujo ( grupo, i ) - coeficiente * xMILP ( IDBC + s + INDDE ( d, i ) - 1 )
            enddo
            flujo ( grupo, i ) = flujo ( grupo, i ) + coeficiente * xMILP ( IDF + d + (i-1)*NumOferDem - 1 )
        enddo
!       excedentes
        do n = 1, NumNodSis ( sistema )
            nodo = inaumnodsis ( n )
            if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
	            flujo ( grupo, i ) = flujo ( grupo, i ) - SnsGruRarInyNod ( br, nodo ) * xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 )
            endif
        enddo
	    flujo ( grupo, i ) = flujo ( grupo, i ) - CoeSnsGruRar ( br, i )
    enddo
    
!   Calcula flujos de elementos que forman parte de un grupo de ramas
    do grupo = 1, NumGruRamSis ( sistema )
        igru = inagruram ( grupo )
        do j = ApuEleGruRam(igru), ApuEleGruRam(igru+1)-1
            rama = LisEleGruRam(j)
            flurar ( j, i ) = 0.0

            if ( disram(rama) .eq. 1 ) then
                      
!               generacion de rango continuo
                do u = 1 , NumUniRC
!                   para todos los nodos distribuidos
                    do NoDi = 1, NoNodDisRC ( u, i )
                        nodo = Tempnodorc ( u, NoDi, i )
                        if ( abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1e-10 ) then
                            flurar ( j, i ) = flurar ( j, i ) + SnsEleGruRamInyNod ( j, nodo )*facdistgen ( u, NoDi, i )*GENUNRC ( u, i )
                        endif
                    enddo
                enddo
                
!               coeficientes de variables de generacion de rango discontinuo
                do u = 1 , NumUniRD
                    coeficiente = 0.0
!                   para todos los modos de operacion
                    do modo = 2, NumModRD(u)
!                       para el modo de operacion asignado
                        if ( xMILP(IARD + INIURDI ( u, i ) + modo - 1) .gt. 0.8 .or. xMILP(IADARD + INIURDI ( u, i ) + modo - 1) .gt. 0.8 ) then
                            do componente = 1, NumCompXModo ( u, modo )
!                               para todas la componentes de la unida de rango discontinuo
                                do componente_1 = 0, NumCompRD ( u ) - 1
                                    if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                                        nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
                                        if ( abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1e-10  ) then
                                            coeficiente = coeficiente + SnsEleGruRamInyNod ( j, nodo ) *GenCompXModo  ( u, modo, componente )
                                        endif
                                        exit
                                    endif
                                enddo
                            enddo
                            exit
                        endif
                    enddo
                    flurar ( j, i ) = flurar ( j, i ) + coeficiente * GENUNRD ( u, i )
                enddo                

!               generacion hidro
                do u = 1 , NumUniHid
                    nodo = nodoh ( u, i )
                    if ( abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1e-10 ) then
	                    flurar ( j, i ) = flurar ( j, i ) + SnsEleGruRamInyNod ( j, nodo ) * GENUNH ( u, i )
                    endif
                enddo
!               generacion renovables
                do u = 1 , NumUniRE
                    nodo = nodounre ( u, i )
                    if ( abs(SnsEleGruRamInyNod ( j, nodo ) ) .gt. 1e-10 ) then
	                    flurar ( j, i ) = flurar ( j, i ) + SnsEleGruRamInyNod ( j, nodo ) * GENUNRE ( u, i )
                    endif
                enddo
!               nivel de demanda y corte de carga
                do d = 1 , NumOferDem
                    coeficiente = 0.0
!                   para todos los nodos distribuidos
                    do NoDi = 1, NoNodDisCar ( d, i )
                        nodo = Tempnodocar ( d, NoDi, i )
                        if ( abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1e-10 ) then
                            coeficiente = coeficiente + SnsEleGruRamInyNod ( j, nodo )*facdistcar ( d, NoDi, i )
                        endif
                    enddo
!                   para todos los segmentos de curva de ofertas de compra
                    do s = 1, NumBloDem( d, i )
                        flurar ( j, i ) = flurar ( j, i ) - coeficiente * xMILP ( IDBC + s + INDDE ( d, i ) - 1 )
                    enddo
                    flurar ( j, i ) = flurar ( j, i ) + coeficiente * xMILP ( IDF + d + (i-1)*NumOferDem - 1 )
                enddo
                
!               excedentes
                do n = 1, NumNodSis ( sistema )
                    nodo = inaumnodsis ( n )
                    if ( abs(SnsEleGruRamInyNod ( j, nodo )) .gt. 1e-10 ) then
	                    flurar ( j, i ) = flurar ( j, i ) - SnsEleGruRamInyNod ( j, nodo ) * xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 )
                    endif
                enddo
	            flurar ( j, i ) = flurar ( j, i ) - CoeSnsRar ( j, i )                      
                      
            endif
        enddo

    enddo
    
enddo

return
end


subroutine ImpEnerT ( sistema )
! ---------------------------------------------------------------------
! Se imprime el resultado de energia en grupos de unidades termo      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2018                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer dia, intervalo, u, unidad, grupo, iniciou, IntIni
Real*8  TotEner ( maxgrute, maxdia ), aux, dual ( maxgrute, maxdia )
Integer ierror, ibanbit, sistema, UniGenTerCSV, uunidad

CHARACTER fecha_Ej*19
character*15 aaux3
character*20 aaux2
character*5 aaux1
character*1 ssistema

ibanbit = 1
ierror = 0
TotEner = 0.0
dual = 0.0

! Abre archivos csv de resultados de generacion en los grupos termicos
uunidad = 121 + sistema
UniGenTerCSV = uunidad
Write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = UniGenTerCSV, FILE = trim(rut_dat_1)//'GPOUTERRES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 300 )

write ( UniGLimT,* ) '     GrupoTe     LinfEner     Energia     LSupEner  Precio Sombra'
write ( UniGLimT,* ) '                   (GWh)       (GWH)       (GWh)      ($/GWh)'

do grupo = 1, NumGruUTer
    IntIni = 1
    do dia = 1, durdia
!       para todos los intervalos del dia
        do intervalo = IntIni, IntIni + intdia (dia) - 1
!           para las unidades de rango continuo que estan en ese grupo
            iniciou = ApunURCxGrupo ( grupo )
            do unidad = 1 , NumURCxGrupo ( grupo )
                u = UniRCxGrupo ( iniciou )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
                    TotEner ( grupo, dia ) = TotEner ( grupo, dia ) + xMILP ( IGRC + u + (intervalo-1)*NumUniRC - 1 ) * Base
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniciou = ApunURDxGrupo ( grupo )
            do unidad = 1 , NumURDxGrupo ( grupo )
                u = UniRDxGrupo ( iniciou )
                TotEner ( grupo, dia ) = TotEner ( grupo, dia ) + GENUNRD ( u, intervalo ) * Base
                iniciou = iniciou + 1
            enddo
        enddo
        IntIni = IntIni + intdia ( dia )
        dual (grupo, dia) = ( dualigpter (grupo, dia) + dualsgpter (grupo,dia) ) / Base
!       Si existe infactibilidad en el grupo
        if ( xMILP ( IARGT + NumGruUTer*(dia-1) + grupo - 1 ) .gt. 1.0d-3 .and. SiEnerTer .gt. 0 ) then
            SemBandera ( 5 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD EN ENERGIA TERMO'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux2, 5102 ) NomGpoTer ( grupo )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 GRUPO ENERGIA: '//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux1, 5100 ) dia 
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 DIA          : '//aaux1
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            aux = xMILP ( IARGT + NumGruUTer*(dia-1) + grupo - 1  )/10.0
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD (GWh)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        endif
    enddo
enddo

TotEner = TotEner/1000.0

do grupo = 1, NumGruUTer
    do dia = 1, durdia
        write ( UniGLimT, 100 ) grupo, NomGpoTer ( grupo ), LimEnerIUTermo ( grupo, dia )*base/1000.0, &
                                TotEner ( grupo, dia ), LimEnerSUTermo ( grupo, dia )*base/1000.0, -dual ( grupo, dia )
    enddo
    write ( UniGLimT,* ) 
!   imprime resultado de generacion total en el grupo
    write ( UniGenTerCSV, 300 ) NomGpoTer ( grupo ), ( TotEner ( grupo, dia ), -dual ( grupo, dia ), dia = 1, durdia )
enddo

100 FORMAT ( i4,3x, a6, 4(2x,f10.2) )
300 FORMAT ( a12, ',', 7( 2(f10.2,',')) )
5101 FORMAT (F8.2)
5102 FORMAT (A20)
5100 FORMAT (I3)

!close ( UniGLimT )
close ( UniGenTerCSV )

return
end    

    
! ---------------------------------------------------------------------
! Imprime resultados de costos marginales por region.                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Junio 2019                                                          *
! ---------------------------------------------------------------------
Subroutine ImprimeMargReg ( CorteReg, ExceReg, PotGenReg, DemReg, PmlMaxReg, PmlMinReg, DesvStdReg, MargPonDemReg, MargPonGenReg )

Use ParAUHE, only: NTINTR, Base, Unimarreg, maxint, rut_dat_1

Use ParGloRed, only: MargRegional, NumRegPre, nomregpre, maxregpre

Implicit none


integer i, intervalo, ierror

real*8 CorteReg ( maxregpre, maxint ), ExceReg ( maxregpre, maxint ), PmlMaxReg ( maxregpre, maxint ), PmlMinReg ( maxregpre, maxint ), DesvStdReg ( maxregpre, maxint )
real*8 MargPonDemReg  ( maxregpre, maxint ),  MargPonGenReg  ( maxregpre, maxint ), PotGenReg ( maxregpre, maxint ), DemReg ( maxregpre, maxint )

character*1 let

write (Unimarreg,*) 'Marginales'
write (Unimarreg,*)

let = "1"
OPEN ( UNIT = 210, FILE = trim(rut_dat_1)//'RESREGIONES_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Imprime costos marginales regionales
do intervalo = 1, NTINTR
  write ( Unimarreg, * )
  write ( Unimarreg, * ) 'Intervalo: ', intervalo
  write ( Unimarreg, * )
  write ( Unimarreg, * ) '      Region                      CMENRE  '
  write ( Unimarreg, * )
  do i = 1, NumRegPre
    if ( MargRegional( i , intervalo ) .ne. 0.0 ) then
      write ( Unimarreg, 100 ) i, nomregpre(i), MargRegional( i , intervalo )/Base
    endif
    write ( 210, 200, iostat = ierror ) intervalo, i, nomregpre(i), CorteReg ( i, intervalo )*base, ExceReg ( i, intervalo )*Base, &
                                        PotGenReg ( i, intervalo )*Base, DemReg ( i, intervalo )*Base, MargRegional ( i, intervalo )/Base, &
                                        PmlMinReg ( i, intervalo )/Base, PmlMaxReg ( i, intervalo )/Base,DesvStdReg ( i, intervalo )/base, & 
                                        MargPonDemReg ( i, intervalo )/Base, MargPonGenReg ( i, intervalo )/Base
  enddo
enddo

! cierra archivo de resultados de costos marginales regionales por intervalo
!close ( UNIT = Unimarreg )

100 format ( 2x, i4, 2x, a20, 5x, f15.2 )
200 format ( i3, ",", i4, "," a20, ",", 10(f15.2, ",") )
    
close ( 210 )

return
end

! ---------------------------------------------------------------------
! Escribe a archivos CSV los resultados de generacion, estado de las  *
! unidades, asi como las unidades marginales.                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre del 2015                                                    *
! ---------------------------------------------------------------------
Subroutine EscribeCSVGen ( sistema )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: MargNodal

Implicit none

INTEGER   i, sistema, u, uunidad, s, modo, falso, &
          NoDi, nodo, componente, componente_1
character*1 ssistema

integer UniGENCSV, UniMODCSV, UniUNMARG

integer   ierror, UNIMAR ( maxurc+maxurd+maxuh+maxure, maxint )
real*8    tol, bloque, marginal

ierror = 0
UNIMAR = 0
tol = 1.0e-5

! Se concatena el numero de subsistema al nombre de los archivos de debugger
Write( ssistema, '(I1)' )  sistema

! Resultados CSV de generacion de unidades
uunidad = 130 + sistema
UniGENCSV = uunidad
OPEN ( UNIT = UniGENCSV, FILE = trim(rut_dat_1)//'RESGEN_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 51, FILE = rut_res//'RESGEN.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

! Resultados CSV de modos de generacion de unidades
uunidad = 140 + sistema
UniMODCSV = uunidad
OPEN ( UNIT = UniMODCSV, FILE = trim(rut_dat_1)//'RESMODO_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

! Resultados CSV de unidades marginales
uunidad = 150 + sistema
UniUNMARG = uunidad
OPEN ( UNIT = UniUNMARG, FILE = trim(rut_dat_1)//'UNIMARG_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

! para todas las unidades de rango continuo
do u = 1, NumUniRC
!   para todos los intervalos de planeacion
    write ( UniGENCSV, 600 ) nombunirc(u), 0, ( GENUNRC(u,i)*Base, i=1,NTINTR )
    write ( 51, 600 ) nombunirc(u), 0, ( GENUNRC(u,i)*Base, i=1,NTINTR )
    write ( UniMODCSV, 700 ) nombunirc(u), 0, ( RESMODO(u,i), i=1,NTINTR )
!   para todos los intervalos
    do i = 1, NTINTR
!       si la unidad no esta apagada
        if ( GENUNRC(u,i) .gt. 0.0 ) then
!           si la unidad no esta en sincronizacion
            if ( RESMODO(u,i) .ne. 11 ) then
!               si la unidad es cordinable
                if ( COORDURC ( u, i ) .eq. 1 ) then
!                   si la unidad no esta en limites minimo o maximo
                    if ( (abs(GENUNRC(u,i)-PotMinGRC(u,i)).gt.tol) .and. (abs(GENUNRC(u,i)-PotMaxGRC(u,i)).gt.tol) ) then
                        falso = 1
                        bloque = 0.0
!                       si la unidad esta en limites de ofertas
                        do s = 1, NumBloVRC( u, i )*0
                            bloque = bloque + OferVenEnerRC (u,s,i)
                            if ( abs( (PotMinGRC(u,i)+bloque) - GENUNRC(u,i) ) .lt. tol ) then
                                falso = 0
                            endif
                        enddo
!                        if ( falso .eq. 1 ) then
!                            UNIMAR ( u, i ) = 1
!                        endif
                        if ( falso .eq. 1 ) then
!                           para todos los nodos distribuidos
                            do NoDi = 1, NoNodDisRC ( u, i )
                                nodo = Tempnodorc ( u, NoDi, i )
                                marginal = MargNodal( nodo , i )/Base
!                               para todos los segmentos de curva de ofertas de venta
                                do s = 1, NumBloVRC( u, i )
                                    if ( abs((PreVenEnerRC(u,s,i)/Base - marginal)/marginal) .lt. 5.0e-3 ) then
!                                       posible unidad marginal
                                        UNIMAR ( u, i ) = 1
                                    endif
                                enddo
                            enddo
                        endif
                    endif
                endif
!               si la unidad agoto su rango operativo
!                if ( abs(GENUNRC(u,i)+xMILP(IRR10RC+u+(i-1)*NumUniRC-1)+xMILP(IRRSRC+u+(i-1)*NumUniRC-1)-PotMaxGRC(u,i)) .lt. tol ) then
!                    UNIMAR ( u, i ) = 0
!                endif
!               si la unidad esta vendiendo reserva de regulacion y no toca su oferta
!                if ( xMILP(IRRERC + u + (i-1)*NumUniRC - 1) .gt. 0.0 .and. &
!                     xMILP(IRRERC + u + (i-1)*NumUniRC - 1) .lt. OferResRegRC ( u, i ) ) then
!                     falso = 0
!               si la unidad esta vendiendo reserva rodante de 10 y no toca su oferta
!                elseif ( xMILP(IRR10RC + u + (i-1)*NumUniRC - 1) .gt. 0.0 .and. &
!                         xMILP(IRR10RC + u + (i-1)*NumUniRC - 1) .lt. OferResR10RC ( u, i ) ) then
!                     falso = 0
!               si la unidad esta vendiendo reserva rodante suplementaria y no toca su oferta
!                elseif ( xMILP(IRRSRC + u + (i-1)*NumUniRC - 1) .gt. 0.0 .and. &
!                         xMILP(IRRSRC + u + (i-1)*NumUniRC - 1) .lt. OferResRxRC ( u, i ) ) then
!                     falso = 0
!                endif
!                if ( falso .eq. 0 ) then
!                endif
            endif
        endif
    enddo
enddo

! para todas las unidades de rango discontinuo
do u = 1, NumUniRD
!   para todos los intervalos de planeacion
    write ( UniGENCSV, 600 ) nombunird(u), 1, ( GENUNRD(u,i)*Base, i=1,NTINTR )
    write ( 51, 600 ) nombunird(u), 1, ( GENUNRD(u,i)*Base, i=1,NTINTR )
    write ( UniMODCSV, 700 ) nombunird(u), 1, ( RESMODO(u+NumUniRC,i), i=1,NTINTR )
!   para todos los intervalos
    do i = 1, NTINTR
        modo = RESMODO(u+NumUniRC,i)
!       si la unidad no esta apagada
        if ( GENUNRD(u,i) .gt. 0.0 ) then
!           si la unidad no esta en sincronizacion
            if ( modo .ne. 11 ) then
!               si la unidad es cordinable
                if ( CoordURD ( u, modo, i ) .eq. 1 ) then
!                   si la unidad no esta en limites minimo o maximo
                    if ( (abs(GENUNRD(u,i)-PotMinGRD(u,modo,i)).gt.tol) .and. (abs(GENUNRD(u,i)-PotMaxGRD(u,modo,i)).gt.tol) ) then
                        falso = 1
                        bloque = 0.0
!                       si la unidad esta en limites de ofertas
                        do s = 1, NumBloVRD( u, modo, i )*0
                            bloque = bloque + OferVenEnerRD ( u, modo, s, i )
                            if ( abs( (PotMinGRD(u,modo,i)+bloque) - GENUNRD(u,i) ) .lt. tol ) then
                                falso = 0
                            endif
                        enddo
!                        if ( falso .eq. 1 ) then
!                            UNIMAR ( u, i ) = 1
!                        endif
                        if ( falso .eq. 1 ) then
                            do componente = 1, NumCompXModo ( u, modo )
!                               para todas la componentes de la unida de rango discontinuo
                                do componente_1 = 0, NumCompRD ( u ) - 1
                                    if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                                        nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
                                        marginal = MargNodal( nodo , i )/Base
!                                       para todos los segmentos de curva de ofertas de venta
                                        do s = 1, NumBloVRD( u, modo, i )
                                            if ( abs((PreVenEnerRD(u,modo,s,i)/Base - marginal)/marginal) .lt. 5.0e-3 ) then
!                                               posible unidad marginal
                                                UNIMAR ( u+NumUniRC, i ) = 1
                                            endif
                                        enddo
                                    endif
                                enddo
                            enddo
                        endif
                    endif
                endif
            endif
        endif
    enddo
enddo

! para todas las unidades hidro
do u = 1, NumUniHid
!   para todos los intervalos de planeacion
    write ( UniGENCSV, 600 ) nombunih(u), 2, ( GENUNH(u,i)*Base, i=1,NTINTR )
    write ( 51, 600 ) nombunih(u), 2, ( GENUNH(u,i)*Base, i=1,NTINTR )
    write ( UniMODCSV, 700 ) nombunih(u), 2, ( RESMODO(u+NumUniRC+NumUniRD,i), i=1,NTINTR )
!   para todos los intervalos
    do i = 1, NTINTR
!       si la unidad no esta apagada
        if ( GENUNH(u,i) .gt. 0.0 ) then
!           si la unidad es cordinable
            if ( COORDUH ( u, i ) .eq. 1 ) then
!               si la unidad no esta en limites minimo o maximo
                if ( (abs(GENUNH(u,i)-PotMinUniH(u,i)).gt.tol) .and. (abs(GENUNH(u,i)-PotMaxUniH(u,i)).gt.tol) ) then
                    nodo = nodoh ( u, i )
                    marginal = MargNodal( nodo , i )/Base
                    if ( abs((CostoOporUH(u,i)/Base - marginal)/marginal) .lt. 5.0e-3 ) then
!                       posible unidad marginal
                        UNIMAR ( u+NumUniRC+NumUniRD, i ) = 1
                    endif
                endif
            endif
        endif
!               si la unidad esta vendiendo reserva
!                if ( xMILP(IRR10H + u + (i-1)*NumUniHid - 1).gt.0.0 .or. xMILP(IRRSH + u + (i-1)*NumUniHid - 1).gt.0.0 &
!                     .or. xMILP(IRREH + u + (i-1)*NumUniHid - 1).gt.0.0 ) then
!                    UNIMAR ( u+NumUniRC+NumUniRD, i ) = 0
!                endif
!               si la unidad esta vendiendo reserva de regulacion
!                if ( xMILP(IRREH + u + (i-1)*NumUniHid - 1) .gt. 0.0 ) then
!                    UNIMAR ( u+NumUniRC+NumUniRD, i ) = 0
!                else
!                   si la unidad agoto su rango operativo
!                    if ( abs(GENUNH(u,i)+xMILP(IRR10H+u+(i-1)*NumUniHid-1)+xMILP(IRRSH+u+(i-1)*NumUniHid-1)-PotMaxUniH(u,i)) .lt. tol ) then
!                        UNIMAR ( u+NumUniRC+NumUniRD, i ) = 0
!                    endif
!                endif
!            endif
!        else
!            UNIMAR ( u+NumUniRC+NumUniRD, i ) = 0
!        endif
    enddo
enddo

! para todas las unidades renovables intermitentes
do u = 1, NumUniRE
!   para todos los intervalos de planeacion
    write ( UniGENCSV, 600 ) nombunire(u), 3, ( GENUNRE(u,i)*Base, i=1,NTINTR )
    write ( 51, 600 ) nombunire(u), 3, ( GENUNRE(u,i)*Base, i=1,NTINTR )
    write ( UniMODCSV, 700 ) nombunire(u), 3, ( RESMODO(u+NumUniRC+NumUniRD+NumUniHid,i), i=1,NTINTR )
!   para todos los intervalos
    do i = 1, NTINTR
!       si la unidad no esta apagada
        if ( GENUNRE(u,i) .gt. 0.0 ) then
!           si la unidad no esta en limites minimo o maximo
            if ( (abs(GENUNRE(u,i)-PotMinGRE(u,i)).gt.tol) .and. (abs(GENUNRE(u,i)-PotMaxGRE(u,i)).gt.tol) ) then
!               posible unidad marginal
                UNIMAR ( u+NumUniRC+NumUniRD+NumUniHid, i ) = 1
            endif
        endif
    enddo
enddo

! para todas las unidades de rango continuo
do u = 1, NumUniRC
!   para todos los intervalos de planeacion
    write ( UniUNMARG, 700 ) nombunirc(u), 0, ( UNIMAR(u,i), i=1,NTINTR )
enddo

! para todas las unidades de rango discontinuo
do u = 1, NumUniRD
!   para todos los intervalos de planeacion
    write ( UniUNMARG, 700 ) nombunird(u), 1, ( UNIMAR(u+NumUniRC,i), i=1,NTINTR )
enddo

! para todas las unidades hidro
do u = 1, NumUniHid
!   para todos los intervalos de planeacion
    write ( UniUNMARG, 700 ) nombunih(u), 2, ( UNIMAR(u+NumUniRC+NumUniRD,i), i=1,NTINTR )
enddo
    
! para todas las unidades renovables intermitentes
do u = 1, NumUniRE
!   para todos los intervalos de planeacion
    write ( UniUNMARG, 700 ) nombunire(u), 3, ( UNIMAR(u+NumUniRC+NumUniRD+NumUniHid,i), i=1,NTINTR )
enddo
    
close ( UniGENCSV )
close ( UniMODCSV )
close ( UniUNMARG )
close ( 51 )

! Imprime resultados por area
call ImpResultadosArea ( sistema )

! Imprime resultados de generacion y reserva de todas las unidades
call EscribeResumenUnidades ( sistema )

600 format ( A15, ',', I2, ',', 169(f10.3, ',')  )
700 format ( A15, ',', I2, ',', 169(I2, ',')  )

return
end
    
! ---------------------------------------------------------------------
! Escribe resultados por area                                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2016                                                      *
! ---------------------------------------------------------------------
Subroutine ImpResultadosArea ( sistema )

Use ParAUHE, only: NumUniRC, NumUniRD, NumUNiHid, NumUniRE, NumUniNPR, NumNodInt, NumOferDem,  &
                  GENUNRC, GENUNRD, GENUNH, GENUNRE, PotNPR, PotNodInt, DemFija, &
                  nodorc, nodocompurd, nodocar, nodoh, nodounre, nodonpr, NodoInt, NumBloDem, &
                  CompXModo, ListCompURD, ApunCompURD, GenCompXModo, NumCompRD, NumModRD, &
                  NumCompXModo, Tempnodorc, facdistgen, Tempnodocar, facdistcar, NoNodDisRC, &
                  NoNodDisCar, NoNodDisNPR, Tempnodonpr, facdistnpr, NTINTR, maxnod, maxint, &
                  NumNodos, nodo_area, RUT_RES, rut_dat_1, base, numsis_act
                  
use ProblemaAUHE, only: IDBC, INDDE, IDF, IEXC, xMILP, IADARD, INIURDI, IARD, IPERD

Use ParGloRed, only: inaumnodsis, numnodsis, inrnod, maxare, numarea, nomarea, perpacnod, &
                     NumRegPre, maxregpre, regnod, MargRegional, nomregpre

implicit none

integer d, k, modo,  intervalo, nodo, u, NoDi, s, n, componente_1, componente, sistema, area, BuscaArea, ierror, ireg

real*8 genterarea(maxare,maxint), genhidarea(maxare,maxint),  genrearea(maxare,maxint),  gennoproarea (maxare,maxint), demarea(maxare,maxint), excearea (maxare,maxint), &
       perarea (maxare,maxint), corarea (maxare,maxint),  potintarea (maxare,maxint)


real*8 genterreg(maxregpre,maxint), genhidreg(maxregpre,maxint),  genrereg(maxregpre,maxint),  gennoproreg (maxregpre,maxint), demreg(maxregpre,maxint), excereg (maxregpre,maxint), &
       perreg (maxregpre,maxint), correg (maxregpre,maxint),  potintreg (maxregpre,maxint)

real*8 gentot, pertot, perapro


character*1 ssistema
write ( ssistema, 300 ) sistema

OPEN (UNIT = 801, FILE = RUT_RES//'r_Areas.csv', &
      IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)
OPEN (UNIT = 802, FILE = RUT_RES//'r_Regiones.csv', &
      IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)
OPEN ( UNIT = 1, FILE = trim(rut_dat_1)//'RESAREAS_'//trim(ssistema)//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 400 )

if ( ierror .ne. 0 ) return

write ( 801, * ) 'Inter , Area, GenTer, GenHid, GenRE, GenNP, GenTot, Demanda, Corte, Excedente, PotInt, Perdidas'
write ( 802, * ) 'Inter , Region, GenTer, GenHid, GenRE, GenNP, GenTot, Demanda, Corte, Excedente, PotInt, Perdidas, PrecioMarginal'
! Inicializa matriz de arreglos por area
genterarea = 0.0
genhidarea = 0.0
genrearea = 0.0
demarea = 0.0
excearea = 0.0
perarea = 0.0
corarea = 0.0
gennoproarea = 0.0
potintarea = 0.0

! Inicializa matriz de arreglos por area
genterreg = 0.0
genhidreg = 0.0
genrereg = 0.0
demreg = 0.0
excereg = 0.0
perreg = 0.0
correg = 0.0
gennoproreg = 0.0
potintreg = 0.0

! Calcula e imprime resultados globales del area
do intervalo = 1, NTINTR

    ! Incluye generacion de rango continuo
    do u = 1 , NumUniRC  ! JLC
    !   para todos los nodos distribuidos
        do NoDi = 1, NoNodDisRC ( u, intervalo )
            nodo = Tempnodorc ( u, NoDi, intervalo )
            area = BuscaArea ( nodo_area(nodo) )
            ireg = regnod(nodo)
            genterarea(area,intervalo) = genterarea(area,intervalo) + facdistgen ( u, NoDi, intervalo )*GENUNRC ( u, intervalo )
            genterreg(ireg,intervalo) = genterreg(ireg,intervalo) + facdistgen ( u, NoDi, intervalo )*GENUNRC ( u, intervalo )
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
                            area = BuscaArea ( nodo_area(nodo) )
                            ireg = regnod(nodo)
                            genterarea(area,intervalo) = genterarea(area,intervalo) + GENUNRD ( u, intervalo )*GenCompXModo  ( u, modo, componente )
                            genterreg(ireg,intervalo) = genterreg(ireg,intervalo) + GENUNRD ( u, intervalo )*GenCompXModo  ( u, modo, componente )
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
      area = BuscaArea ( nodo_area(nodo) )
      ireg = regnod(nodo)
      genhidarea(area,intervalo) = genhidarea(area,intervalo) +GENUNH ( u, intervalo )
      genhidreg(ireg,intervalo) = genhidreg(ireg,intervalo) +GENUNH ( u, intervalo )
    enddo

    ! Incluye generacion renovables
    do u = 1 , NumUniRE
        nodo = nodounre ( u, intervalo )
        area = BuscaArea ( nodo_area(nodo) )
        ireg = regnod(nodo)
        genrearea(area,intervalo) = genrearea(area,intervalo) +  GENUNRE ( u, intervalo )
        genrereg(ireg,intervalo) = genrereg(ireg,intervalo) +  GENUNRE ( u, intervalo )
    enddo
        
    ! Incluye nivel de demanda y corte de carga
    do d = 1 , NumOferDem  
    !   para todos los nodos distribuidos
        do NoDi = 1, NoNodDisCar ( d, intervalo )
            nodo = Tempnodocar ( d, NoDi, intervalo )
            area = BuscaArea ( nodo_area(nodo) )
            ireg = regnod(nodo)
    !       para todos los segmentos de curva de ofertas de compra
            do s = 1, NumBloDem( d, intervalo )
	           demarea(area,intervalo) = demarea(area,intervalo) + xMILP ( IDBC + s + INDDE ( d, intervalo ) - 1 )*facdistcar ( d, NoDi, intervalo )
	           demreg(ireg,intervalo) = demreg(ireg,intervalo) + xMILP ( IDBC + s + INDDE ( d, intervalo ) - 1 )*facdistcar ( d, NoDi, intervalo )
            enddo
    !       corte de carga
	        corarea(area,intervalo) = corarea(area,intervalo) + xMILP ( IDF + d + (intervalo-1)*NumOferDem - 1 )*facdistcar ( d, NoDi, intervalo )
            correg(ireg,intervalo) = correg(ireg,intervalo) + xMILP ( IDF + d + (intervalo-1)*NumOferDem - 1 )*facdistcar ( d, NoDi, intervalo )
        enddo
    enddo
        
    ! Incluye excedentes
    do n = 1, NumNodSis ( sistema )
       nodo = inaumnodsis ( n )
       ireg = regnod(nodo)
       area = BuscaArea ( nodo_area(nodo) )
       excearea(area,intervalo) = excearea(area,intervalo) + xMILP ( IEXC + n + (intervalo-1)*NumNodSis ( sistema ) - 1 )
       excereg(ireg,intervalo) = excereg(ireg,intervalo) + xMILP ( IEXC + n + (intervalo-1)*NumNodSis ( sistema ) - 1 )
    enddo

    !   Agrega termino asociado a generaciones no programables
    do k = 1, NumUniNPR 
        do NoDi = 1, NoNodDisNPR ( k, intervalo )
            nodo = Tempnodonpr ( k, NoDi, intervalo )
            ireg = regnod(nodo)
            area = BuscaArea ( nodo_area(nodo) )
            if ( inrnod(nodo) .ne. 0 ) then
	            gennoproarea(area,intervalo) = gennoproarea(area,intervalo) + PotNPR ( k, intervalo )*facdistnpr ( k, NoDi, intervalo )
	            gennoproreg(ireg,intervalo) = gennoproreg(ireg,intervalo) + PotNPR ( k, intervalo )*facdistnpr ( k, NoDi, intervalo )
            endif
        enddo
    enddo

    ! Agrega termino asociado a ofertas de demanda ( componente fija )
    do k = 1, NumOferDem 
    !   para todos los nodos distribuidos
        do NoDi = 1, NoNodDisCar ( k, intervalo )
            nodo = Tempnodocar ( k, NoDi, intervalo )
            area = BuscaArea ( nodo_area(nodo) )
            ireg = regnod(nodo)
!            if ( inrnod(nodo) .ne. 0 ) then
                demarea(area,intervalo) = demarea(area,intervalo) +  DemFija ( k, intervalo )*facdistcar ( k, NoDi, intervalo )
                demreg(ireg,intervalo) = demreg(ireg,intervalo) +  DemFija ( k, intervalo )*facdistcar ( k, NoDi, intervalo )
!            endif
        enddo
     enddo

    !   Agrega termino asociado a potencias de intercambio 
    do k = 1, NumNodInt
       nodo = NodoInt(k, intervalo)
       area = BuscaArea ( nodo_area(nodo) )
       ireg = regnod(nodo)
!       if ( inrnod(nodo) .ne. 0 ) then
           potintarea(area,intervalo) = potintarea(area,intervalo) + PotNodInt ( k, intervalo )
           potintreg(ireg,intervalo) = potintreg(ireg,intervalo) + PotNodInt ( k, intervalo )
!       endif
    enddo

    ! Agrega termino asociado a perdidas del area
    pertot = 0
    do nodo = 1, NumNodos
       area = BuscaArea ( nodo_area(nodo) )
       ireg = regnod(nodo)
       perarea(area,intervalo) =  perarea(area,intervalo) + perpacnod(nodo,intervalo)
       perreg(ireg,intervalo) =  perreg(ireg,intervalo) + perpacnod(nodo,intervalo)
       pertot = pertot + perpacnod(nodo,intervalo)
    enddo
    perapro = xMILP ( IPERD + intervalo + (sistema-1)*numsis_act - 1 )
    
!   Escribe resultados para todas las area    
    do area = 1, numarea
        gentot = base*(genterarea(area,intervalo) + genhidarea(area,intervalo) + genrearea(area,intervalo) + gennoproarea (area,intervalo) )
        write ( 801, 100, iostat = ierror) intervalo, nomarea(area),  genterarea(area,intervalo)*base, genhidarea(area,intervalo)*base,  genrearea(area,intervalo)*base, &
                          gennoproarea (area,intervalo)*base, gentot, demarea(area,intervalo)*base,  corarea (area,intervalo)*base, excearea (area,intervalo)*base, &
                          potintarea (area,intervalo)*base, (perarea (area,intervalo)*perapro/pertot)*base
        
        write ( 1, 100, iostat = ierror) intervalo, nomarea(area),  genterarea(area,intervalo)*base, genhidarea(area,intervalo)*base,  genrearea(area,intervalo)*base, &
                          gennoproarea (area,intervalo)*base, gentot, demarea(area,intervalo)*base,  corarea (area,intervalo)*base, excearea (area,intervalo)*base, &
                          potintarea (area,intervalo)*base, (perarea (area,intervalo)*perapro/pertot)*base
        
    enddo

    !   Escribe resultados para todas las regiones    
    do ireg = 1, numregpre
        gentot = base*(genterreg(ireg,intervalo) + genhidreg(ireg,intervalo) + genrereg(ireg,intervalo) + gennoproreg (ireg,intervalo) )
        write ( 802, 200, iostat = ierror) intervalo, nomregpre(ireg),  genterreg(ireg,intervalo)*base, genhidreg(ireg,intervalo)*base,  genrereg(ireg,intervalo)*base, &
                          gennoproreg (ireg,intervalo)*base, gentot, demreg(ireg,intervalo)*base,  correg (ireg,intervalo)*base, excereg (ireg,intervalo)*base, &
                          potintreg (ireg,intervalo)*base, perreg (ireg,intervalo)*base, MargRegional(ireg,intervalo)/Base
    enddo

enddo

close ( 801 )
close (802 )
close (1)

100 format ( i3, ',' a12, ',', 10(F15.3,',') ) 
200 format ( i3, ',' a12, ',', 11(F15.3,',') ) 
300 format ( i1 )    
return
end
    
! ---------------------------------------------------------------------
! Escribe a archivos CSV los resultados de generacion, estado de las  *
! unidades, generacion y reserva                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2019                                                  *
! ---------------------------------------------------------------------
Subroutine EscribeResumenUnidades ( sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

INTEGER   i, sistema, u, m, modo, gunidad, disp, asign, coord, mod, ro, s, smarg, kv

real*8 ResReg, ResRod10, ResRodSup, ResNoRod10, ResNoRodSup, uso, usore

integer   ierror

real*8    genini, subir, bajar, sube, delta, coinc

character*2 tip

ierror = 0


! Resultados CSV de generaciones nodales por intervalo
gunidad = 155 + sistema
OPEN ( UNIT = gunidad, FILE = trim(RUT_RES)//'RESUMEN_UNIDADES.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
write ( gunidad, * ) 'UNIDAD, TIPO, INTERVALO, CONDICION INICIAL, POTENCIA, LIM_INF, LIM_SUP, REG, RR10, RRS, RNR10, RNRS, '// &
                     'DISPONIBILIDAD, ASIGNABILIDAD, COORDINABILIDAD, MODO,'// &
                     'LIM_REG_INF, LIM_REG_SUP, OFE_REG, CAP_REG, ALERTA_REG, COND_SINC,COSINC,SUBIR,BAJAR,DELTA,USO_ROD,USO_REG'

! para todas las unidades de rango continuo
kv = IGABRC
do u = 1, NumUniRC
    tip = "RC"
    genini = GenCIURC ( u )
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
!      Determina costo incremental        
       smarg = 0
       if ( TipoOferta .eq. 1 ) then
    !      	para todos los segmentos de curva de ofertas de venta
       	    do s = 1, NumBloVRC( u, i )
                if (  xMILP( kv ) .lt. OferVenEnerRC ( u, s, i ) .and. smarg .eq. 0 ) then
                    smarg = s
                endif
          	    kv = kv + 1
            enddo
            if ( smarg .eq. 0 ) smarg = NumBloVRC( u, i )
    !       si son ofertas de costo en vacio
       else
           
       	    if ( NumBloVRC( u, i ) .gt. 0 ) then
           	    kv = kv + 1
       	    endif
    !      	para todos los segmentos de curva de ofertas de venta, excepto el primero
       	    do s = 2, NumBloVRC( u, i )
                if (  xMILP( kv ) .lt. OferVenEnerRC ( u, s, i ) .and. smarg .eq. 0 ) then
                    smarg = s
                endif
           	    kv = kv + 1
            enddo
            if ( smarg .eq. 0 ) smarg = NumBloVRC( u, i )
        endif
        if ( smarg .eq. 0 ) then
           coinc = 0.0
        else
           coinc = PreVenEnerRC(u,smarg,i)/BASE 
        endif
        
        ResReg = xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )*Base
        ResRod10 = xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 )*Base
        ResRodSup = xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )*Base
        ResNoRod10 = xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )*Base
        ResNoRodSup = xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )*Base
        genini = GenCIURC ( u )
        subir = 0.0 
        bajar = 0.0
        if ( DispoURC ( u, i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 .and. GENUNRC ( u, i ) .gt. 0.0 ) then
            if ( NoRaOpRC ( u ) .gt. 0 ) then
                do ro = 1, NoRaOpRC ( u )
                    if ( GENUNRC ( u, i ) .ge. RaOpInfRC ( u, ro, i ) .and. GENUNRC ( u, i ) .le.  RaOpSupRC( u, ro, i ) ) then
                         bajar = ( GENUNRC ( u, i ) - RaOpInfRC ( u, ro, i )  )*Base - ResReg
                         subir = ( RaOpSupRC ( u, ro, i ) - GENUNRC ( u, i ) )*Base - ResRod10 -  ResRodSup - ResReg
                         exit
                    endif
                enddo
            else
                subir = ( PotMaxGRC ( u, i ) - GENUNRC ( u, i ) )*Base - ResRod10 -  ResRodSup - ResReg
                if ( GENUNRC ( u, i ) .gt. PotMinGRC ( u, i ) ) then
                    bajar = ( GENUNRC ( u, i ) - PotMinGRC ( u, i ) )*Base  
                endif
            endif
            sube = subir
            if ( ResReg .gt. 0 ) then
                if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
                      do ro = 1, NoRaOpRC ( u )
                          if ( GENUNRC ( u, i ) .ge. RaRegInfRC ( u, ro, i ) .and. GENUNRC ( u, i ) .le. RaRegSupRC  ( u, ro, i ) ) then
                               bajar = ( GENUNRC ( u, i ) - RaRegInfRC ( u, ro, i )  )*Base - ResReg  
                               sube = ( RaRegSupRC ( u, ro, i ) - GENUNRC ( u, i ) )*Base - ResReg
                               exit
                          endif
                      enddo
                else
                   if ( GENUNRC ( u, i ) .gt. PotMinRRC ( u, i ) ) then
                      bajar = ( GENUNRC ( u, i ) - PotMinRRC ( u, i )  )*Base - ResReg  
                   endif
                   sube = ( PotMaxRRC ( u, i ) - GENUNRC ( u, i ) )*Base - ResReg
                endif
                subir = min ( subir, sube )
            endif
        endif
        if ( i .gt. 1 ) genini = GENUNRC(u,i-1)
        delta = ( GENUNRC ( u, i ) -  genini )*Base
        if ( TipoEjecu .le. 1 ) then
            uso = xMILP ( IREUSO10 + u + (i-1)*NumUniRC - 1 )*SiRelRod*Base
            usore = xMILP ( IREUSORE + u + (i-1)*NumUniRC - 1 )*Base
        else
            uso = 0.0
            usore = 0.0
        endif
        write ( gunidad, 800, iostat = ierror ) nombunirc(u), tip, i, genini*BASE, GENUNRC(u,i)*BASE, PotMinGRC ( u, i )*BASE, PotMaxGRC ( u, i )*BASE, &
                                ResReg, ResRod10, ResRodSup, ResNoRod10, ResNoRodSup, DispoURC ( u , i ), AsignURC ( u , i ), &
                                CoordURC ( u , i ), resmodo ( u, i ), PotMinRRC (u,i )*BASE, &
                                PotMaxRRC(u,i)*BASE, OferResRegRC ( u, i )*BASE, CalOferResRegRC ( u, i )*BASE, & 
                                alertaregRC ( u, i ), CompSincRC(u,i), coinc, subir, bajar, delta, uso, usore
    enddo
enddo

! para todas las unidades de rango discontinuo
do u = 1, NumUniRD
    tip = "RD"
    do m = 1, NumModRD ( u )
!       para el modo en condiciones iniciales
        if ( EstadoCIURD ( u, modo ) .gt. 0 ) then
            modo = m
        endif               
    enddo
    genini = GenCIURD ( u, modo )
    COINC = 0.0
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        do m = 1, NumModRD ( u )
!           para el modo asignado
            if ( xMILP(IARD+INIURDI(u,i)+modo-1) .gt. 0  ) then
                modo = m
            endif               
        enddo
        ResReg = xMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 )*Base
        ResRod10 = xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 )*Base
        ResRodSup = xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 )*Base
        ResNoRod10 = xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )*Base
        ResNoRodSup = xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )*Base
        subir = 0.0 
        bajar = 0.0
        if (  DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u , modo, i ) .eq. 1 .and. GENUNRD ( u, i ) .gt. 0.0 ) then
            subir = ( PotMaxGRD ( u, modo, i ) - GENUNRD ( u, i ) )*Base - ResRod10 -  ResRodSup - ResReg 
            bajar = (  GENUNRD ( u, i ) - PotMinGRD ( u, modo, i ) )*Base - ResReg 
        endif
        if ( i .gt. 1 ) genini = GENUNRD(u,i-1)
        delta = (GENUNRD ( u, i ) - genini)*Base
        uso = 0.0
        usore = 0.0
        write ( gunidad, 800 ) nombunird(u), tip, i, genini*BASE, GENUNRD(u,i)*BASE, PotMinGRD ( u, modo, i )*BASE, PotMaxGRD( u, modo, i )*BASE, &
                               ResReg, ResRod10, ResRodSup, ResNoRod10, ResNoRodSup, DispoURD ( u , modo, i ), AsignURD ( u , modo, i ), &
                               CoordURD ( u , modo, i ), resmodo ( u + NumUniRC , i ), PotMinGRD (u,modo, i )*BASE, &
                               PotMaxRRD(u,modo,i)*BASE, OferResRegRD ( u, modo, i )*BASE, CalOferResRegRD ( u, modo, i )*BASE, & 
                               alertaregRD ( u, i ), CompSincRC(u,i), coinc, subir, bajar, delta, uso, usore
    enddo

enddo

! para todas las unidades hidro
do u = 1, NumUniHid
    tip = "HI"    
!   para todos los intervalos de planeacion
    genini = 0
    if ( EstadoCIUH ( u ) .eq. 1 ) genini = GENUNH(u,1)
    do i = 1, NTINTR
        ResReg = xMILP ( IRREH + u + (i-1)*NumUniHid - 1 )*Base
        ResRod10 = xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 )*Base
        ResRodSup = xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 )*Base
        ResNoRod10 = xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )*Base
        ResNoRodSup = xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )*Base
        if ( i .gt. 1 ) genini = GENUNH(u,i-1)
        delta = (GENUNH(u,i) - genini)*Base
        coinc = CostoOporUH(u,i)/BASE
        subir = 0.0 
        bajar = 0.0
        if (  DispoUH ( u, i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 ) then
            if ( NoRaOpH ( u ) .gt. 0 ) then
                do ro = 1, NoRaOpH ( u )
                    if ( GENUNH ( u, i ) .ge. RaOpInfH ( u, ro, i ) .and. GENUNH ( u, i ) .le.  RaOpSupH ( u, ro, i ) ) then
                         bajar = ( GENUNH ( u, i ) - RaOpInfH ( u, ro, i )  )*Base - ResReg
                         subir = ( RaOpSupH ( u, ro, i ) - GENUNH ( u, i ) )*Base - ResRodSup -  ResRodSup - ResReg
                         exit
                    endif
                enddo
            else
                subir = ( PotMaxUniH ( u, i ) - GENUNH ( u, i ) )*Base - ResRodSup -  ResRodSup - ResReg
                if ( GENUNH ( u, i ) .gt. PotMinUniH ( u, i ) ) then
                   bajar = ( GENUNH ( u, i ) - PotMinUniH ( u, i ) )*Base  
                endif
            endif
            sube = subir
            if ( ResReg .gt. 0 ) then
                if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
                      do ro = 1, NoRaOpH ( u )
                          if ( GENUNH ( u, i ) .ge. RaRegInfH ( u, ro, i ) .and. GENUNH ( u, i ) .le. RaRegSupH  ( u, ro, i ) ) then
                               bajar = ( GENUNH ( u, i ) - RaRegInfH ( u, ro, i )  )*Base - ResReg  
                               sube = ( RaRegSupH ( u, ro, i ) - GENUNH ( u, i ) )*Base - ResReg
                               exit
                          endif
                      enddo
                else
                   if ( GENUNH ( u, i ) .gt. PotMinRUniH ( u, i ) ) then
                      bajar = ( GENUNH ( u, i ) - PotMinRUniH ( u, i )  )*Base - ResReg  
                   endif
                   sube = ( PotMaxRUniH ( u, i ) - GENUNH ( u, i ) )*Base - ResReg
                endif
                subir = min ( subir, sube )
            endif
        endif
        uso = 0.0
        if ( TipoEjecu .le. 1 ) then
            usore = xMILP ( IREUSOREH + u + (i-1)*NumUniHid - 1 )*Base
        else
            usore = 0.0
        endif
        write ( gunidad, 800 ) nombunih(u), tip, i, genini*BASE, GENUNH(u,i)*BASE, PotMinUniH ( u, i )*BASE, PotMaxUniH( u, i )*BASE, &
                               ResReg, ResRod10, ResRodSup, ResNoRod10, ResNoRodSup,  DispoUH ( u , i ), AsignUH ( u , i ), CoordUH ( u , i ) , &
                               resmodo ( u + NumUniRC + NumUniRD , i ), PotMinRUniH (u,i )*BASE, &
                               PotMaxRUniH(u,i)*BASE, OferResRegH ( u, i )*BASE,  CalOferResRegH ( u, i )*BASE, & 
                               alertaregUH ( u, i ), CompSincH(u,i), coinc, subir, bajar, delta, uso, usore
    enddo
enddo

! para todas las unidades renovables intermitentes
do u = 1, NumUniRE
    tip = "RE"
    ResReg = 0.0
    ResRod10 = 0.0
    ResRodSup = 0.0
    ResNoRod10 = 0.0
    ResNoRodSup = 0.0
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        genini = PotMaxGRE ( u, i )
        coord = DispoURE ( u , i )
        smarg = 0
        do s = 1, NumBloVRE( u, i )
           if (  xMILP( kv ) .lt. OferVenEnerRE ( u, s, i ) .and. smarg .eq. 0 ) then
                smarg = s
           endif
           kv = kv + 1
        enddo
        if ( smarg .eq. 0 ) smarg = NumBloVRE( u, i )
        if ( smarg .eq. 0 ) then
            coinc = 0.0
        else
            coinc = PreVenEnerRE(u,smarg,i)/BASE
        endif
        if ( DispoURE ( u, i ) .eq. 1 ) then
            subir = ( PotMaxGRE ( u, i ) - GENUNRE ( u, i ) )*Base - ResRod10 -  ResRodSup - ResReg 
            bajar = ( GENUNRE ( u, i ) - PotMinGRE (u,i ) )*Base - ResReg 
        else
            subir = 0.0 
            bajar = 0.0
        endif
        if ( i .gt. 1 ) genini = GENUNRE(u,i-1)
        delta = (GENUNRE ( u, i ) - genini)*Base
        uso = 0.0
        usore = 0.0
        write ( gunidad, 800 ) nombunire(u), tip, i, genini*BASE, GENUNRE(u,i)*BASE, PotMinGRE ( u, i )*BASE, PotMaxGRE ( u, i )*BASE, &
                               ResReg, ResRod10, ResRodSup, ResNoRod10, ResNoRodSup, DispoURE ( u , i ), AsignURE ( u , i ), coord, &
                               resmodo ( u + NumUniRC + NumUniRD + NumUniHid, i ),  0.0, 0.0, 0.0, 0.0, 0, 0, &
                                     coinc, subir, bajar, delta, uso, usore
    enddo
enddo


! para todas las unidades no programables
do u = 1 , NumUniNPR
    tip = "NP"
    ResReg = 0.0; ResRod10 = 0.0; ResRodSup = 0.0; ResNoRod10 = 0.0; ResNoRodSup = 0.0
    mod = 0
    asign = 0; coord = 0
    subir = 0.0; bajar = 0.0
    genini = PotNPR(u,1)
!   para todos los intervalos
    do i = 1, NTINTR
       genini = PotNPR(u,i)
       disp = 0
       if ( abs(genini) .gt. 0.0 ) mod = 1; disp = 1
       if ( i .gt. 1 ) genini = PotNPR(u,i-1)
       delta = (PotNPR(u,i) - genini)*Base
       uso = 0.0
       usore = 0.0
       write ( gunidad, 800 ) nombuninpr ( u ), tip, i, genini*BASE, PotNPR(u,i)*BASE, PotNPR ( u, i )*BASE, PotNPR ( u, i )*BASE, &
                               ResReg, ResRod10, ResRodSup, ResNoRod10, ResNoRodSup, disp, asign, coord, mod, 0.0, 0.0, 0.0, 0.0, 0, 0, 0.0, subir, bajar, delta, uso, usore
    enddo
enddo

close ( gunidad )

800 format ( A15, ',', A2, ',', I3, ',', 9(f10.3, ','), 3(I1, ','), i2, "," , 4(f10.3, ','), 2(i1, ",") , 6(f10.3, ','))
return
end    


subroutine ImpEnerT_new ( sistema )
! ---------------------------------------------------------------------
! Se imprime el resultado de energia en grupos de unidades termo      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2019                                                        *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, intervalo, u, unidad, grupo, iniciou, sistema
Real*8  TotEner, aux, dualinf, dualsup
Integer ierror, ibanbit

CHARACTER fecha_Ej*19
character*15 aaux3
character*20 aaux2
character*5 aaux1
character*1 ssistema

ibanbit = 1
ierror = 0

write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = 298, FILE = trim(rut_dat_1)//'GPOUTERRES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 300 )

write ( UniGLimT, * ) ''
!Para todas las restricciones
write ( UniGLimT, * ) 'Restriccion  Grupo    NomGrupo   HoraIni   HoraFin     LInfEner      LSupEner       Energia       DualInfer   DualSuper'

! para todos los grupos con limitacion de Enerustible
do grupo = 1, NResEner
    TotEner = 0.0
    i = grupo
    dualinf = 0.0
    dualsup = 0.0
    
!   para todos los intervalos del grupo
    do intervalo = HIResEner(grupo), HFResEner(grupo)
        iniciou = ApunURCxGrupo ( grupo )
!       para todas las unidades de rango continuo que estan en ese grupo
        do unidad = 1 , NumURCxGrupo ( grupo )
            u = UniRCxGrupo ( iniciou )
!           si es unidad disponible
            if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!               generacion de la unidad
                TotEner = TotEner + xMILP ( IGRC + u + (intervalo-1)*NumUniRC - 1 ) * Base
            endif
            iniciou = iniciou + 1
        enddo
!       para las unidades de rango discontinuo que estan en ese grupo
        iniciou = ApunURDxGrupo ( grupo )
        do unidad = 1 , NumURDxGrupo ( grupo )
            u = UniRDxGrupo ( iniciou )
!           genracion de la unidad
            TotEner = TotEner + GENUNRD ( u, intervalo ) * Base
            iniciou = iniciou + 1
        enddo
    enddo
!   si el grupo esta activo
    if ( ActResEner (grupo) .eq. 1 ) then
        dualinf = dualinfener (grupo) / Base
        dualsup = dualsupener (grupo) / Base
    endif
    
    TotEner = TotEner/1000.0
    
    write (298, 103)  i, NGpoResEner ( i ), NomGpoResEner ( i ), HIResEner ( i ), HFResEner ( i ), LInfResEner ( i )*Base/1000.0, LSupResEner ( i )*Base/1000.0, TotEner, -dualinf, -dualsup
    write (UniGLimT, 104)  i, NGpoResEner ( i ), NomGpoResEner ( i ), HIResEner ( i ), HFResEner ( i ), LInfResEner ( i )*Base/1000.0, LSupResEner ( i )*Base/1000.0, TotEner, -dualinf, -dualsup

!   Si existe infactibilidad en el grupo
    if ( xMILP ( IARGT + grupo - 1 ) .gt. 1.0d-3 .and. SiEnerTer .gt. 0 .and. ActResEner (grupo) .eq. 1 ) then
        SemBandera ( 5 ) = 1
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD EN ENERGIA TERMO'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux2, 5102 ) NomGpoResEner ( grupo )
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 RESTR DE Ener: '//aaux2
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux1, 5100 ) grupo 
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 RESTRICCION  : '//aaux1
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        aux = xMILP ( IARGT + grupo - 1 )/10.0
        write ( aaux3, 5101 )  aux
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD (GWh): '//aaux3
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif

enddo


103 format(2(i4,8x,','), a12, ',', 2(I3, 7x, ','), 5(f9.2, 5x,',') )
104 format(2(i4,8x), a12, 2(I3, 7x), 5(f9.2, 5x))
5100 FORMAT (I4)
5101 FORMAT (F8.2)
5102 FORMAT (A20)


!close ( UniGLimT )
close ( 298 )

return
end