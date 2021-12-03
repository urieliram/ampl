
! ---------------------------------------------------------------------
! Imprime resultados de asignaciones, despachos y variables duales    *
! para unidades de rango discontinuo.                                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2017                                                      *
! ---------------------------------------------------------------------
Subroutine AsigDesRD (sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

INTEGER   i, u, modo, modoD, UniTransCSV, sistema
INTEGER   uunidad, modoTrans (maxurd, maxint), ierror
character*1 ssistema

! Resultados CSV de cambio de modo de unidades de rango discontinuo
uunidad = 140 + sistema
UniTransCSV = uunidad
Write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = UniTransCSV, FILE = trim(rut_dat_1)//'RESTRANS_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

! Escribe solucion de asignacion y despacho

write (UnichauRD,*) 'Generacion de unidades de rango discontinuo'
write (UnichauRD,*)

! Escribe solucion de generacion de unidades de rango discontinuo
do u = 1, NumUniRD
!   para todos los intervalos de planeacion
    write ( UnichauRD, 700 ) u, nombunird(u), ( GENUNRD(u,i)*Base, i=1,NTINTR )
    do i = 1, NTINTR
        if ( GENUNRD(u,i) .le. 0.9 ) then
            RESMODO ( u+NumUniRC, i ) = 1
        else
            RESMODO ( u+NumUniRC, i ) = 11
!           para todos los modos de operacion
            do modo = 2, NumModRD ( u )
                if ( xMILP(IARD+INIURDI(u,i)+modo-1) .gt. 0.9 ) then
                    RESMODO ( u+NumUniRC, i ) = modo
                    exit
                endif
            enddo
        endif
    enddo
enddo

write (UnichauRD,*)
write (UnichauRD,*) 'Generacion de unidades de rango discontinuo por modo de operacion'
write (UnichauRD,*)

modoTrans = 0

! Escribe solucion de generacion de unidades de rango discontinuo por modo de operacion
do u = 1, NumUniRD
    write ( UnichauRD, 500 ) u, nombunird(u)
    write (UnichauRD,*)
!   para todos los modos de operacion
    do modo = 1, NumModRD ( u )
!       para todos los intervalos de planeacion
        write ( UnichauRD, 800 ) 'Genera', ( xMILP(IGRD+INIURDI(u,i)+modo-1)*Base, i=1,NTINTR )
        write ( UnichauRD, 800 ) 'Gen Ar', ( xMILP(IGDARD+INIURDI(u,i)+modo-1)*Base, i=1,NTINTR )
        write ( UnichauRD, 800 ) 'Asigna', ( xMILP(IARD+INIURDI(u,i)+modo-1), i=1,NTINTR )
        write ( UnichauRD, 800 ) 'AsigAr', ( xMILP(IADARD+INIURDI(u,i)+modo-1), i=1,NTINTR )
        write ( UnichauRD, 800 ) 'Arranq', ( xMILP(IARRD+INIURDI(u,i)+modo-1), i=1,NTINTR )
        write (UnichauRD,*)
        do modoD = 1, NumModRD ( u )
            write ( UnichauRD, 800 ) 'AlphaT', ( xMILP(IOMARD + INALPHAT ( u, i ) + (modo-1)*NumModRD ( u ) + modoD - 1), i=1,NTINTR )
!           si hubo un cambio de transicion
            do i = 1, NTINTR
                if ( xMILP(IOMARD + INALPHAT ( u, i ) + (modo-1)*NumModRD ( u ) + modoD - 1) .gt. 0 ) then
                    modoTrans (u, i) = modoD
                endif
            enddo
        enddo
        write (UnichauRD,*)
    enddo
    write ( UnichauRD, 800 ) 'Paro  ', ( xMILP(IPRD+u+(i-1)*NumUniRD-1), i=1,NTINTR )
    write (UnichauRD,*)
    write ( UniTransCSV, 900 ) ( modoTrans (u, i), i=1,NTINTR )
enddo

close ( UniTransCSV )
!close ( UnichauRD )

500 format ( i4, x, a5 )
600 format ( 169(f9.2) )
700 format ( i4, x, a5, 169(f9.2) )
800 format ( a10, 169(f9.2) )
900 format ( 169(i4, ',') )

return
end
    

Subroutine ResZonUniRD
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por zona aceptadas por unidad de *
! rango discontinuo.                                                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2015                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, j, r, u, iniciou, unidad, modo
real*8    TresR10 ( maxint ), Tres10 ( maxint ), TresS ( maxint ), &
          TresRe ( maxint ), TresNR10 ( maxint ), TresRS ( maxint ), &
          TresNRS ( maxint )

write (UnirznuRD,*) 'Reservas aceptadas por zona y unidades'
write (UnirznuRD,*)
! para todo los grupos de reserva
do r = 1, NumGruRes
!   si la zona tiene unidades de rango continuo
    if ( NumURDxZona ( r ) .ne. 0 ) then
        TresR10 = 0.0
        TresNR10 = 0.0
        Tres10 = 0.0
        TresS = 0.0
        TresRS = 0.0
        TresNRS = 0.0
        TresRe = 0.0
        write ( UnirznuRD, * ) 'Zona : ', NomZonaRes ( r )
        write ( UnirznuRD, * )
!       para las unidades de rango discontinuo que estan en ese grupo (zona)
        iniciou = ApunURDxZona ( r )
        do unidad = 1 , NumURDxZona ( r )
            u = UniRDxZona ( iniciou )
            iniciou = iniciou + 1
            write ( UnirznuRD, 500 ) u, nombunird(u)
            write ( UnirznuRD, * )
!           para los modos de la unidad
            do modo = 1, NumModRD ( u )
                write ( UnirznuRD, 600 ) ( xMILP ( IRR10RD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                write ( UnirznuRD, 601 ) ( xMILP ( IRNR10RD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                write ( UnirznuRD, 602 ) ( xMILP ( IRRSRD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                write ( UnirznuRD, 603 ) ( xMILP ( IRNRSRD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                write ( UnirznuRD, 604 ) ( xMILP ( IRRERD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                write ( UnirznuRD, * )
            enddo
            do i = 1, NTINTR
!               para los modos de la unidad
                do modo = 1, NumModRD ( u )
                    TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 )
                    TresNR10 ( i ) = TresNR10 ( i ) + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )
                    Tres10 ( i ) = Tres10 ( i ) + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )
                    TresS ( i ) = TresS ( i ) + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 ) + &
                                                xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )
                    TresRS ( i ) = TresRS ( i ) + xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 )
                    TresNRS ( i ) = TresNRS ( i ) + xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )
                    TresRe ( i ) = TresRe ( i ) + xMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 )
                enddo
            enddo
        enddo
        write ( UnirznuRD, 605 ) 'TRR10 ', ( TresR10 ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 ) 'TRNR10', ( TresNR10 ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 ) 'TR10  ', ( Tres10 ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 ) 'TRRSU ', ( TresRS ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 ) 'TRNRSU', ( TresNRS ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 ) 'TRSU  ', ( TresS ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 ) 'TREG  ', ( TresRe ( j )*Base, j=1,NTINTR )
        write ( UnirznuRD, 605 )
    endif
enddo

500 format ( i4, x, a5 )
600 format ( x, 'RR10', 2x, 169(f8.2) )
601 format ( x, 'RNR10', x, 169(f8.2) )
602 format ( x, 'RRS', 3x, 169(f8.2) )
603 format ( x, 'RNRS', 2x, 169(f8.2) )
604 format ( x, 'RRE', 3x, 169(f8.2) )
605 format ( x, a6, 169(f8.2) )

!close ( UnirznuRD )

return
end
    

Subroutine ResSisUniRD
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por sistema aceptadas por unidad *
! de rango discontinuo.                                               *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2015                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, j, r, u, modo, uunidad, UniRR10CSV, ierror, UniRNR10CSV, &
          UniRRSCSV, UniRNRSCSV, UniRRECSV
real*8    TresR10 ( maxint ), Tres10 ( maxint ), TresS ( maxint ), &
          TresRe ( maxint ), TresNR10 ( maxint ), TresRS ( maxint ), &
          TresNRS ( maxint ), auxRR10 ( maxurd, maxint ), auxRNR10 ( maxurd, maxint ), &
          auxRRS ( maxurd, maxint ), auxRNRS ( maxurd, maxint ), auxRRE ( maxurd, maxint )

character*1 ssistema

write (UnirsnuRD,*) 'Reservas aceptadas por sistema y unidades'
write (UnirsnuRD,*)
! para todos los sistemas
do r = 1, numsis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        Write( ssistema, '(I1)' )  r
        !Abre archivos csv de resultados de reserva rodante de diez minutos por unidad por hora por subsitema
        uunidad = 118 + r
        UniRR10CSV = uunidad
        OPEN ( UNIT = UniRR10CSV, FILE = trim(rut_dat_1)//'RESRERO10U_'//ssistema//'.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='old', RECORDSIZE = 6000 )
        uunidad = 149 + r
        UniRNR10CSV = uunidad
        OPEN ( UNIT = UniRNR10CSV, FILE = trim(rut_dat_1)//'RESRENRO10U_'//ssistema//'.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='old', RECORDSIZE = 6000 )
        uunidad = 159 + r
        UniRRSCSV = uunidad
        OPEN ( UNIT = UniRRSCSV, FILE = trim(rut_dat_1)//'RESREROSUU_'//ssistema//'.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='old', RECORDSIZE = 6000 )
        uunidad = 169 + r
        UniRNRSCSV = uunidad
        OPEN ( UNIT = UniRNRSCSV, FILE = trim(rut_dat_1)//'RESRENROSUU_'//ssistema//'.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='old', RECORDSIZE = 6000 )
        uunidad = 179 + r
        UniRRECSV = uunidad
        OPEN ( UNIT = UniRRECSV, FILE = trim(rut_dat_1)//'RESRERESEU_'//ssistema//'.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='old', RECORDSIZE = 6000 )
        TresR10 = 0.0
        TresNR10 = 0.0
        Tres10 = 0.0
        TresS = 0.0
        TresRS = 0.0
        TresNRS = 0.0
        TresRe = 0.0
        write ( UnirsnuRD, * ) 'Sistema: ', nomsis ( r )
        write ( UnirsnuRD, * )
!       para las unidades de rango discontinuo que estan en ese sistema
        auxRR10 = 0.0
        auxRNR10 = 0.0
        auxRRS = 0.0
        auxRNRS = 0.0
        auxRRE = 0.0
        do u = 1 , NumUniRD
            if ( IslaGenRC ( u ) .eq. r ) then
                write ( UnirsnuRD, 500 ) u, nombunird(u)
                write ( UnirsnuRD, * ) 
!               para los modos de la unidad
                do modo = 1, NumModRD ( u )
                    write ( UnirsnuRD, 600 ) ( xMILP ( IRR10RD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )                    
                    write ( UnirsnuRD, 601 ) ( xMILP ( IRNR10RD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                    do j = 1,NTINTR  
                        ResNR10URD ( u, modo, j ) =  xMILP ( IRNR10RD + INIURDI ( u, j ) + modo - 1 )
                    end do
                    write ( UnirsnuRD, 602 ) ( xMILP ( IRRSRD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                    write ( UnirsnuRD, 603 ) ( xMILP ( IRNRSRD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                    do j = 1,NTINTR  
                        ResNRSUURD ( u, modo, j ) =  xMILP ( IRNRSRD + INIURDI ( u, j ) + modo - 1 )
                    end do
                    write ( UnirsnuRD, 604 ) ( xMILP ( IRRERD + INIURDI ( u, j ) + modo - 1 )*Base, j=1,NTINTR )
                    !hacer para todos los intervalos
                    do j =1, ntintr
                        !Se suma la reserva rodante de diez minutos de todos los modos de la unidad por hora
                        auxRR10 ( u, j ) = auxRR10 ( u, j ) + xMILP ( IRR10RD + INIURDI ( u, j ) + modo - 1 )*Base
                        !Se suma la reserva no rodante de diez minutos de todos los modos de la unidad por hora
                        auxRNR10 ( u, j ) = auxRNR10 ( u, j ) + xMILP ( IRNR10RD + INIURDI ( u, j ) + modo - 1 )*Base
                        !Se suma la reserva rodante suplementaria de todos los modos de la unidad por hora
                        auxRRS ( u, j ) = auxRRS ( u, j ) + xMILP ( IRRSRD + INIURDI ( u, j ) + modo - 1 )*Base
                        !Se suma la reserva no rodante suplementaria de todos los modos de la unidad por hora
                        auxRNRS ( u, j ) = auxRNRS ( u, j ) + xMILP ( IRNRSRD + INIURDI ( u, j ) + modo - 1 )*Base
                        !Se suma la reserva de regulacion secundaria de todos los modos de la unidad por hora
                        auxRRE ( u, j ) = auxRRE ( u, j ) + xMILP ( IRRERD + INIURDI ( u, j ) + modo - 1 )*Base
                    end do
                    write ( UnirsnuRD, * )
                enddo
                do i = 1, NTINTR
!                   para los modos de la unidad
                    do modo = 1, NumModRD ( u )
                        TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 )
                        TresNR10 ( i ) = TresNR10 ( i ) + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )
                        Tres10 ( i ) = Tres10 ( i ) + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 )
                        TresS ( i ) = TresS ( i ) + xMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 ) + &
                                                    xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) + xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )
                        TresRS ( i ) = TresRS ( i ) + xMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 )
                        TresNRS ( i ) = TresNRS ( i ) + xMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 )
                        TresRe ( i ) = TresRe ( i ) + xMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 )
                    enddo
                enddo
            endif
            !Escribe CSV
            write ( UniRR10CSV, 700 ) nombunird(u), 1, ( auxRR10 ( u, j ), j=1,NTINTR )
            !Escribe CSV
            write ( UniRNR10CSV, 700 ) nombunird(u), 1, ( auxRNR10 ( u, j ), j=1,NTINTR )
            !Escribe CSV
            write ( UniRRSCSV, 700 ) nombunird(u), 1, ( auxRRS ( u, j ), j=1,NTINTR )
            !Escribe CSV
            write ( UniRNRSCSV, 700 ) nombunird(u), 1, ( auxRNRS ( u, j ), j=1,NTINTR )
            !Escribe CSV
            write ( UniRRECSV, 700 ) nombunird(u), 1, ( auxRRE ( u, j ), j=1,NTINTR )
        enddo
        write ( UnirsnuRD, 605 ) 'TRR10 ', ( TresR10 ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 ) 'TRNR10', ( TresNR10 ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 ) 'TR10  ', ( Tres10 ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 ) 'TRRSU ', ( TresRS ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 ) 'TRNRSU', ( TresNRS ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 ) 'TRSU  ', ( TresS ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 ) 'TREG  ', ( TresRe ( j )*Base, j=1,NTINTR )
        write ( UnirsnuRD, 605 )
        
        close ( UniRR10CSV )
        close ( UniRNR10CSV )
        close ( UniRRSCSV )
        close ( UniRNRSCSV )
        close ( UniRRECSV )
        
    endif
enddo

500 format ( i4, x, a5 )
600 format ( x, 'RR10', 2x, 169(f8.2) )
601 format ( x, 'RNR10', x, 169(f8.2) )
602 format ( x, 'RRS', 3x, 169(f8.2) )
603 format ( x, 'RNRS', 2x, 169(f8.2) )
604 format ( x, 'RRE', 3x, 169(f8.2) )
605 format ( x, a6, 169(f8.2) )
700 format ( a12, ',', i2, ',', 169(f10.2, ',')  )

!close ( UnirsnuRD )

return
end
