
! ---------------------------------------------------------------------
! Imprime resultados de asignaciones, despachos y variables duales    *
! para unidades de rango continuo.                                    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2017                                                      *
! ---------------------------------------------------------------------
Subroutine AsigDesRC

use ParAUHE
use ProblemaAUHE

Implicit none

INTEGER   i, u, k, bloque

Real*8    gensinc ( maxurc, maxint ), asigsinc ( maxurc, maxint ), & 
          arranque ( maxurc, maxint )

RESMODO = 0

! Escribe solucion de asignacion y despacho

write (UnichauRC,*) 'Generacion de unidades de rango continuo'
write (UnichauRC,*)

! Escribe solucion de generacion de unidades de rango continuo
do u = 1, NumUniRC
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque 
       write ( UnichauRC, 600 ) u, nombunirc(u), ( GENUNRC(u,i)*Base, i=k*24-23,k*24 )
    enddo
    do i = 1, NTINTR
        if ( u.eq.31 .and. i.eq.12 ) then
            continue
        endif
        if ( xMILP ( IARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 .and. DispoURC ( u , i ) .eq. 1 .and. CompSincRC (u, i ) .eq. 0 ) then
            RESMODO ( u, i ) = 1
        endif
        if ( xMILP ( IARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 .and. DispoURC ( u , i ) .eq. 1 .and. CompSincRC (u, i ) .eq. 2 ) then
            if ( xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) .gt. 0.0  .or. xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) .gt. 0.0 ) then
                RESMODO ( u, i ) = 1
            endif
        endif
    enddo
enddo

write (UnichauRC,*)
write (UnichauRC,*) 'Generacion sincronizacion de unidades de rango continuo'
write (UnichauRC,*)

! Escribe solucion de generacion de sincronizacion en unidades de rango continuo
do u = 1, NumUniRC
    do i = 1, NTINTR
        gensinc ( u, i ) = xMILP ( IGDARC + u + (i-1)*NumUniRC - 1 )*Base
    enddo
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque
       write ( UnichauRC, 600 ) u, nombunirc(u), ( gensinc(u,i), i=k*24 -23,k*24 )
    enddo
enddo

write (UnichauRC,*)
write (UnichauRC,*) 'Arranque de unidades de rango continuo'
write (UnichauRC,*)

! Escribe solucion de arranque de unidades de rango continuo
do u = 1, NumUniRC
    do i = 1, NTINTR
        arranque ( u, i ) = xMILP ( IARRC + u + (i-1)*NumUniRC - 1 )
    enddo
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque
       write ( UnichauRC, 600 ) u, nombunirc(u), ( arranque(u,i), i=k*24-23,k*24 )
    enddo
enddo

write (UnichauRC,*)
write (UnichauRC,*) 'Asignacion sincronizacion de unidades de rango continuo'
write (UnichauRC,*)

! Escribe solucion de asignacion de sincronizacion en unidades de rango continuo
do u = 1, NumUniRC
    do i = 1, NTINTR
        asigsinc ( u, i ) = xMILP ( IADARC + u + (i-1)*NumUniRC - 1 )
        if ( asigsinc ( u, i ) .gt. 0 ) then
            RESMODO ( u, i ) = 11
        endif
    enddo
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque
       write ( UnichauRC, 600 ) u, nombunirc(u), ( asigsinc(u,i), i=k*24-23,k*24 )
    enddo
enddo


!close ( UnichauRC )

600 format ( i4, x, a12, 169(f9.2) )

return
end
    

    
Subroutine ResZonUniRC ( y )
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por zona aceptadas por unidad de *
! rango continuo.                                                     *
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

INTEGER   i, j, r, u, iniciou, unidad, k, bloque, Uniduallm, ierror, irest
real*8    TresR10 ( maxint ), Tres10 ( maxint ), TresS ( maxint ), &
          TresRe ( maxint ), TresNR10 ( maxint ), TresRS ( maxint ), &
          TresNRS ( maxint ), y
DIMENSION   y          ( maxresMILP )
real*8  pmin, pbase, rre, rro10, rrosu, suma, pmax, dual

! Resultados de costos marginales de limites maximos de las unidades
Uniduallm = 333
!OPEN ( UNIT = Uniduallm, FILE = RUT_RES//'r_dualLMRC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3500 )

write (UnirznuRC,*) 'Reservas aceptadas por zona y unidades'
write (UnirznuRC,*)
! para todo los grupos de reserva
do r = 1, NumGruRes
!   si la zona tiene unidades de rango continuo
    if ( NumURCxZona ( r ) .ne. 0 ) then
        TresR10 = 0.0
        TresNR10 = 0.0
        Tres10 = 0.0
        TresS = 0.0
        TresRS = 0.0
        TresNRS = 0.0
        TresRe = 0.0
        write ( UnirznuRC, * ) 'Zona : ', NomZonaRes ( r )
        write ( UnirznuRC, * )
!        write ( Uniduallm, * ) 'Zona : ', NomZonaRes ( r )
!        write ( Uniduallm, * )
!       para las unidades de rango continuo que estan en ese grupo (zona)
        iniciou = ApunURCxZona ( r )
        do unidad = 1 , NumURCxZona ( r )
            u = UniRCxZona ( iniciou )
            iniciou = iniciou + 1
            write ( UnirznuRC, 500 ) u, nombunirc(u)
            write ( UnirznuRC, * ) 
            bloque = NTINTR/24
            do k = 1, bloque
                if ( SiRelRod .eq. 1 ) then
                    write ( UnirznuRC, 600 ) ( xMILP ( IRR10RC + u + (j-1)*NumUniRC - 1 )*Base - xMILP ( IREUSO10 + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                else
                    write ( UnirznuRC, 600 ) ( xMILP ( IRR10RC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                endif
            enddo
            do k = 1, bloque
               write ( UnirznuRC, 601 ) ( xMILP ( IRNR10RC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
            enddo
            do k = 1, bloque
               write ( UnirznuRC, 602 ) ( xMILP ( IRRSRC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
            enddo
            do k = 1, bloque
               write ( UnirznuRC, 603 ) ( xMILP ( IRNRSRC + u + (j-1)*NumUniRC - 1 )*Base,j=k*24-23,k*24 )
            enddo
            do k = 1, bloque
                if ( TipoEjecu .le. 1 ) then
                    write ( UnirznuRC, 604 ) ( xMILP ( IRRERC + u + (j-1)*NumUniRC - 1 )*Base - xMILP ( IREUSORE + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                else
                    write ( UnirznuRC, 604 ) ( xMILP ( IRRERC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                endif
            enddo
            write ( UnirznuRC, * )
!            write ( Uniduallm, 606 ) u, nombunirc(u)
!            write ( Uniduallm, * ) 'intervalo, restriccion, pmin, punto base, rre, rro10, rrosu, total, pmax, dual'
!            write ( Uniduallm, * ) 'intervalo, pmin, punto base, rre, rro10, rrosu, total, pmax, dual'
            do i = 1, NTINTR
                pbase = GENUNRC ( u, i )*Base
                rre = xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )*Base
                rro10 = xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 )*Base
                rrosu = xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )*Base
                suma = pbase + rre + rro10 + rrosu
                pmin = PotMinGRC ( u, i )*Base
                pmax = PotMaxGRC ( u, i )*Base
                irest = IRLMRC + i + (u-1)*NTINTR - 1
                dual = y ( irest )/Base
!                write ( Uniduallm, 607 ) i, irest, pmin, pbase, rre, rro10, rrosu, suma, pmax, dual
!                write ( Uniduallm, 607 ) i, pmin, pbase, rre, rro10, rrosu, suma, pmax, dual
            enddo
            do i = 1, NTINTR
                if ( TipoEjecu .le. 1 ) then
                    TresRe ( i ) = TresRe ( i ) + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) - xMILP ( IREUSORE + u + (i-1)*NumUniRC - 1 )
                else
                    TresRe ( i ) = TresRe ( i ) + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )
                endif
                if ( SiRelRod .eq. 1 ) then
                    TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) - xMILP ( IREUSO10 + u + (i-1)*NumUniRC - 1 )
                else
                    TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 )
                endif
                TresNR10 ( i ) = TresNR10 ( i ) + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )
!                Tres10 ( i ) = Tres10 ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )
                Tres10 ( i ) = TresRe ( i )*SiRegEnRod + TresR10 ( i ) + TresNR10 ( i )
                TresRS ( i ) = TresRS ( i ) + xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )
                TresNRS ( i ) = TresNRS ( i ) + xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )
!                TresS ( i ) = TresS ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) + &
!                                            xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )
                TresS ( i ) = Tres10 ( i ) + TresRS ( i ) + TresNRS ( i )
            enddo
        enddo
        bloque = NTINTR/24
        do k = 1, bloque
           write ( UnirznuRC, 605 ) 'TREG  ', ( TresRe ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirznuRC, 605 ) 'TRR10 ', ( TresR10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirznuRC, 605 ) 'TRNR10', ( TresNR10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
        write ( UnirznuRC, 605 ) 'TR10  ', ( Tres10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
        write ( UnirznuRC, 605 ) 'TRRSU ', ( TresRS ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
        write ( UnirznuRC, 605 ) 'TRNRSU', ( TresNRS ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
        write ( UnirznuRC, 605 ) 'TRSU  ', ( TresS ( j )*Base, j=k*24-23,k*24 )
        enddo
        write ( UnirznuRC, 605 )
    endif
enddo

!close ( Uniduallm )
500 format ( i4, x, a12 )
600 format ( x, 'RR10', 2x, 169(f8.2) )
601 format ( x, 'RNR10', x, 169(f8.2) )
602 format ( x, 'RRS', 3x, 169(f8.2) )
603 format ( x, 'RNRS', 2x, 169(f8.2) )
604 format ( x, 'RRE', 3x, 169(f8.2) )
605 format ( x, a6, 169(f8.2) )
606 format ( i4, ',', a12,',')
!607 format ( i4, ',', i7, ',', 7(f10.2,','), f15.2, ',' )
607 format ( i4, ',', 7(f10.2,','), f15.2, ',' )

!close ( UnirznuRC )

return
end

   
Subroutine ResSisUniRC
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por sistema aceptadas por unidad *
! de rango continuo.                                                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2019                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, j, r, u, uunidad, UniRR10CSV, ierror, UniRNR10CSV, &
          UniRRSCSV, UniRNRSCSV, UniRRECSV, k, bloque
real*8    TresR10 ( maxint ), Tres10 ( maxint ), TresS ( maxint ), &
          TresRe ( maxint ), TresNR10 ( maxint ), TresRS ( maxint ), &
          TresNRS ( maxint )

character*1 ssistema

write (UnirsnuRC,*) 'Reservas aceptadas por sistema y unidades'
write (UnirsnuRC,*)
! para todos los sistemas
do r = 1, numsis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        Write( ssistema, '(I1)' )  r
        !Abre archivos csv de resultados de reserva rodante de diez minutos por unidad por hora por subsitema
        uunidad = 118 + r
        UniRR10CSV = uunidad
        OPEN ( UNIT = UniRR10CSV, FILE = trim(rut_dat_1)//'RESRERO10U_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        uunidad = 149 + r
        UniRNR10CSV = uunidad
        OPEN ( UNIT = UniRNR10CSV, FILE = trim(rut_dat_1)//'RESRENRO10U_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        uunidad = 159 + r
        UniRRSCSV = uunidad
        OPEN ( UNIT = UniRRSCSV, FILE = trim(rut_dat_1)//'RESREROSUU_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        uunidad = 169 + r
        UniRNRSCSV = uunidad
        OPEN ( UNIT = UniRNRSCSV, FILE = trim(rut_dat_1)//'RESRENROSUU_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        uunidad = 179 + r
        UniRRECSV = uunidad
        OPEN ( UNIT = UniRRECSV, FILE = trim(rut_dat_1)//'RESRERESEU_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
        TresR10 = 0.0
        TresNR10 = 0.0
        Tres10 = 0.0
        TresS = 0.0
        TresRS = 0.0
        TresNRS = 0.0
        TresRe = 0.0
        write ( UnirsnuRC, * ) 'Sistema: ', nomsis ( r )
        write ( UnirsnuRC, * )
!       para las unidades de rango continuo que estan en ese sistema
        do u = 1 , NumUniRC
            if ( IslaGenRC ( u ) .eq. r ) then
                write ( UnirsnuRC, 500 ) u, nombunirc(u)
                write ( UnirsnuRC, * ) 
                bloque = NTINTR/24
                do k = 1, bloque
                    if ( SiRelRod .eq. 1 ) then
                        write ( UnirsnuRC, 600 ) ( xMILP ( IRR10RC + u + (j-1)*NumUniRC - 1 )*Base - xMILP ( IREUSO10 + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                    else
                        write ( UnirsnuRC, 600 ) ( xMILP ( IRR10RC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                    endif
                enddo
                !Escribe CSV
                if ( SiRelRod .eq. 1 ) then
                    write ( UniRR10CSV, 700 ) nombunirc(u), 0, ( xMILP ( IRR10RC + u + (j-1)*NumUniRC - 1 )*Base - xMILP ( IREUSO10 + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                else
                    write ( UniRR10CSV, 700 ) nombunirc(u), 0, ( xMILP ( IRR10RC + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                endif
                do k = 1, bloque
                   write ( UnirsnuRC, 601 ) ( xMILP ( IRNR10RC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                enddo
                do j = 1,NTINTR  
                    ResNR10URC ( u, j ) =  xMILP ( IRNR10RC + u + (j-1)*NumUniRC - 1 )
                end do
                !Escribe CSV
                write ( UniRNR10CSV, 700 ) nombunirc(u), 0, ( xMILP ( IRNR10RC + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                   write ( UnirsnuRC, 602 ) ( xMILP ( IRRSRC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                enddo
                !Escribe CSV
                write ( UniRRSCSV, 700 ) nombunirc(u), 0, ( xMILP ( IRRSRC + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                   write ( UnirsnuRC, 603 ) ( xMILP ( IRNRSRC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                enddo
                do j = 1,NTINTR  
                    ResNRSUURC ( u, j ) =  xMILP ( IRNRSRC + u + (j-1)*NumUniRC - 1 )
                end do
                !Escribe CSV
                write ( UniRNRSCSV, 700 ) nombunirc(u), 0, ( xMILP ( IRNRSRC + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                    if ( TipoEjecu .le. 1 ) then
                        write ( UnirsnuRC, 604 ) ( xMILP ( IRRERC + u + (j-1)*NumUniRC - 1 )*Base - xMILP ( IREUSORE + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                    else
                        write ( UnirsnuRC, 604 ) ( xMILP ( IRRERC + u + (j-1)*NumUniRC - 1 )*Base, j=k*24-23,k*24 )
                    endif
                enddo
                !Escribe CSV
                if ( TipoEjecu .le. 1 ) then
                    write ( UniRRECSV, 700 ) nombunirc(u), 0, ( xMILP ( IRRERC + u + (j-1)*NumUniRC - 1 )*Base - xMILP ( IREUSORE + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                else
                    write ( UniRRECSV, 700 ) nombunirc(u), 0, ( xMILP ( IRRERC + u + (j-1)*NumUniRC - 1 )*Base, j=1,NTINTR )
                endif
                write ( UnirsnuRC, * )
                do i = 1, NTINTR
                    if ( TipoEjecu .le. 1 ) then
                        TresRe ( i ) = TresRe ( i ) + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) - xMILP ( IREUSORE + u + (i-1)*NumUniRC - 1 )
                    else
                        TresRe ( i ) = TresRe ( i ) + xMILP ( IRRERC + u + (i-1)*NumUniRC - 1 )
                    endif
                    if ( SiRelRod .eq. 1 ) then
                        TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) - xMILP ( IREUSO10 + u + (i-1)*NumUniRC - 1 )
                    else
                        TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 )
                    endif
                    TresNR10 ( i ) = TresNR10 ( i ) + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )
!                    Tres10 ( i ) = Tres10 ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 )
                    Tres10 ( i ) = TresRe ( i )*SiRegEnRod + TresR10 ( i ) + TresNR10 ( i )
                    TresRS ( i ) = TresRS ( i ) + xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )
                    TresNRS ( i ) = TresNRS ( i ) + xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )
!                    TresS ( i ) = TresS ( i ) + xMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) + &
!                                                xMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) + xMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 )
                    TresS ( i ) = Tres10 ( i ) + TresRS ( i ) + TresNRS ( i )
                enddo
            endif
        enddo
        bloque = NTINTR/24
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TREG  ', ( TresRe ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TRR10 ', ( TresR10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TRNR10', ( TresNR10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TR10  ', ( Tres10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TRRSU ', ( TresRS ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TRNRSU', ( TresNRS ( j )*Base,j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuRC, 605 ) 'TRSU  ', ( TresS ( j )*Base, j=k*24-23,k*24 )
        enddo
        write ( UnirsnuRC, 605 )
        
        close ( UniRR10CSV )
        close ( UniRNR10CSV )
        close ( UniRRSCSV )
        close ( UniRNRSCSV )
        close ( UniRRECSV )
        
    endif
enddo

500 format ( i4, x, a12 )
600 format ( x, 'RR10', 2x, 169(f8.2) )
601 format ( x, 'RNR10', x, 169(f8.2) )
602 format ( x, 'RRS', 3x, 169(f8.2) )
603 format ( x, 'RNRS', 2x, 169(f8.2) )
604 format ( x, 'RRE', 3x, 169(f8.2) )
605 format ( x, a6, 169(f8.2) )
700 format ( a12, ',', i2, ',', 169(f10.2, ',')  )

!close ( UnirsnuRC )

return
end

!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Imprime archivo csv de salida con la asignabilidad modificada   *
!         para dar entrada al AUGC                                        *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Agosto 2015                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Asig_Out_MDA

use ParAUHE, only: NumUniRC, maxurc, maxurd, maxuh, maxure, ntintr, GENUNRC, &
                  maxint, AsignURC, rut_dat_1, corresprc, RESMODO, NumUniRD, &
                  genunrd, TiempoTrans, AsignURD, maxmodos, NumModRD, corresprd, SisUniRC, SisUniRD
!
IMPLICIT NONE

integer AsigOutput ( maxurc + maxuh + maxure, maxint ), u, i, ierror, &
        consecutivo, intervalo, generico ( maxurc + maxurd + maxuh + maxure, maxint ), &
        AsigOutputRD ( maxurd, maxmodos, maxint ), genericoRD ( maxurd, maxmodos, maxint ), m


character*3000 letaux

AsigOutput = 1

generico = 1

!Para las unidades de rango continuo
do u = 1, NumUniRC
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        if ( GENUNRC(u,i) .gt. 0 ) then
            if ( RESMODO ( u, i ) .ne. 11 ) then                
                AsigOutput ( u, i ) = 0
            end if
        endif
    enddo
enddo

!Escribe archivo de asignabilidad de salida ASIGN_MDA_RC.csv

consecutivo = 1

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_MDA_RC.csv',IOSTAT = IERROR,  STATUS='unknown', RECORDSIZE = 6000 )

if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. consecutivo .le. SisUniRC )        
	    read ( 311, 100, iostat = ierror ) letaux
        if ( ierror .ne. 0 ) then
            do u = 1, NumUniRC
                if ( corresprc ( u ) .eq. consecutivo ) then
                    generico ( consecutivo, : ) = AsigOutput ( u, : )
                    consecutivo = consecutivo + 1
                else
                    !do while ( corresprc ( u ) .ne. consecutivo .and. ierror .ne. 0  )
                    do while ( corresprc ( u ) .ne. consecutivo .and. ierror .ne. 0 .and. consecutivo .le. SisUniRC )
                        generico ( consecutivo, : ) = 1
                        consecutivo = consecutivo + 1
                    end do
                    generico ( consecutivo, : ) = AsigOutput ( u, : )
                    consecutivo = consecutivo + 1 
                end if
            enddo
        else
            !leer informacion
            do u = 1, NumUniRC
                if ( corresprc ( u ) .eq. consecutivo ) then
                    generico ( consecutivo, : ) = AsigOutput ( u, : )
                    consecutivo = consecutivo + 1
                    read ( 311, 100, iostat = ierror ) letaux
                else
                    do while ( corresprc ( u ) .ne. consecutivo .and. ierror .eq. 0  )
                        read ( letaux, * ) ( generico ( consecutivo, intervalo ), intervalo = 1 , ntintr  ) 
                        consecutivo = consecutivo + 1
                        read ( 311, 100, iostat = ierror ) letaux
                    end do
                    if ( corresprc ( u ) .eq. consecutivo ) then
                        generico ( consecutivo, : ) = AsigOutput ( u, : )
                        consecutivo = consecutivo + 1 
                    end if
                end if
            enddo
        end if
    end do
end if



close ( 311 )

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_MDA_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do u = 1, consecutivo - 1
    write ( 311, 600 ) ( generico ( u, intervalo ), intervalo = 1 , ntintr  ) 
end do

close ( 311 )


!Inicilizar asignacion de modos para Rango Discontinuo
AsigOutputRD = 0

!Poner por default todos los modos existentes asignables
!Hacer para todas las unidades
do u = 1, NumUniRD
    !Hacer para todos los modos existentes
    do m = 1, NumModRD(u)
        AsigOutputRD ( u, m, : ) = 1
    end do 
end do

!Para las unidades de rango discontinuo
do u = 1, NumUniRD
    !para todos los intervalos de planeacion
    do i = 1, NTINTR
        if ( GENUNRD(u,i) .gt. 0 ) then
            if ( RESMODO ( u + NumUniRC, i ) .ne. 11 ) then              
                m = RESMODO ( u + NumUniRC, i )
                AsigOutputRD ( u, m, i ) = 0
            end if
        endif
    enddo
enddo

consecutivo = 1

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_MDA_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

!Si no hay unidades de rango discontinuo
if ( NumUniRD .eq. 0 ) then
    ierror = 1
end if

if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. consecutivo .le. SisUniRD )        
	    read ( 311, 100, iostat = ierror ) letaux
        if ( ierror .ne. 0 ) then
            do u = 1, NumUniRD
                if ( corresprd ( u ) .eq. consecutivo ) then
                    !hacer para el maximo de moodos
                    do m = 1, maxmodos 
                        genericoRD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )                        
                    end do
                    consecutivo = consecutivo + 1
                else
                    !do while ( corresprd ( u ) .ne. consecutivo .and. ierror .ne. 0 )
                    do while ( corresprd ( u ) .ne. consecutivo .and. ierror .ne. 0 .and. consecutivo .le. SisUniRD )
                        !hacer para el maximo de moodos
                        do m = 1, maxmodos 
                            genericoRD ( consecutivo, m, : ) = 1                            
                        end do
                        consecutivo = consecutivo + 1
                    end do
                    do m = 1, maxmodos 
                        genericoRD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                    end do 
                    consecutivo = consecutivo + 1 
                end if
            enddo
        else
            !leer informacion
            do u = 1, NumUniRD
                if ( corresprd ( u ) .eq. consecutivo ) then
                    !hacer para el maximo de moodos
                    do m = 1, maxmodos 
                        genericoRD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                        read ( 311, 100, iostat = ierror ) letaux                        
                    end do
                    consecutivo = consecutivo + 1
                else
                    do while ( corresprd ( u ) .ne. consecutivo .and. ierror .eq. 0  )
                        !Hacer para el  maximo de modos
                        do m = 1, maxmodos 
                            read ( letaux, * ) ( genericoRD ( consecutivo, m, intervalo ), intervalo = 1 , ntintr  )                             
                            read ( 311, 100, iostat = ierror ) letaux
                        end do
                        consecutivo = consecutivo + 1
                    end do
                    if ( corresprd ( u )  .eq. consecutivo ) then
                        do m = 1, maxmodos 
                            genericoRD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                            if ( m .lt. maxmodos ) then
                                read ( 311, 100, iostat = ierror ) letaux
                            end if                            
                        end do
                        consecutivo = consecutivo + 1 
                    end if                    
                end if
            enddo
        end if
    end do
end if



close ( 311 )

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_MDA_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do u = 1, consecutivo - 1
    do m = 1, maxmodos
        write ( 311, 600 ) ( genericoRD ( u, m, intervalo ), intervalo = 1 , ntintr  ) 
    end do
end do

close ( 311 )



100  format ( a )
600 format ( 169(I,',') )

end subroutine Asig_Out_MDA
    
    !
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Imprime archivo csv de salida con la asignabilidad y            *
!         y disponibilidad modificada para dar entrada al AUTR            *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Agosto 2015                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Asig_Out_AUGC

use ParAUHE, only: NumUniRC, maxurc, maxurd, maxuh, maxure, ntintr, GENUNRC, BMensaje, NomEjecu, &
                  maxint, AsignURC, ResNR10URC, ResNRSUURC, rut_dat_1, corresprc, &
                  RESMODO, PotMinGRC, PotMaxGRC, base, SisUniRC, NumUniRD, PotMinGRD, PotMaxGRD, &
                  GENUNRD, NumModRD, ResNR10URD, ResNRSUURD, maxmodos, SisUniRD, corresprd, ResNR10UH, ResNRSUUH, &
                  SisUniH, NumUniHid, GENUNH, corresph, NumUniRE, GENUNRE, SisUniRE, correspre, NumEmbalses, NumCenHEmb, &
!                  ApunPlantaEmbalse, ListaPlantasH, NoUnidades_plantaH, ApunUnidadPlanta, ListaUnidadesH, nmxemb, NumGruUTer, &
                  ApunPlantaEmbalse, ListaPlantasH, ApunUnidadPlanta, ListaUnidadesH, nmxemb, NumGruUTer, &
                  ApunURCxGrupo, NumURCxGrupo, UniRCxGrupo, ApunURDxGrupo, NumURDxGrupo, UniRDxGrupo, maxgrute, DurIntAUTR, durintr, &
                  RampArraURC, RampArraURD, nombunirc, nombunih, nombunird, nombunire, DispoUH, DispoURC, ASIGNUH

use ParAuHeHidro, only: NOMEMB, NOUN

IMPLICIT NONE

integer AsigOutput ( maxurc + maxuh + maxure, maxint*4 ), u, i, ibanbit, &
        DispOutput ( maxurc + maxuh + maxure, maxint*4 ), ierror, ierror_1, ierror_2, ierror_3, &
        consecutivo, genericoA ( maxurc + maxurd + maxuh + maxure, maxint*4 ), genericoD ( maxurc + maxurd + maxuh + maxure, maxint*4 ), &
        intervalo, AsigOutputRD ( maxurd, maxmodos, maxint*4 ), DispOutputRD ( maxurd, maxmodos, maxint*4 ), m, j, genericoARD ( maxurd, maxmodos, maxint*4 ), &
        genericoDRD ( maxurd, maxmodos, maxint*4 ), planta, embalse, grupo, iniciou, unidad, contador, int, NoIntAUTR

real*8 genericoPmin ( maxurc + maxurd + maxuh + maxure, maxint*4 ), genericoPmax ( maxurc + maxurd + maxuh + maxure, maxint*4 ), &
       genericoPminRD ( maxurd, maxmodos, maxint*4 ), genericoPmaxRD ( maxurd, maxmodos, maxint*4 ), EnerHidro ( nmxemb, maxint*4 ), &
       EnerTermo ( maxgrute, maxint*4 ), cambio, PotMinGRC_1 ( maxurc, maxint*4 ), PotMaxGRC_1 ( maxurc, maxint*4 ), &
       PotMinGRD_1  ( maxurd, maxmodos, maxint*4 ), PotMaxGRD_1  ( maxurd, maxmodos, maxint*4 )

character*3000 letaux, letaux_1, letaux_2, letaux_3
CHARACTER fecha_Ej*19
character*20 aaux2

NoIntAUTR = durintr / DurIntAUTR 

AsigOutput = 0
DispOutput = 0
!Por default sin es de apagad en i a apagado en i - 1 no se hace nada porque esta inicializado en ND, NA
!Para las unidades de rango continuo
do u = 1, NumUniRC
!   para todos los intervalos de planeacion
    do i = 1, NTINTR - 1
        int = ( i * NoIntAUTR ) - ( NoIntAUTR - 1 )
        if ( GENUNRC(u,i) .gt. 0 .and. GENUNRC(u,i + 1) .gt. 0 ) then
            !unidad en operacion en i e i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 0
                DispOutput ( u, int + contador ) = 1
                PotMaxGRC_1 ( u, int + contador ) = PotMaxGRC ( u, i )
                PotMinGRC_1 ( u, int + contador ) = PotMinGRC ( u, i )
                !Manipular limites de generacion si esta en sincronizacion en i
                if ( RESMODO ( u, i ) .eq. 11 ) then
                    cambio = ( RampArraURC ( u ) / durintr ) * DurIntAUTR
                    PotMinGRC_1 ( u, int + contador ) = GENUNRC(u,i) + contador * cambio
                    PotMaxGRC_1 ( u, int + contador ) = PotMinGRC_1 ( u, int + contador )
                end if
                if ( i .eq. ntintr - 1 ) then                
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 0
                    DispOutput ( u, int + contador + NoIntAUTR ) = 1
                    PotMaxGRC_1 ( u, int + contador + NoIntAUTR ) = PotMaxGRC ( u, i + 1 )
                    PotMinGRC_1 ( u, int + contador + NoIntAUTR ) = PotMinGRC ( u, i + 1 )
                    !Manipular limites de generacion si esta en sincronizacion en i
                    if ( RESMODO ( u, i + 1 ) .eq. 11 ) then
                        cambio = ( RampArraURC ( u ) / durintr ) * DurIntAUTR
                        PotMinGRC_1 ( u, int + contador + NoIntAUTR ) = GENUNRC( u, i + 1 ) + contador * cambio
                        PotMaxGRC_1 ( u, int + contador + NoIntAUTR ) = PotMinGRC_1 ( u, int + contador + NoIntAUTR )
                    end if
                end if
            end do
        end if
        if ( GENUNRC(u,i) .eq. 0 .and. GENUNRC(u,i + 1) .eq. 0 ) then
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                !unidad apagada en i e i + 1
                AsigOutput ( u, int + contador ) = 0
                DispOutput ( u, int + contador ) = 0
                PotMaxGRC_1 ( u, int + contador ) = PotMaxGRC ( u, i )
                PotMinGRC_1 ( u, int + contador ) = PotMinGRC ( u, i )
                if ( i .eq. ntintr - 1 ) then
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 0
                    DispOutput ( u, int + contador + NoIntAUTR ) = 0
                    PotMaxGRC_1 ( u, int + contador + NoIntAUTR ) = PotMaxGRC ( u, i + 1 )
                    PotMinGRC_1 ( u, int + contador + NoIntAUTR ) = PotMinGRC ( u, i + 1 )
                end if
            end do
        end if
        if (  ( ( GENUNRC(u,i) .eq. 0 .and. GENUNRC(u,i + 1) .gt.  0 ) .or. &
             ( GENUNRC(u,i) .gt. 0 .and. GENUNRC(u,i + 1) .eq.  0 ) )   ) then
            !unidad apagada/encendida en i y encendida/apagada en i + 1
            !Hacer para los intervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
!               si la unidad es disponible y asignable
                if ( DispoURC ( u, i ) .gt. 0  ) then
                    DispOutput ( u, int + contador ) = 1
                    if ( AsignURC ( u, i ) .gt. 0 ) then
                        AsigOutput ( u, int + contador ) = 1
                    endif
                endif
                PotMaxGRC_1 ( u, int + contador ) = PotMaxGRC ( u, i )
                PotMinGRC_1 ( u, int + contador ) = PotMinGRC ( u, i )
                if ( i .eq. ntintr - 1 ) then
                    if ( DispoURC ( u, i+1 ) .gt. 0  ) then
                        DispOutput ( u, int + contador + NoIntAUTR ) = 1
                        if ( AsignURC ( u, i+1 ) .gt. 0 ) then
                            AsigOutput ( u, int + contador + NoIntAUTR ) = 1
                        endif
                    endif
                    PotMaxGRC_1 ( u, int + contador + NoIntAUTR ) = PotMaxGRC ( u, i + 1 )
                    PotMinGRC_1 ( u, int + contador + NoIntAUTR ) = PotMinGRC ( u, i + 1 )
                end if
                !Si la unidad se soncroniza
                if ( RESMODO ( u, i + 1 ) .eq. 11 ) then
                    AsigOutput ( u, int + contador ) = 0
                    DispOutput ( u, int + contador ) = 0
                    cambio = ( RampArraURC ( u ) / durintr ) * DurIntAUTR
                    PotMinGRC_1 ( u, int + contador + NoIntAUTR ) = GENUNRC( u, i + 1 ) + contador * cambio
                    PotMaxGRC_1 ( u, int + contador + NoIntAUTR ) = PotMinGRC_1 ( u, int + contador + NoIntAUTR )
                end if
            end do
        end if
        !reservas no rodantes  
        if ( ResNR10URC(u,i) .gt. 0 .or. ResNRSUURC(u,i) .gt. 0 ) then
            !unidad en reserva no rodante en i
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 1
                DispOutput ( u, int + contador ) = 1
                PotMaxGRC_1 ( u, int + contador ) = PotMaxGRC ( u, i )
                PotMinGRC_1 ( u, int + contador ) = PotMinGRC ( u, i )
            end do
        end if
    enddo
    if ( ResNR10URC(u,NTINTR) .gt. 0 .or. ResNRSUURC(u,NTINTR) .gt. 0 ) then
        !unidad en reserva no rodante en i
        !Hacer para losintervalos de x min de la hora
        int = ( NTINTR * NoIntAUTR ) - ( NoIntAUTR - 1 )
        do contador = 0, NoIntAUTR - 1
            AsigOutput ( u, int + contador ) = 1
            DispOutput ( u, int + contador ) = 1
            PotMaxGRC_1 ( u, int + contador ) = PotMaxGRC ( u, NTINTR )
            PotMinGRC_1 ( u, int + contador ) = PotMinGRC ( u, NTINTR )
        end do
    end if
enddo


!Escribe archivo de asignabilidad de salida ASIGN_AUGC_RC.csv
!Escribe archivo de asignabilidad de salida DISPO_AUGC_RC.csv

consecutivo = 1

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_RC.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 313, FILE = trim(rut_dat_1)//'LSUNIT_AUGC_RC.csv',IOSTAT = IERROR_2,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 314, FILE = trim(rut_dat_1)//'LIUNIT_AUGC_RC.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

letaux = 'letaux'
letaux_1 = 'letaux_1'
letaux_2 = 'letaux_2'
letaux_3 = 'letaux_3'

if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 .and. ierror_3 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 &
               .and. ierror_2 .eq. 0 .and. len_trim(letaux_2) .ne. 0 .and. ierror_3 .eq. 0 .and. len_trim(letaux_3) .ne. 0 &
               .and. consecutivo .le. SisUniRC )        
!	     read ( 311, 100, iostat = ierror ) letaux
!        read ( 312, 100, iostat = ierror_1 ) letaux_1
!        read ( 313, 100, iostat = ierror_2 ) letaux_2
!        read ( 314, 100, iostat = ierror_3 ) letaux_3        
        if ( ierror + ierror_1 + ierror_2 + ierror_3 .ne. 0 ) then 
            !Si al menos falta una archivo se ignoran todos
            ierror = - 1 
            ierror_1 = - 1
            ierror_2 = - 1
            ierror_3 = - 1
        end if
        if ( ierror .ne. 0 .and. ierror_1 .ne. 0 .and. ierror_2 .ne. 0 .and. ierror_3 .ne. 0 ) then
            do u = 1, NumUniRC
                if ( corresprc ( u ) .eq. consecutivo ) then
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    genericoPmax ( consecutivo, : ) = PotMaxGRC_1 ( u, : ) * base
                    genericoPmin ( consecutivo, : ) = PotMinGRC_1 ( u, : ) * base
                    consecutivo = consecutivo + 1
                else
                    do while ( corresprc ( u ) .ne. consecutivo .and. ierror .ne. 0 .and. ierror_1 .ne. 0 .and. ierror_2 .ne. 0 .and. ierror_3 .ne. 0 )
                        genericoA ( consecutivo, : ) = 1
                        genericoD ( consecutivo, : ) = 1
                        genericoPmax ( consecutivo, : ) = 1000.0
                        genericoPmin ( consecutivo, : ) = 0.0
                        consecutivo = consecutivo + 1
                    end do
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    genericoPmax ( consecutivo, : ) = PotMaxGRC_1 ( u, : ) * base
                    genericoPmin ( consecutivo, : ) = PotMinGRC_1 ( u, : ) * base
                    consecutivo = consecutivo + 1 
                end if
            enddo
        else
            !leer informacion
            do u = 1, NumUniRC
                if ( corresprc ( u ) .eq. consecutivo ) then
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    genericoPmax ( consecutivo, : ) = PotMaxGRC_1 ( u, : ) * base
                    genericoPmin ( consecutivo, : ) = PotMinGRC_1 ( u, : ) * base
                    consecutivo = consecutivo + 1
!                    read ( 311, 100, iostat = ierror ) letaux
!                    read ( 312, 100, iostat = ierror_1 ) letaux_1
!                    read ( 313, 100, iostat = ierror_2 ) letaux_2
!                    read ( 314, 100, iostat = ierror_3 ) letaux_3
                else
                    do while ( corresprc ( u ) .ne. consecutivo .and. ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 .and. ierror_3 .eq. 0 )
                        read ( letaux, * ) ( genericoA ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
                        read ( letaux_1, * ) ( genericoD ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
                        read ( letaux_2, * ) ( genericoPmax ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
                        read ( letaux_3, * ) ( genericoPmin ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
                        consecutivo = consecutivo + 1
                        read ( 311, 100, iostat = ierror ) letaux
                        read ( 312, 100, iostat = ierror_1 ) letaux_1
                        read ( 313, 100, iostat = ierror_2 ) letaux_2
                        read ( 314, 100, iostat = ierror_3 ) letaux_3
                    end do
                    if ( corresprc ( u ) .eq. consecutivo ) then
                        genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                        genericoD ( consecutivo, : ) = DispOutput ( u, : )
                        genericoPmax ( consecutivo, : ) = PotMaxGRC_1 ( u, : ) * base
                        genericoPmin ( consecutivo, : ) = PotMinGRC_1 ( u, : ) * base
                        consecutivo = consecutivo + 1 
                    end if                    
                end if
            enddo
        end if
    end do
end if



close ( 311 )
close ( 312 )
close ( 313 )
close ( 314 )

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_RC.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 313, FILE = trim(rut_dat_1)//'LSUNIT_AUGC_RC.csv',IOSTAT = IERROR_2,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 314, FILE = trim(rut_dat_1)//'LIUNIT_AUGC_RC.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_RC.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

CLOSE ( UNIT = 315, status = 'delete' )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_RC.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do u = 1, consecutivo - 1
    write ( 311, 600 ) nombunirc ( u ), ( genericoA ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
    write ( 312, 600 ) nombunirc ( u ), ( genericoD ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
    write ( 313, 601 ) nombunirc ( u ), ( genericoPmax ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
    write ( 314, 601 ) nombunirc ( u ), ( genericoPmin ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
end do

ibanbit = 1
ierror = 0

do u = 1, consecutivo - 1
!   se descartan la unidades virtuales
    if ( nombunirc ( u ) (1:2) .ne. 'I-' ) then
        intervalo = 0
        do i = 1, ntintr
            do int = 1, NoIntAUTR
                intervalo = intervalo + 1
                write ( 315, 700 ) nombunirc ( u ), i, int, genericoA ( u, intervalo ), genericoD ( u, intervalo ), &
    !                               genericoPmin ( u, intervalo ), genericoPmax ( u, intervalo ), GENUNRC ( u, i )*Base
                                   genericoPmin ( u, intervalo ), genericoPmax ( u, intervalo )
            enddo
        enddo
    else
        write ( aaux2, 5102 ) nombunirc ( u )
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'002 UNIDAD VIRTUAL AUHE: '//aaux2
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif
enddo

close ( 311 )
close ( 312 )
close ( 313 )
close ( 314 )
close ( 315 )
5102 FORMAT (A20)

!Para las unidades de RD

AsigOutputRD = 0
DispOutputRD = 0
j = 0
m = 0

!Para las unidades de rango discontinuo
do u = 1, NumUniRD
!   para todos los intervalos de planeacion
    do i = 1, NTINTR - 1
        int = ( i * NoIntAUTR ) - ( NoIntAUTR - 1 )
        if ( RESMODO ( u + NumUniRC, i ) .eq. RESMODO ( u + NumUniRC, i + 1 ) ) then
            !no hay cambio de modo, fijarlo
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                m = RESMODO ( u + NumUniRC, i )
                if ( m .ne. 11 ) then
                    AsigOutputRD ( u, m, int + contador ) = 0
                    DispOutputRD ( u, m, int + contador ) = 1
                    PotMinGRD_1 ( u, m, int + contador ) = PotMinGRD ( u, m, i )
                    PotMaxGRD_1 ( u, m, int + contador ) = PotMaxGRD ( u, m, i )
                !si esta en sincronizacion fijarlo al modo en que arranca
                else
                !if ( m .eq. 11 ) then
                    !hacer hasta el fin de los intervalos
                    j = i
                    do while  ( j .le. ntintr .and. m .eq. 11 )
                        j = j + 1    
                        m = RESMODO ( u + NumUniRC, j )
                    end do
                    AsigOutputRD ( u, m, int + contador ) = 0
                    DispOutputRD ( u, m, int + contador ) = 1
                    cambio = ( RampArraURD ( u, m ) / durintr ) * DurIntAUTR
                    PotMinGRD_1 ( u, m, int + contador ) = GENUNRD(u,i) + contador * cambio
                    PotMaxGRD_1 ( u, m, int + contador ) = PotMinGRD_1 ( u, m, int + contador )
                end if
                if ( i .eq. ntintr - 1 ) then           
                    AsigOutputRD ( u, m, int + contador + NoIntAUTR ) = 0
                    DispOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                    PotMaxGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMaxGRD ( u, m, i + 1 )
                    PotMinGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMinGRD ( u, m, i + 1 )
                    if ( RESMODO ( u + NumUniRC, i + 1 ) .eq. 11 ) then
                        cambio = ( RampArraURD ( m, u ) / durintr ) * DurIntAUTR
                        PotMinGRD_1 ( u, m, int + contador + NoIntAUTR ) = GENUNRD( u, i + 1 ) + contador * cambio
                        PotMaxGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMinGRD_1 ( u, m, int + contador + NoIntAUTR )
                    end if
                end if
            end do
        else
            !Cambio de modo no en sincronizacion
            if ( ( RESMODO ( u + NumUniRC, i ) .ne. 11 ) .and. ( RESMODO ( u + NumUniRC, i + 1 ) .ne. 11 ) ) then                
                !Hacer para losintervalos de x min de la hora
                do contador = 0, NoIntAUTR - 1
                    m = RESMODO ( u + NumUniRC, i )
                    AsigOutputRD ( u, m, int + contador ) = 1
                    DispOutputRD ( u, m, int + contador ) = 1
                    PotMaxGRD_1 ( u, m, int + contador ) = PotMaxGRD ( u, m, i )
                    PotMinGRD_1 ( u, m, int + contador ) = PotMinGRD ( u, m, i )
                    if ( i .eq. ntintr - 1 ) then
                        AsigOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                        DispOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                        PotMaxGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMaxGRD ( u, m, i + 1 )
                        PotMinGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMinGRD ( u, m, i + 1 )
                    end if
                    m = RESMODO ( u + NumUniRC, i + 1 )
                    AsigOutputRD ( u, m, int + contador ) = 1
                    DispOutputRD ( u, m, int + contador ) = 1
                    PotMaxGRD_1 ( u, m, int + contador ) = PotMaxGRD ( u, m, i )
                    PotMinGRD_1 ( u, m, int + contador ) = PotMinGRD ( u, m, i )
                    if ( i .eq. ntintr - 1 ) then
                        AsigOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                        DispOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                        PotMaxGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMaxGRD ( u, m, i + 1 )
                        PotMinGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMinGRD ( u, m, i + 1 )
                    end if
                end do
            end if
            if ( RESMODO ( u + NumUniRC, i ) .eq. 1 .and. RESMODO ( u + NumUniRC, i + 1 ) .eq. 11 ) then
                !de apagado a sincronizacion
                !Hacer para losintervalos de x min de la hora
                do contador = 0, NoIntAUTR - 1
                    !hacer hasta el fin de los intervalos
                    j = i
                    m = 11
                    do while  ( j .le. ntintr .and. m .eq. 11 )
                        j = j + 1    
                        m = RESMODO ( u + NumUniRC, j )
                    end do
                    AsigOutputRD ( u, 1, int + contador ) = 0
                    DispOutputRD ( u, 1, int + contador ) = 1
                    PotMinGRD_1 ( u, 1, int + contador ) = PotMinGRD ( u, 1, i )
                    PotMaxGRD_1 ( u, 1, int + contador ) = PotMaxGRD ( u, 1, i )
                    if ( i .eq. ntintr - 1 ) then      
                        AsigOutputRD ( u, m, int + contador + NoIntAUTR ) = 0
                        DispOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                        cambio = ( RampArraURD ( m, u ) / durintr ) * DurIntAUTR
                        PotMinGRD_1 ( u, m, int + contador + NoIntAUTR ) = GENUNRD( u, i + 1 ) + contador * cambio
                        PotMaxGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMinGRD_1 ( u, m, int + contador + NoIntAUTR )
                    end if
                end do
            end if 
            if ( RESMODO ( u + NumUniRC, i ) .eq. 11 .and. RESMODO ( u + NumUniRC, i + 1 ) .gt. 1 ) then
                !Hacer para losintervalos de x min de la hora
                do contador = 0, NoIntAUTR - 1
                    j = i
                    m = 11
                    do while  ( j .le. ntintr .and. m .eq. 11 )
                        j = j + 1    
                        m = RESMODO ( u + NumUniRC, j )
                    end do
                    AsigOutputRD ( u, m, int + contador ) = 0
                    DispOutputRD ( u, m, int + contador ) = 1                   
                    cambio = ( RampArraURD ( u, m ) / durintr ) * DurIntAUTR
                    PotMinGRD_1 ( u, m, int + contador ) = GENUNRD(u,i) + contador * cambio
                    PotMaxGRD_1 ( u, m, int + contador ) = PotMinGRD_1 ( u, m, int + contador )
                    if ( i .eq. ntintr - 1 ) then      
                        AsigOutputRD ( u, m, int + contador + NoIntAUTR ) = 0
                        DispOutputRD ( u, m, int + contador + NoIntAUTR ) = 1
                        PotMaxGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMaxGRD ( u, m, i + 1 )
                        PotMinGRD_1 ( u, m, int + contador + NoIntAUTR ) = PotMinGRD ( u, m, i + 1 )
                    end if
                end do
            end if
        end if
        !reservas no rodantes  
        !Hacer para todos los modos
        do m = 1, NumModRD ( u )        
            if ( ResNR10URD ( u, m, i ) .gt. 0 .or. ResNRSUURd ( u, m, i ) .gt. 0 ) then
                !Hacer para losintervalos de x min de la hora
                do contador = 0, NoIntAUTR - 1
                    !unidad en reserva no rodante en i
                    AsigOutputRD ( u, m, int + contador ) = 1
                    DispOutputRD ( u, m, int + contador ) = 1
                    PotMaxGRD_1 ( u, m, int + contador ) = PotMaxGRD ( u, m, i )
                    PotMinGRD_1 ( u, m, int + contador ) = PotMinGRD ( u, m, i )
                    !Modo apagado disponible
                    AsigOutputRD ( u, 1, int + contador ) = 1
                    DispOutputRD ( u, 1, int + contador ) = 1
                    PotMaxGRD_1 ( u, 1, int + contador ) = PotMaxGRD ( u, 1, i )
                    PotMinGRD_1 ( u, 1, int + contador ) = PotMinGRD ( u, 1, i )
                end do
            end if
        enddo
    end do
    !Hacer para todos los modos
    do m = 1, NumModRD ( u )       
        if ( ResNR10URD ( u, m, NTINTR ) .gt. 0 .or. ResNRSUURd ( u, m, NTINTR ) .gt. 0 ) then
            !Hacer para losintervalos de x min de la hora
            int = ( NTINTR * NoIntAUTR ) - ( NoIntAUTR - 1 )
            do contador = 0, NoIntAUTR - 1
                !unidad en reserva no rodante en i
                AsigOutputRD ( u, m, int + contador ) = 1
                DispOutputRD ( u, m, int + contador ) = 1
                PotMaxGRD_1 ( u, m, int + contador ) = PotMaxGRD ( u, m, NTINTR )
                PotMinGRD_1 ( u, m, int + contador ) = PotMinGRD ( u, m, NTINTR )
                !Modo apagado disponible
                AsigOutputRD ( u, 1, int + contador ) = 1
                DispOutputRD ( u, 1, int + contador ) = 1
                PotMaxGRD_1 ( u, 1, int + contador ) = PotMaxGRD ( u, 1, NTINTR )
                PotMinGRD_1 ( u, 1, int + contador ) = PotMinGRD ( u, 1, NTINTR )
            end do
        end if
    end do
enddo

!Escribe archivo de asignabilidad de salida ASIGN_AUGC_RD.csv
!Escribe archivo de asignabilidad de salida DISPO_AUGC_RD.csv

consecutivo = 1

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_RD.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 313, FILE = trim(rut_dat_1)//'LSUNIT_AUGC_RD.csv',IOSTAT = IERROR_2,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 314, FILE = trim(rut_dat_1)//'LIUNIT_AUGC_RD.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

!Si no hay unidades de rango discontinuo
if ( NumUniRD .eq. 0 ) then
    ierror = 1
    ierror_1 = 1
    ierror_2 = 1
    ierror_3 = 1
end if

letaux = 'letaux'
letaux_1 = 'letaux_1'
letaux_2 = 'letaux_2'
letaux_3 = 'letaux_3'
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 .and. ierror_3 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 &
               .and. ierror_2 .eq. 0 .and. len_trim(letaux_2) .ne. 0 .and. ierror_3 .eq. 0 .and. len_trim(letaux_3) .ne. 0 &
               .and. consecutivo .le. SisUniRD )        
!	    read ( 311, 100, iostat = ierror ) letaux
!        read ( 312, 100, iostat = ierror_1 ) letaux_1
!        read ( 313, 100, iostat = ierror_2 ) letaux_2
!        read ( 314, 100, iostat = ierror_3 ) letaux_3        
        if ( ierror + ierror_1 + ierror_2 + ierror_3 .ne. 0 ) then 
            !Si al menos falta una archivo se ignoran todos
            ierror = - 1 
            ierror_1 = - 1
            ierror_2 = - 1
            ierror_3 = - 1
        end if
        if ( ierror .ne. 0 .and. ierror_1 .ne. 0 .and. ierror_2 .ne. 0 .and. ierror_3 .ne. 0 ) then
            do u = 1, NumUniRD
                if ( corresprd ( u ) .eq. consecutivo ) then
                    !hacer para el maximo de moodos
                    do m = 1, maxmodos 
                        genericoARD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                        genericoDRD ( consecutivo, m, : ) = DispOutputRD ( u, m, : )
                        genericoPmaxRD ( consecutivo, m, : ) = PotMaxGRD_1 ( u, m, : ) * base
                        genericoPminRD ( consecutivo, m, : ) = PotMinGRD_1 ( u, m, : ) * base
                    end do
                    consecutivo = consecutivo + 1
                else
                    do while ( corresprd ( u ) .ne. consecutivo .and. ierror .ne. 0 .and. ierror_1 .ne. 0 .and. ierror_2 .ne. 0 .and. ierror_3 .ne. 0 )
                        !hacer para el maximo de moodos
                        do m = 1, maxmodos 
                            genericoARD ( consecutivo, m, : ) = 1   
                            genericoDRD ( consecutivo, m, : ) = 1
                            genericoPmaxRD ( consecutivo, m, : ) = 1000.0
                            genericoPminRD ( consecutivo, m, : ) = 0.0
                        end do
                        consecutivo = consecutivo + 1
                    end do
                    do m = 1, maxmodos 
                        genericoARD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                        genericoDRD ( consecutivo, m, : ) = DispOutputRD ( u, m, : )
                        genericoPmaxRD ( consecutivo, m, : ) = PotMaxGRD_1 ( u, m, : ) * base
                        genericoPminRD ( consecutivo, m, : ) = PotMinGRD_1 ( u, m, : ) * base
                    end do 
                    consecutivo = consecutivo + 1 
                end if
            enddo
        else
            !leer informacion
            do u = 1, NumUniRD
                if ( corresprd ( u ) .eq. consecutivo ) then
                    !hacer para el maximo de moodos
                    do m = 1, maxmodos 
                        genericoARD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                        genericoDRD ( consecutivo, m, : ) = DispOutputRD ( u, m, : )
                        genericoPmaxRD ( consecutivo, m, : ) = PotMaxGRD_1 ( u, m, : ) * base
                        genericoPminRD ( consecutivo, m, : ) = PotMinGRD_1 ( u, m, : ) * base
!                        read ( 311, 100, iostat = ierror ) letaux        
!                        read ( 312, 100, iostat = ierror_1 ) letaux_1
!                        read ( 313, 100, iostat = ierror_2 ) letaux_2
!                        read ( 314, 100, iostat = ierror_3 ) letaux_3
                    end do
                    consecutivo = consecutivo + 1
                else
                    do while ( corresprd ( u ) .ne. consecutivo .and. ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 .and. ierror_3 .eq. 0 )
                        !Hacer para el  maximo de modos
                        do m = 1, maxmodos 
                            read ( letaux, * ) ( genericoARD ( consecutivo, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
                            read ( letaux_1, * ) ( genericoDRD ( consecutivo, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
                            read ( letaux_2, * ) ( genericoPmaxRD ( consecutivo, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
                            read ( letaux_3, * ) ( genericoPminRD ( consecutivo, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
!                            read ( 311, 100, iostat = ierror ) letaux
!                            read ( 312, 100, iostat = ierror_1 ) letaux_1
!                            read ( 313, 100, iostat = ierror_2 ) letaux_2
!                            read ( 314, 100, iostat = ierror_3 ) letaux_3
                        end do
                        consecutivo = consecutivo + 1
                    end do
                    if ( corresprd ( u )  .eq. consecutivo ) then
                        do m = 1, maxmodos 
                            genericoARD ( consecutivo, m, : ) = AsigOutputRD ( u, m, : )
                            genericoDRD ( consecutivo, m, : ) = DispOutputRD ( u, m, : )
                            genericoPmaxRD ( consecutivo, m, : ) = PotMaxGRD_1 ( u, m, : ) * base
                            genericoPminRD ( consecutivo, m, : ) = PotMinGRD_1 ( u, m, : ) * base
                            if ( m .lt. maxmodos ) then
!                                read ( 311, 100, iostat = ierror ) letaux
!                                read ( 312, 100, iostat = ierror_1 ) letaux_1
!                                read ( 313, 100, iostat = ierror_2 ) letaux_2
!                                read ( 314, 100, iostat = ierror_3 ) letaux_3
                            end if                            
                        end do
                        consecutivo = consecutivo + 1 
                    end if                    
                end if
            enddo
        end if
    end do
end if



close ( 311 )
close ( 312 )
close ( 313 )
close ( 314 )

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_RD.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 313, FILE = trim(rut_dat_1)//'LSUNIT_AUGC_RD.csv',IOSTAT = IERROR_2,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 314, FILE = trim(rut_dat_1)//'LIUNIT_AUGC_RD.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_RD.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

CLOSE ( UNIT = 315, status = 'delete' )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_RD.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do u = 1, consecutivo - 1
    do m = 1, maxmodos 
        write ( 311, 600 ) nombunird ( u ), ( genericoARD ( u, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
        write ( 312, 600 ) nombunird ( u ), ( genericoDRD ( u, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
        write ( 313, 601 ) nombunird ( u ), ( genericoPmaxRD ( u, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
        write ( 314, 601 ) nombunird ( u ), ( genericoPminRD ( u, m, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
    end do    
end do

do u = 1, consecutivo - 1
    do m = 1, maxmodos 
        intervalo = 0
        do i = 1, ntintr
            do int = 1, NoIntAUTR
                intervalo = intervalo + 1
                write ( 315, 701 ) nombunird ( u ), i, int, m, genericoARD ( u, m, intervalo ), genericoDRD ( u, m, intervalo ), &
!                                   genericoPminRD ( u, m, intervalo ), genericoPmaxRD ( u, m, intervalo ), GENUNRD ( u, i )*Base
                                   genericoPminRD ( u, m, intervalo ), genericoPmaxRD ( u, m, intervalo )
            enddo
        enddo
    enddo
enddo

close ( 311 )
close ( 312 )
close ( 313 )
close ( 314 )
close ( 315 )

EnerTermo = 0.0
!Produccion de energia por grupo de energia termo
do grupo = 1, NumGruUTer
!   para las unidades de rango continuo que estan en ese grupo
    iniciou = ApunURCxGrupo ( grupo )
    do unidad = 1 , NumURCxGrupo ( grupo )
        u = UniRCxGrupo ( iniciou )
!       para todos los intervalos
        do intervalo = 1, NTINTR
            EnerTermo ( grupo, intervalo ) = EnerTermo ( grupo, intervalo ) + GENUNRC ( u, intervalo )
        enddo
        iniciou = iniciou + 1
    enddo
!   para las unidades de rango discontinuo que estan en ese grupo
    iniciou = ApunURDxGrupo ( grupo )
    do unidad = 1 , NumURDxGrupo ( grupo )
        u = UniRDxGrupo ( iniciou )
!       para todos los intervalos
        do intervalo = 1, NTINTR
            EnerTermo ( grupo, intervalo ) = EnerTermo ( grupo, intervalo ) + GENUNRD ( u, intervalo ) 
        enddo
        iniciou = iniciou + 1
    enddo
end do

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'GPOUTER_AUGC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do grupo = 1, NumGruUTer
    !Se multiplica por cuatro porque hay cuatro intervalos quinceminutales en una hora
    write ( 311, 601 ) ( EnerTermo ( grupo, intervalo ) * base* 4, intervalo = 1 , ntintr  ) 
end do

close ( 311 )


AsigOutput = 0
DispOutput = 0
!Por default sin es de apagado en i a apagado en i - 1 no se hace nada porque esta inicializado en ND, NA
!Hacer para todas las unidades hidro
do u = 1, NumUniHid
!   para todos los intervalos de planeacion
    do i = 1, NTINTR - 1
        int = ( i * NoIntAUTR ) - ( NoIntAUTR - 1 )
        if ( GENUNH(u,i) .gt. 0 .and. GENUNH(u,i + 1) .gt. 0 ) then
            !unidad en operacion en i e i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador) = 0
                DispOutput ( u, int + contador ) = 1
                if ( i .eq. ntintr - 1 ) then                
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 0
                    DispOutput ( u, int + contador + NoIntAUTR ) = 1
                end if
            end do
        end if
        if ( GENUNH(u,i) .eq. 0 .and. GENUNH(u,i + 1) .eq. 0 ) then
            !unidad apagada en i e i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 0
                DispOutput ( u, int + contador ) = 0
                if ( i .eq. ntintr - 1 ) then
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 0
                    DispOutput ( u, int + contador + NoIntAUTR ) = 0
                end if
            end do
        end if
        if ( ( ( GENUNH(u,i) .eq. 0 .and. GENUNH(u,i + 1) .gt.  0 ) .or. &
             ( GENUNH(u,i) .gt. 0 .and. GENUNH(u,i + 1) .eq.  0 ) )   ) then
            !unidad apagada/encendida en i y encendida/apagada en i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                if ( DispoUH ( u, i ) .gt. 0 ) then
                   DispOutput ( u, int + contador ) = 1
                   if ( ASIGNUH (u,i) .gt. 0) then
                       AsigOutput ( u, int + contador ) = 1
                   endif
                endif
                if ( i .eq. ntintr - 1 ) then
                    if ( DispoUH ( u, i+1 ) .gt. 0 ) then
                       DispOutput ( u, int + contador + NoIntAUTR ) = 1
                       if ( ASIGNUH (u,i+1) .gt. 0) then
                           AsigOutput ( u, int + contador + NoIntAUTR ) = 1
                       endif
                    endif
                end if
            end do
        end if
        !reservas no rodantes  
        if ( ResNR10UH(u,i) .gt. 0 .or. ResNRSUUH(u,i) .gt. 0 ) then
            !unidad en reserva no rodante en i
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 1
                DispOutput ( u, int + contador ) = 1
            end do
        end if
    enddo
    if ( ResNR10UH(u,NTINTR) .gt. 0 .or. ResNRSUUH(u,NTINTR) .gt. 0 ) then
        !unidad en reserva no rodante en i
        !Hacer para losintervalos de x min de la hora
        int = ( NTINTR * NoIntAUTR ) - ( NoIntAUTR - 1 )
        do contador = 0, NoIntAUTR - 1
            AsigOutput ( u, int + contador ) = 1
            DispOutput ( u, int + contador ) = 1
        end do
    end if
enddo


!Escribe archivo de asignabilidad de salida ASIGN_AUGC_H.csv
!Escribe archivo de asignabilidad de salida DISPO_AUGC_H.csv

consecutivo = 1

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_H.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_H.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

letaux = 'letaux'
letaux_1 = 'letaux_1'

if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 &               
               .and. consecutivo .le. SisUniH )        
!	    read ( 311, 100, iostat = ierror ) letaux
!        read ( 312, 100, iostat = ierror_1 ) letaux_1
        if ( ierror + ierror_1 .ne. 0 ) then 
            !Si al menos falta una archivo se ignoran todos
            ierror = - 1 
            ierror_1 = - 1
        end if
        if ( ierror .ne. 0 .and. ierror_1 .ne. 0 ) then
            do u = 1, NumUniHid
                if ( corresph ( u ) .eq. consecutivo ) then
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    consecutivo = consecutivo + 1
                else
                    do while ( corresph ( u ) .ne. consecutivo .and. ierror .ne. 0 .and. ierror_1 .ne. 0 )
                        genericoA ( consecutivo, : ) = 1
                        genericoD ( consecutivo, : ) = 1
                        consecutivo = consecutivo + 1
                    end do
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    consecutivo = consecutivo + 1 
                end if
            enddo
        else
            !leer informacion
            do u = 1, NumUniHid
                if ( corresph ( u ) .eq. consecutivo ) then
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    consecutivo = consecutivo + 1
!                    read ( 311, 100, iostat = ierror ) letaux
!                    read ( 312, 100, iostat = ierror_1 ) letaux_1
                else
                    do while ( corresph ( u ) .ne. consecutivo .and. ierror .eq. 0 .and. ierror_1 .eq. 0 )
                        read ( letaux, * ) ( genericoA ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
                        read ( letaux_1, * ) ( genericoD ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
                        consecutivo = consecutivo + 1
!                        read ( 311, 100, iostat = ierror ) letaux
!                        read ( 312, 100, iostat = ierror_1 ) letaux_1
                    end do
                    if ( corresph ( u ) .eq. consecutivo ) then
                        genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                        genericoD ( consecutivo, : ) = DispOutput ( u, : )
                        consecutivo = consecutivo + 1 
                    end if                    
                end if
            enddo
        end if
    end do
end if



close ( 311 )
close ( 312 )

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_H.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_H.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_H.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

CLOSE ( UNIT = 315, status = 'delete' )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_H.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do u = 1, consecutivo - 1
    write ( 311, 600 ) nombunih ( u ), ( genericoA ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
    write ( 312, 600 ) nombunih ( u ), ( genericoD ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR ) 
end do

do u = 1, consecutivo - 1
    intervalo = 0
    do i = 1, ntintr
        do int = 1, NoIntAUTR
            intervalo = intervalo + 1
            write ( 315, 702 ) nombunih ( u ), i, int, genericoA ( u, intervalo ), genericoD ( u, intervalo )
        enddo
    enddo
enddo

close ( 311 )
close ( 312 )
close ( 315 )

!Produccion de energia hidro por hora y por embalse 
EnerHidro = 0.0
do embalse = 1, NumEmbalses
    do i = 0, NumCenHEmb ( embalse ) - 1
        planta = ListaPlantasH ( ApunPlantaEmbalse ( embalse ) + i )
!       para todas las unidades de la panta
        do j = 0, NOUN ( planta ) - 1
            u = ListaUnidadesH ( ApunUnidadPlanta ( planta ) + j )
!           para todos los intervalos
            do intervalo = 1, NTINTR
!               generacion de la unidad
                EnerHidro ( embalse, intervalo ) = EnerHidro ( embalse, intervalo ) + GENUNH ( u, intervalo ) 
            enddo
        enddo
    enddo
end do

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'LIMENEREMB_AUGC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do embalse = 1, NumEmbalses
    !Se multiplica por cuatro porque hay cuatro intervalos quinceminutales en una hora
    write ( 311, 601 ) nomemb ( embalse ), ( EnerHidro ( embalse, intervalo ) * base/1000.0, intervalo = 1 , ntintr  ) 
end do

close ( 311 )

AsigOutput = 0
DispOutput = 0
!Por default sin es de apagado en i a apagado en i - 1 no se hace nada porque esta inicializado en ND, NA
!Hacer para todas las unidades renovables
do u = 1, NumUniRE
!   para todos los intervalos de planeacion
    do i = 1, NTINTR - 1
        int = ( i * NoIntAUTR ) - ( NoIntAUTR - 1 )
        if ( GENUNRE(u,i) .gt. 0 .and. GENUNRE(u,i + 1) .gt. 0 ) then
            !unidad en operacion en i e i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 0
                DispOutput ( u, int + contador ) = 1
                if ( i .eq. ntintr - 1 ) then                
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 0
                    DispOutput ( u, int + contador + NoIntAUTR ) = 1
                end if
            end do
        end if
        if ( GENUNRE(u,i) .eq. 0 .and. GENUNRE(u,i + 1) .eq. 0 ) then
            !unidad apagada en i e i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 0
                DispOutput ( u, int + contador ) = 0
                if ( i .eq. ntintr - 1 ) then
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 0
                    DispOutput ( u, int + contador + NoIntAUTR ) = 0
                end if
            end do
        end if
        if ( ( GENUNRE(u,i) .eq. 0 .and. GENUNRE(u,i + 1) .gt.  0 ) .or. &
             ( GENUNRE(u,i) .gt. 0 .and. GENUNRE(u,i + 1) .eq.  0 ) ) then
            !unidad apagada/encendida en i y encendida/apagada en i + 1
            !Hacer para losintervalos de x min de la hora
            do contador = 0, NoIntAUTR - 1
                AsigOutput ( u, int + contador ) = 1
                DispOutput ( u, int + contador ) = 1
                if ( i .eq. ntintr - 1 ) then
                    AsigOutput ( u, int + contador + NoIntAUTR ) = 1
                    DispOutput ( u, int + contador + NoIntAUTR ) = 1
                end if
            end do
        end if
    enddo
enddo


!Escribe archivo de asignabilidad de salida ASIGN_AUGC_RE.csv
!Escribe archivo de asignabilidad de salida DISPO_AUGC_RE.csv

consecutivo = 1

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RE.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_RE.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

letaux = 'letaux'
letaux_1 = 'letaux_1'
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 &               
               .and. consecutivo .le. SisUniRE )        
!	    read ( 311, 100, iostat = ierror ) letaux
!        read ( 312, 100, iostat = ierror_1 ) letaux_1
        if ( ierror + ierror_1 .ne. 0 ) then 
            !Si al menos falta una archivo se ignoran todos
            ierror = - 1 
            ierror_1 = - 1
        end if
        if ( ierror .ne. 0 .and. ierror_1 .ne. 0 ) then
            do u = 1, NumUniRE
                if ( correspre ( u ) .eq. consecutivo ) then
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    consecutivo = consecutivo + 1
                else
                    do while ( correspre ( u ) .ne. consecutivo .and. ierror .ne. 0 .and. ierror_1 .ne. 0 )
                        genericoA ( consecutivo, : ) = 1
                        genericoD ( consecutivo, : ) = 1
                        consecutivo = consecutivo + 1
                    end do
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    consecutivo = consecutivo + 1 
                end if
            enddo
        else
            !leer informacion
            do u = 1, NumUniRE
                if ( correspre ( u ) .eq. consecutivo ) then
                    genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                    genericoD ( consecutivo, : ) = DispOutput ( u, : )
                    consecutivo = consecutivo + 1
!                    read ( 311, 100, iostat = ierror ) letaux
!                    read ( 312, 100, iostat = ierror_1 ) letaux_1
                else
                    do while ( correspre ( u ) .ne. consecutivo .and. ierror .eq. 0 .and. ierror_1 .eq. 0 )
                        read ( letaux, * ) ( genericoA ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
                        read ( letaux_1, * ) ( genericoD ( consecutivo, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
                        consecutivo = consecutivo + 1
!                        read ( 311, 100, iostat = ierror ) letaux
!                        read ( 312, 100, iostat = ierror_1 ) letaux_1
                    end do
                    if ( correspre ( u ) .eq. consecutivo ) then
                        genericoA ( consecutivo, : ) = AsigOutput ( u, : )
                        genericoD ( consecutivo, : ) = DispOutput ( u, : )
                        consecutivo = consecutivo + 1 
                    end if                    
                end if
            enddo
        end if
    end do
end if



close ( 311 )
close ( 312 )

OPEN ( UNIT = 311, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RE.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 312, FILE = trim(rut_dat_1)//'DISPO_AUGC_RE.csv',IOSTAT = IERROR_1,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_RE.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

CLOSE ( UNIT = 315, status = 'delete' )
OPEN ( UNIT = 315, FILE = trim(rut_dat_1)//'ResAUGC_RE.csv',IOSTAT = IERROR_3,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

do u = 1, consecutivo - 1
    write ( 311, 600 ) nombunire ( u ), ( genericoA ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
    write ( 312, 600 ) nombunire ( u ), ( genericoD ( u, intervalo ), intervalo = 1 , ntintr * NoIntAUTR  ) 
end do

do u = 1, consecutivo - 1
    intervalo = 0
    do i = 1, ntintr
        do int = 1, NoIntAUTR
            intervalo = intervalo + 1
            write ( 315, 702 ) nombunire ( u ), i, int, genericoA ( u, intervalo ), genericoD ( u, intervalo )
        enddo
    enddo
enddo

close ( 311 )
close ( 312 )
close ( 315 )


100  format ( a )
600  format ( a20,',',169(I,',') )
601  format ( a20,',',169(f10.4,',') )
!700  format ( a20,',', 2(I3,','), 2(I,','), 3(f10.2,',') )
700  format ( a20,',', 2(I3,','), 2(I,','), 2(f10.2,',') )
!701  format ( a20,',', 3(I3,','), 2(I,','), 3(f10.2,',') )
701  format ( a20,',', 3(I3,','), 2(I,','), 2(f10.2,',') )
702  format ( a20,',', 2(I3,','), 2(I,',') )

end subroutine Asig_Out_AUGC

subroutine ImprimeCurvaCostosURC
! ---------------------------------------------------------------------
! Imprime la curva de costos de unidades de rango continuo            *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2020                                                     *
! ---------------------------------------------------------------------
use ParAUHE, only: CostoMinGRC, NumUniRC, NTINTR, NumBloVRC, PotMinGRC, OferVenEnerRC, maxsegrc, base, rut_dat_1

Implicit none

integer u, i, s, ierror


! Imprime oferta de venta de energía de la unidad de rango continuo
OPEN ( UNIT = 10, FILE = trim(rut_dat_1)//'POTVERC_DERS_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
do u = 1, numunirc
    do s = 1, maxsegrc
         write ( 10, 100 )  ( base*OferVenEnerRC ( u, s, i ), i = 1, ntintr )
    enddo
enddo

close ( 10 )

! Imprime costo de operación a potencia minima de la unidad de rango continuo
OPEN ( UNIT = 11, FILE = trim(rut_dat_1)//'CGMRC_DERS_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
do u = 1, numunirc
   write ( 11, 200 )  ( CostoMinGRC ( u,  i ), i = 1, ntintr )
enddo

close ( 11 )

100 format ( 169 (f9.3,",") )
200 format ( 169 (f15.2,",") )
    
return
end