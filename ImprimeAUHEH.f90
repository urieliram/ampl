
! ---------------------------------------------------------------------
! Imprime resultados de asignaciones, despachos y variables duales    *
! para unidades hidro.                                                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2017                                                      *
! ---------------------------------------------------------------------
Subroutine AsigDesH

use ParAUHE
use ProblemaAUHE

Implicit none

INTEGER   i, k, bloque, u

Real*8   arranque ( maxurc, maxint )


! Escribe solucion de asignacion y despacho

write (UnichauH,*) 'Generacion de unidades hidro'
write (UnichauH,*)

! Escribe solucion de generacion de unidades de rango continuo
do u = 1, NumUniHid
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque
       write ( UnichauH, 600 ) u, nombunih(u), ( GENUNH(u,i)*Base, i=k*24-23,k*24 )
    enddo
    do i = 1, NTINTR
        if ( xMILP ( IAH + u + (i-1)*NumUniHid - 1 ) .gt. 0.9 .and. DispoUH ( u , i ) .eq. 1 .and. CompSincH (u, i ) .eq. 0 ) then
            RESMODO ( u+NumUniRC+NumUniRD, i ) = 1
        endif
        if ( xMILP ( IAH + u + (i-1)*NumUniHid - 1 ) .gt. 0.9 .and. DispoUH ( u , i ) .eq. 1 .and. CompSincH (u, i ) .eq. 2 ) then
            if ( xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) .gt. 0.0  .or. xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) .gt. 0.0 ) then
                RESMODO ( u+NumUniRC+NumUniRD, i ) = 1
            endif
        endif
    enddo
enddo

write (UnichauH,*)
write (UnichauH,*) 'Arranque de unidades hidro'
write (UnichauH,*)

! Escribe solucion de arranque de unidades hidro
do u = 1, NumUniHid
    do i = 1, NTINTR
        arranque ( u, i ) = xMILP ( IARH + u + (i-1)*NumUniHid - 1 )
    enddo
!   para todos los intervalos de planeacion
    bloque = NTINTR/24
    do k = 1, bloque
       write ( UnichauH, 600 ) u, nombunih(u), ( arranque(u,i), i=k*24-23,k*24 )
    enddo
enddo

!close ( UnichauH )

600 format ( i4, x, a15, 169(f9.2) )

return
end
    

    
Subroutine ResZonUniH
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por zona aceptadas por unidad    *
! hidro.                                                              *
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

INTEGER   i, j, r, u, iniciou, unidad, k, bloque
real*8    TresR10 ( maxint ), Tres10 ( maxint ), TresS ( maxint ), &
          TresRe ( maxint ), TresNR10 ( maxint ), TresRS ( maxint ), &
          TresNRS ( maxint )

write (UnirznuH,*) 'Reservas aceptadas por zona y unidades'
write (UnirznuH,*)
! para todo los grupos de reserva
do r = 1, NumGruRes
!   si la zona tiene unidades
    if ( NumUHxZona ( r ) .ne. 0 ) then
        TresR10 = 0.0
        TresNR10 = 0.0
        Tres10 = 0.0
        TresS = 0.0
        TresRS = 0.0
        TresNRS = 0.0
        TresRe = 0.0
        write ( UnirznuH, * ) 'Zona : ', NomZonaRes ( r )
        write ( UnirznuH, * )
!       para las unidades hidro que estan en ese grupo (zona)
        iniciou = ApunUHxZona ( r )
        do unidad = 1 , NumUHxZona ( r )
            u = UniHxZona ( iniciou )
            iniciou = iniciou + 1
            write ( UnirznuH, 500 ) u, nombunih(u)
            write ( UnirznuH, * ) 
            bloque = NTINTR/24
            do k = 1, bloque 
                write ( UnirznuH, 600 ) ( xMILP ( IRR10H + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
            enddo
            do k = 1, bloque
                write ( UnirznuH, 601 ) ( xMILP ( IRNR10H + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24  )
            enddo
            do k = 1, bloque
                write ( UnirznuH, 602 ) ( xMILP ( IRRSH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24  )
            enddo
            do k = 1 , bloque
                write ( UnirznuH, 603 ) ( xMILP ( IRNRSH + u + (j-1)*NumUniHid - 1 )*Base,j=k*24-23,k*24  )
            enddo
            do k = 1, bloque
                if ( TipoEjecu .le. 1 ) then
                    write ( UnirznuH, 604 ) ( xMILP ( IRREH + u + (j-1)*NumUniHid - 1 )*Base - xMILP ( IREUSOREH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                else
                    write ( UnirznuH, 604 ) ( xMILP ( IRREH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24  )
                endif
            enddo
            write ( UnirznuH, * )
            do i = 1, NTINTR
                if ( TipoEjecu .le. 1 ) then
                    TresRe ( i ) = TresRe ( i ) + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) - xMILP ( IREUSOREH + u + (i-1)*NumUniHid - 1 )
                else
                    TresRe ( i ) = TresRe ( i ) + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 )
                endif
                TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 )
                TresNR10 ( i ) = TresNR10 ( i ) + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )
!                Tres10 ( i ) = Tres10 ( i ) + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )
                Tres10 ( i ) = TresRe ( i )*SiRegEnRod + TresR10 ( i ) + TresNR10 ( i )
                TresRS ( i ) = TresRS ( i ) + xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 )
                TresNRS ( i ) = TresNRS ( i ) + xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )
!                TresS ( i ) = TresS ( i ) + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) + &
!                                            xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )
                TresS ( i ) = Tres10 ( i ) + TresRS ( i ) + TresNRS ( i )
            enddo
        enddo
        bloque = NTINTR/24
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TREG  ', ( TresRe ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TRR10 ', ( TresR10 ( j )*Base, j=k*24-23,k*24  )
        enddo
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TRNR10', ( TresNR10 ( j )*Base, j=k*24-23,k*24  )
        enddo
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TR10  ', ( Tres10 ( j )*Base, j=k*24-23,k*24  )
        enddo
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TRRSU ', ( TresRS ( j )*Base, j=k*24-23,k*24  )
        enddo
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TRNRSU', ( TresNRS ( j )*Base, j=k*24-23,k*24  )
        enddo
        do k = 1, bloque
            write ( UnirznuH, 605 ) 'TRSU  ', ( TresS ( j )*Base, j=k*24-23,k*24  )
        enddo
        write ( UnirznuH, 605 )
    endif
enddo

500 format ( i4, x, a15 )
600 format ( x, 'RR10', 2x, 169(f8.2) )
601 format ( x, 'RNR10', x, 169(f8.2) )
602 format ( x, 'RRS', 3x, 169(f8.2) )
603 format ( x, 'RNRS', 2x, 169(f8.2) )
604 format ( x, 'RRE', 3x, 169(f8.2) )
605 format ( x, a6, 169(f8.2) )

!close ( UnirznuH )

return
end

   
Subroutine ResSisUniH
! ---------------------------------------------------------------------
! Se escriben resultados de reservas por sistema aceptadas por unidad *
! hidro.                                                              *
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

INTEGER   i, j, r, u, k, bloque, uunidad, UniRR10CSV, ierror, UniRNR10CSV, &
          UniRRSCSV, UniRNRSCSV, UniRRECSV
real*8    TresR10 ( maxint ), Tres10 ( maxint ), TresS ( maxint ), &
          TresRe ( maxint ), TresNR10 ( maxint ), TresRS ( maxint ), &
          TresNRS ( maxint )

character*1 ssistema

write (UnirsnuH,*) 'Reservas aceptadas por sistema y unidades'
write (UnirsnuH,*)
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
        write ( UnirsnuH, * ) 'Sistema: ', nomsis ( r )
        write ( UnirsnuH, * )
!       para las unidades de rango continuo que estan en ese sistema
        do u = 1 , NumUniHid
            if ( IslaGenRC ( u ) .eq. r ) then
                write ( UnirsnuH, 500 ) u, nombunih(u)
                write ( UnirsnuH, * ) 
                bloque = NTINTR/4
                do k = 1, bloque
                    write ( UnirsnuH, 600 ) ( xMILP ( IRR10H + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                enddo
                !Escribe CSV
                write ( UniRR10CSV, 700 ) nombunih(u), 2, ( xMILP ( IRR10H + u + (j-1)*NumUniHid - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                   write ( UnirsnuH, 601 ) ( xMILP ( IRNR10H + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                enddo
                do j = 1,NTINTR  
                    ResNR10UH ( u, j ) =  xMILP ( IRNR10H + u + (j-1)*NumUniHid - 1 )
                end do
                !Escribe CSV
                write ( UniRNR10CSV, 700 ) nombunih(u), 2, ( xMILP ( IRNR10H + u + (j-1)*NumUniHid - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                   write ( UnirsnuH, 602 ) ( xMILP ( IRRSH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                enddo
                !Escribe CSV
                write ( UniRRSCSV, 700 ) nombunih(u), 2, ( xMILP ( IRRSH + u + (j-1)*NumUniHid - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                   write ( UnirsnuH, 603 ) ( xMILP ( IRNRSH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                enddo
                do j = 1,NTINTR  
                    ResNRSUUH ( u, j ) =  xMILP ( IRNRSH + u + (j-1)*NumUniHid - 1 )
                end do
                !Escribe CSV
                write ( UniRNRSCSV, 700 ) nombunih(u), 2, ( xMILP ( IRNRSH + u + (j-1)*NumUniHid - 1 )*Base, j=1,NTINTR )
                do k = 1, bloque
                    if ( TipoEjecu .le. 1 ) then
                        write ( UnirsnuH, 604 ) ( xMILP ( IRREH + u + (j-1)*NumUniHid - 1 )*Base - xMILP ( IREUSOREH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                    else
                        write ( UnirsnuH, 604 ) ( xMILP ( IRREH + u + (j-1)*NumUniHid - 1 )*Base, j=k*24-23,k*24 )
                    endif
                enddo
                !Escribe CSV
                if ( TipoEjecu .le. 1 ) then
                    write ( UniRRECSV, 700 ) nombunih(u), 2, ( xMILP ( IRREH + u + (j-1)*NumUniHid - 1 )*Base - xMILP ( IREUSOREH + u + (j-1)*NumUniHid - 1 )*Base, j=1,NTINTR )
                else
                    write ( UniRRECSV, 700 ) nombunih(u), 2, ( xMILP ( IRREH + u + (j-1)*NumUniHid - 1 )*Base, j=1,NTINTR )
                endif
                write ( UnirsnuH, * )
                do i = 1, NTINTR
                    if ( TipoEjecu .le. 1 ) then
                        TresRe ( i ) = TresRe ( i ) + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) - xMILP ( IREUSOREH + u + (i-1)*NumUniHid - 1 )
                    else
                        TresRe ( i ) = TresRe ( i ) + xMILP ( IRREH + u + (i-1)*NumUniHid - 1 )
                    endif
                    TresR10 ( i ) = TresR10 ( i ) + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 )
                    TresNR10 ( i ) = TresNR10 ( i ) + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )
!                    Tres10 ( i ) = Tres10 ( i ) + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 )
                    Tres10 ( i ) = TresRe ( i )*SiRegEnRod + TresR10 ( i ) + TresNR10 ( i )
                    TresRS ( i ) = TresRS ( i ) + xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 )
                    TresNRS ( i ) = TresNRS ( i ) + xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )
!                    TresS ( i ) = TresS ( i ) + xMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) + &
!                                                xMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) + xMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 )
                    TresS ( i ) = Tres10 ( i ) + TresRS ( i ) + TresNRS ( i )
                enddo
            endif
        enddo
        bloque = NTINTR/24
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TREG  ', ( TresRe ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TRR10 ', ( TresR10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TRNR10', ( TresNR10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TR10  ', ( Tres10 ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TRRSU ', ( TresRS ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TRNRSU', ( TresNRS ( j )*Base, j=k*24-23,k*24 )
        enddo
        do k = 1, bloque
           write ( UnirsnuH, 605 ) 'TRSU  ', ( TresS ( j )*Base, j=k*24-23,k*24 )
        enddo
        write ( UnirsnuH, 605 )
    
        close ( UniRR10CSV )
        close ( UniRNR10CSV )
        close ( UniRRSCSV )
        close ( UniRNRSCSV )
        close ( UniRRECSV )
        
    endif
enddo

500 format ( i4, x, a15 )
600 format ( x, 'RR10', 2x, 169(f8.2) )
601 format ( x, 'RNR10', x, 169(f8.2) )
602 format ( x, 'RRS', 3x, 169(f8.2) )
603 format ( x, 'RNRS', 2x, 169(f8.2) )
604 format ( x, 'RRE', 3x, 169(f8.2) )
605 format ( x, a6, 169(f8.2) )
700 format ( a15, ',', i2, ',', 169(f10.2, ',')  )

!close ( UnirsnuH )

return
end

    
subroutine ImpEnerEH ( sistema )
! ---------------------------------------------------------------------
! Se imprime el resultado de energia en embalses hidro                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2017                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only: NOMEMB, NOUN

implicit none

Integer i, intervalo, j, u, embalse, planta
Real*8  TotEner, TotEnerHor ( maxint ), aux, dual
Integer ierror, ibanbit, sistema, UniGenEmbCSV, uunidad, UniGenEmbHorCSV

CHARACTER fecha_Ej*19
character*15 aaux3
character*20 aaux2
character*1 ssistema

ibanbit = 1
ierror = 0

! Abre archivos csv de resultados de generacion en los embalses
uunidad = 131 + sistema
UniGenEmbCSV = uunidad
Write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = UniGenEmbCSV, FILE = trim(rut_dat_1)//'GENEMBRES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 300 )

uunidad = 141 + sistema
UniGenEmbHorCSV = uunidad
Write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = UniGenEmbHorCSV, FILE = trim(rut_dat_1)//'GENEMBHORRES_'//ssistema//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

write ( UnirEnH,* ) 
write ( UnirEnH,* ) '     Embalse   LinfEner   Energia    LSupEner     Pre Sombra'
write ( UnirEnH,* ) '                 (GWh)     (GWH)     (GWh)         ($/MWh)'
write ( UnirEnH,* ) 

do embalse = 1, NumEmbalses
    TotEner = 0.0
    TotEnerHor = 0.0
    do i = 0, NumCenHEmb ( embalse ) - 1
        planta = ListaPlantasH ( ApunPlantaEmbalse ( embalse ) + i )
!       para todas las unidades de la panta
        do j = 0, NOUN ( planta ) - 1
            u = ListaUnidadesH ( ApunUnidadPlanta ( planta ) + j )
!           para todos los intervalos
            do intervalo = 1, NTINTR
!               generacion de la unidad
                TotEner = TotEner + xMILP ( IGH + u + (intervalo-1)*NumUniHid - 1 ) * Base
!               generacion de la unidad por intervalo
                TotEnerHor ( intervalo ) = TotEnerHor ( intervalo ) + xMILP ( IGH + u + (intervalo-1)*NumUniHid - 1 ) * Base
            enddo
        enddo
    enddo
    dual = 0.0
    if ( SiEnerHid .eq. 1 ) then
        dual = dualenerbal ( embalse )
    endif
    write ( UnirEnH, 100 ) embalse, nomemb ( embalse ), LimIEnerEmb ( embalse )*base/1000.0, &
                           TotEner/1000.0, LimSEnerEmb ( embalse )*base/1000.0, -dual
!   imprime resultado de generacion total en el embalse
    write ( UniGenEmbCSV, 300 ) nomemb ( embalse ), TotEner/1000.0, -dual
    write ( UniGenEmbHorCSV, 600 ) nomemb ( embalse ), ( TotEnerHor( intervalo )/1000.0, intervalo=1,NTINTR )
!    j = 1
!    do d = 1, durdia
!        write ( UniGenEmbHorCSV, 600 ) nomemb ( embalse ), ( TotEnerHor( intervalo )/1000.0, intervalo = j, j + intdia ( d ) - 1 )
!        j = j + intdia ( d )
!    enddo

!   Si existe infactibilidad en el embalse
    if ( xMILP ( IAREE + embalse - 1 ) .gt. 1.0d-4 ) then
        SemBandera ( 6 ) = 1
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD EN ENERGIA HIDRO'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux2, 5102 ) nomemb ( embalse )
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 EMBALSE: '//aaux2
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        aux = xMILP ( IAREE + embalse - 1 )/10.0
        write ( aaux3, 5101 )  aux
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD (GWh)  '//aaux3
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif

enddo

close ( UniGenEmbCSV )
close ( UniGenEmbHorCSV )

100 format ( i4,3x, a6, 3(2x,f8.3), 2x, f16.4 )
300 FORMAT ( a6, ',', f8.3, ',', f16.4, ',' )
600 format ( a6, ',', 169(f8.4, ',') )
5101 FORMAT (F8.4)
5102 FORMAT (A20)

return
end    
    