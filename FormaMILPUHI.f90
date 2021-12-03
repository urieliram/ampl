  
! ---------------------------------------------------------------------
! Forman las restricciones del problema de asignacion y despacho de   *
! unidades (MILP), hidro.                                             *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine RestUH ( k, m, sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

Integer k, m,sistema, uunidad, IERROR

character*1 ssistema


! se forman restricciones de limites de generacion en unidades hidro
call LimGenH ( k, m )

! se forman restricciones de transiciones factibles en unidades hidro
call TransFacH ( k, m )

! si el problema contiene ofertas de reserva por zona o sistema
if ( SiOferComResZona .gt. 0 .or. SiOferComResSis .gt. 0 ) then
!   se forman restricciones de limites de reserva en unidades hidro
    call LimResUH ( k, m )
endif

! si el problema contiene limitaciones de energia hidro
if ( SiEnerHid ) then
!   se forman restricciones de limites de energia a embalses hidro
    call LimEnerEH ( k, m )
endif

! se considera la asignabilidad y disponibilidad del escenario para unidades hidro
call EstadoUniH

! si existen unidades hidro en el sistema
if ( NumuniHid .gt. 0 ) then

!   Se concatena el numero de subsistema al nombre de los archivos de debugger
    write( ssistema, '(I1)' )  sistema

!   se abren archivos para escritura de resultados hidro
!   Generacion de uniaddes hidro
    uunidad = 510 + sistema
    UnichauH = uunidad
    OPEN ( UNIT = UnichauH, FILE = RUT_RES//'r_chauH'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1250 )
!   Reserva por zona
    uunidad = 550 + sistema
    UnirznuH = uunidad
    OPEN ( UNIT = UnirznuH, FILE = RUT_RES//'r_UniResZonaH'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
!   Reserva por sistema
    uunidad = 580 + sistema
    UnirsnuH = uunidad
    OPEN ( UNIT = UnirsnuH, FILE = RUT_RES//'r_UniResSisH'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
!   Energia en embalses
    uunidad = 590 + sistema
    UnirEnH = uunidad
    OPEN ( UNIT = UnirEnH, FILE = RUT_RES//'r_EnerHid'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

!   si se desea considerar el modelado hidro
    if ( SiModHid .eq. 1 ) then
!       Abre archivo de resultados de turbinados
        OPEN ( UNIT = 95, FILE = RUT_RES//'r_turbinados.res', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )
!       Abre archivo de resultados de volumenes
        OPEN ( UNIT = 96, FILE = RUT_RES//'r_volumenes.res', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )
!       Abre archivo de resultados de politicas
        OPEN ( UNIT = 97, FILE = RUT_RES//'r_politicas.res', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )
!       Abre archivo de resultados de alturas de los embalses
        OPEN ( UNIT = 118, FILE = RUT_RES//'r_alturas.res', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )
    endif
endif

return
end


subroutine LimGenH ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de generacion operativo en       *
! unidades hidro.                                                     *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2019                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, m, u, ro
character*3 leti

! inicio de este tipo de restricciones
IRLMH = m + 1
write ( 777,* ) 'Inicia restricciones de limite maximo de generacion operativo UHI:', m + 1

! unidades hidro
do u = 1 , NumUniHid
! Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de aportacion a reserva rodante de 10 minutos
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRR10H + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de aportacion a reserva rodante suplementaria
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRRSH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de aportacion a reserva de regulacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRREH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de variable de asignacion
        aaMILP ( k ) = -PotMaxUniH ( u, i )
!       columna asociada
        jcolMILP( k ) = IAH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       si se permite hacer uso de reserva
        if ( TipoEjecu .le. 1 ) then
!           coeficiente de variable de uso de regulacion
            aaMILP ( k ) = - 1.0
!           columna asociada
            jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
            k = k + 1
        endif
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'L'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Limite maximo de generacion operativo UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
!       si es un compensador sincrono
        if ( CompSincH ( u, i ) .eq. 2 ) then
!           punto base en 0
            ubMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = 0
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite maximo con reserva regulacion  UHI:', m + 1

! unidades hidro
do u = 1 , NumUniHid
!   Para todos los intervalos
    do i = 1 , NTINTR
        if ( m == 4656 ) then
            continue
        endif
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de aportacion a reserva de regulacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRREH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       si se desea considerar bandas prohibidas y la unidad las tiene
        if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpH ( u )
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = -RaOpSupH ( u, ro, i )
!               columna asociada
                jcolMILP( k ) = IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2
                k = k + 1
            enddo
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!               para los rangos de la unidad
                do ro = 1, NoRaOpH ( u )
!                   coeficiente de variable de asignacion de reserva en rango operativo
                    aaMILP ( k ) = RaOpSupH ( u, ro, i ) - RaRegSupH  ( u, ro, i )
!                   columna asociada
                    jcolMILP( k ) = INVREROH + IREROH ( u, i ) + ro - 1
                    k = k + 1
                enddo
            endif
        else
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -PotMaxUniH ( u, i )
!           columna asociada
            jcolMILP( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = - PotMaxRUniH ( u, i ) + PotMaxUniH ( u, i )
!               columna asociada
                jcolMILP( k ) = IVREGH + INREGH ( u, i ) - 1
                k = k + 1
            endif
        endif
!       si se permite hacer uso de reserva
        if ( TipoEjecu .le. 1 ) then
!           coeficiente de variable de uso de regulacion
            aaMILP ( k ) = - 1.0
!           columna asociada
            jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
            k = k + 1
        endif
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'L'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Limite maximo con reserva regulacion  UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

! unidades hidro
do u = 1 , NumUniHid
!   Para todos los intervalos
    do i = 1 , NTINTR
!       si se desea considerar bandas prohibidas y la unidad las tiene y oferta regulacion
        if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 .and. CalOferResRegH ( u, i ) .gt. 0.0 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpH ( u )
!               coeficiente de variable de asignacion de reserva en rango operativo
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = INVREROH + IREROH ( u, i ) + ro - 1
                k = k + 1
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2
                k = k + 1
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 0.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Limite maximo con reserva regulacion con bandas prohibidas UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'  
            enddo
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de generacion operativo UHI:', m + 1

! unidades hidro
do u = 1 , NumUniHid
! Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de aportacion a reserva de regulacion
        aaMILP ( k ) = -1.0
!       columna asociada
        jcolMILP( k ) = IRREH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       si se desea considerar bandas prohibidas y la unidad las tiene
        if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpH ( u )
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = -RaOpInfH ( u, ro, i )
!               columna asociada
                jcolMILP( k ) = IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2
                k = k + 1
            enddo
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!               para los rangos de la unidad
                do ro = 1, NoRaOpH ( u )
!                   coeficiente de variable de asignacion de reserva en rango operativo
                    aaMILP ( k ) = RaOpInfH ( u, ro, i ) - RaRegInfH  ( u, ro, i )
!                   columna asociada
                    jcolMILP( k ) = INVREROH + IREROH ( u, i ) + ro - 1
                    k = k + 1
                enddo
            endif
        else
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -PotMinUniH ( u, i )
!           columna asociada
            jcolMILP( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = - PotMinRUniH ( u, i ) + PotMinUniH ( u, i )
!               columna asociada
                jcolMILP( k ) = IVREGH + INREGH ( u, i ) - 1
                k = k + 1
            endif
        endif
!       si se permite hacer uso de reserva
        if ( TipoEjecu .le. 1 ) then
!           coeficiente de variable de uso de regulacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
            k = k + 1
        endif
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'G'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Limite minimo de generacion operativo UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
!       si es un compensador sincrono
        if ( CompSincH ( u, i ) .eq. 2 ) then
!           punto base en 0
            lbMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = 0
        endif      
    enddo
enddo

200 format (i3)
    
return
end    


Subroutine TransFacH ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de transiciones entre condiciones de        *
! operacion y paro en unidades hidro.                                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2018                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, m, u, IntIni, dia, intervalo
real*8  beta
character*3 leti

write ( 777,* ) 'Inicia restricciones de transiciones entre condiciones en unid HI:', m + 1

write ( 777,* ) 'Inicia transiciones factibles de primer intervalo                :', m + 1
! restricciones de transiciones factibles de primer intervalo
do u = 1 , NumUniHid
    if ( m .eq. 2594 ) then
        continue
    end if
    beta = 0.0
    if ( EstadoCIUH( u ) .gt. 0 ) then
!       la unidad esta encendida en condiciones iniciales
        beta = 1.0
    endif
!   coeficiente de asignacion en el intervalo
    aaMILP ( k ) = 1.0
    jcolMILP ( k ) = IAH + u - 1
    k = k + 1
!   coeficiente de arranque en el intervalo
    aaMILP ( k ) = -1.0
    jcolMILP ( k ) = IARH + u - 1
    k = k + 1
!   coeficiente de paro en el intervalo
    aaMILP ( k ) = 1.0
    jcolMILP ( k ) = IPH + u - 1
    k = k + 1
!   lados derechos de las restriciones
    m = m + 1
    bMILP ( m )   = 0.0 + beta
!   sentidos de las restriciones
    sMILP ( m ) = 'E'
!   apuntador al siguiente renglon
    irowMILP ( m + 1 ) = k
    write ( 779,* ) m, ',', '"Transiciones entre condiciones primer intervalo UHI: '//trim(nombunih (u))//'"'
enddo

write ( 777,* ) 'Inicia siguientes restricciones de transiciones factibles        :', m + 1

! siguientes restricciones de transiciones factibles
do u = 1 , NumUniHid
    do i = 2 , NTINTR
!       coeficiente de asignacion en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de asignacion en el intervalo anterior
        aaMILP ( k ) = -1.0
        jcolMILP ( k ) = IAH + u + (i-2)*NumUniHid - 1
        k = k + 1
!       coeficiente de arranque en el intervalo
        aaMILP ( k ) = -1.0
        jcolMILP ( k ) = IARH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de paro en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IPH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m )   = 0.0
!       sentidos de las restriciones
        sMILP ( m ) = 'E'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Transiciones entre condiciones siguientes intervalos UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

write ( 777,* ) 'Inicia restricciones que obligan a un solo tipo de estado en U HI:', m + 1

! restricciones que obligan a un solo tipo de estado (arranque o paro)
! para todas las unidades
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de arranque en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IARH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       coeficiente de paro en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IPH + u + (i-1)*NumUniHid - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m )   = 1.0
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Se obliga a un solo tipo de estado UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de maximo numero de paros en unidades hidro :', m + 1

! restricciones de maximo numero de paros en unidades hidro
! para todas las unidades
do u = 1 , NumUniHid
    IntIni = 1
!   para todos los dias del horizonte
    do dia = 1, DURDIA
!       para todos los intervalos del dia
        do intervalo = IntIni, IntIni + intdia (dia) - 1
!           coeficiente de paro en el intervalo
            aaMILP ( k ) = 1.0
            jcolMILP ( k ) = IPH + u + (intervalo-1)*NumUniHid - 1
            k = k + 1
        enddo
!       lados derechos de las restriciones
        m = m + 1
!       bMILP ( m )   = NumMaxParoUH ( u )
        bMILP ( m )   = max ( 0, NumMaxParoUH ( u ) - NoActParH ( u ) )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) dia
        write ( 779,* ) m,',', '"Maximo numero de paros unidad hidro: '//trim(nombunih(u))//' dia: '//trim(leti)//'"'
        IntIni = IntIni + intdia ( dia )
    enddo
enddo

200 format (i3)
    
return
end

    
Subroutine EstadoUniH
! ---------------------------------------------------------------------
! Restricciones sobre estados de las unidades hidro en el problema    *
! de asignacion (MILP).                                               *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2017                                                        *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, u

! para las unidades de rango continuo
do u = 1 , NumUniHid
    do i = 1 , NTINTR
!       si es unidad no disponible en este periodo
        if ( DispoUH ( u , i ) .eq. 0 ) then
!           la unidad no se puede operar
            lbMILP ( IAH + u + (i-1)*NumUniHid - 1 ) = 0.0
            ubMILP ( IAH + u + (i-1)*NumUniHid - 1 ) = 0.0
!           la unidad no puede generar
            lbMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = 0.0
            ubMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = 0.0
!           si es unidad disponible y no asignables en este periodo
	    elseif ( DispoUH ( u , i ) .eq. 1 .and.  AsignUH ( u , i ) .eq. 0 ) then
!           la unidad debe operar
            lbMILP ( IAH + u + (i-1)*NumUniHid -1 ) = 1.0
            ubMILP ( IAH + u + (i-1)*NumUniHid -1 ) = 1.0
        endif
   enddo
enddo

return
end

    
Subroutine LimResUH ( k, m )
! ---------------------------------------------------------------------
! Restricciones reserva rodante de diez minutos de unidades hidro, en *
! el problema de asignacion (MILP).                                   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2020                                                  *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, k, m, ro, u, unidad
real*8    maximo, minimo
character*3 leti

! restricciones de limites de reserva rodante de 10 minutos
write ( 777,* ) 'Inicia restricciones de reserva rodante de 10 minutos unidades hi:', m + 1
! para las unidades hidro
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible y cordinable en este periodo
!        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 ) then
!       si es unidad disponible en este periodo
        if ( DispoUH ( u , i ) .eq. 1 ) then
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = - CalOferResR10H ( u, i )
            jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva rodante de 10 minutos UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva rodante suplementaria
write ( 777,* ) 'Inicia restricciones de reserva suplementaria para unidades hidro:', m + 1
! para las unidades hidro
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible y cordinable en este periodo
!        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 ) then
!       si es unidad disponible en este periodo
        if ( DispoUH ( u , i ) .eq. 1 ) then
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRRSH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = - CalOferResRxH ( u, i )
            jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva suplementaria UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva de regulacion secundaria
write ( 777,* ) 'Inicia restricciones de limite maximo de reserva de regulacio UHI:', m + 1
unidad = 0
! para las unidades hidro
do u = 1 , NumUniHid
!   si la unidad tiene bandas prohibidas
    if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
        unidad = unidad + 1
    endif
!   para todos los intervalos
    do i = 1 , NTINTR
!       si la unidad oferta reserva de regulacion secundaria
        if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!           si es unidad disponible y cordinable en este periodo
!            if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 ) then
            if ( DispoUH ( u , i ) .eq. 1 ) then
!               coeficiente de la variable de reserva
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
                k = k + 1
!               si se permite hacer uso de reserva
                if ( TipoEjecu .le. 1 ) then
!                   coeficiente de variable de uso
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
!               si se desea considerar bandas prohibidas y la unidad las tiene
                if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
!                   para los rangos de la unidad
                    do ro = 1, NoRaOpH ( u )
!                       coeficiente de variable de asignacion de rango operativo
                        aaMILP ( k ) = -CalOferRegROH  ( unidad, ro, i )
!                       columna asociada
                        jcolMILP( k ) = INVREROH + IREROH ( u, i ) + ro - 1
                        k = k + 1
                    enddo
!                   Si la unidad tiene dos rangos de regulacion
                    if ( NoRaOpH ( u ) .eq. 3 .and. (RaRegSupH  ( u, 1, i ) .eq. RaRegInfH  ( u, 2, i )) .and. &
                         (RaRegSupH  ( u, 2, i ) .eq. RaRegInfH  ( u, 3, i )) ) then
!                       la unidad no debe regular en el rango 2
                        ubMILP ( INVREROH + IREROH ( u, i ) + 1 ) = 0.0
                    endif
                else
!                   coeficiente de variable de asignacion de reserva
                    aaMILP ( k ) = - CalOferResRegH ( u, i )
!                   columna asociada
                    jcolMILP( k ) = IVREGH + INREGH ( u, i ) - 1
                    k = k + 1
! 					si la solucion es en variables enteras
					if ( TipoProblema .eq. 1 ) then
                    	ctypeMILP ( IVREGH + INREGH ( u, i ) - 1 ) = 'B'
					endif
!                   cota inferior de la variable
                    lbMILP ( IVREGH + INREGH ( u, i ) - 1 ) = 0.0
!                   cota superior de la variable
                    ubMILP ( IVREGH + INREGH ( u, i ) - 1 ) = 1.0
                endif
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Limite maximo reserva regulacion de unidad UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
            endif
        else
!           no existe reserva de regulacion para la unidad
            ubMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) = 0.0
        endif
   enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo reserva de regulacion de UH:', m + 1
unidad = 0
! para las unidades hidro
do u = 1 , NumUniHid
!   si la unidad tiene bandas prohibidas
    if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
        unidad = unidad + 1
    endif
!   para todos los intervalos
    do i = 1 , NTINTR
!       si la unidad oferta reserva de regulacion secundaria
        if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!           si es unidad disponible y cordinable en este periodo
!            if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 ) then
            if ( DispoUH ( u , i ) .eq. 1 ) then
!               coeficiente de la variable de reserva
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
                k = k + 1
!               si se permite hacer uso de reserva
                if ( TipoEjecu .le. 1 ) then
!                   coeficiente de variable de uso
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
!               si se desea considerar bandas prohibidas y la unidad las tiene
                if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
!                   para los rangos de la unidad
                    do ro = 1, NoRaOpH ( u )
                        minimo = MrreUH ( u, i )
!                        if ( minimo .gt. CalOferRegROH  ( unidad, ro, i ) ) then
!                            minimo = CalOferRegROH  ( unidad, ro, i )
!                        endif
!                       coeficiente de variable de asignacion de rango operativo
                        aaMILP ( k ) = -minimo
!                       columna asociada
                        jcolMILP( k ) = INVREROH + IREROH ( u, i ) + ro - 1
                        k = k + 1
                    enddo
                else
                    minimo = MrreUH ( u, i )
!                    if ( minimo .gt. CalOferResRegH ( u, i ) ) then
!                        minimo = CalOferResRegH ( u, i )
!                    endif
!                   coeficiente de variable de asignacion de reserva
                    aaMILP ( k ) = - minimo
!                   columna asociada
                    jcolMILP( k ) = IVREGH + INREGH ( u, i ) - 1
                    k = k + 1
                endif
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Limite minimo reserva regulacion de unidad UHI '//trim(nombunih(u))//' intervalo: '//leti//'"'
            endif
        endif
   enddo
enddo

if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "BCA" ) then
!if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "SIS" ) then
    ! restricciones de limites de reserva de regulacion secundaria y rodante de 10 minutos por rampa de 10 minutos
    write ( 777,* ) 'Inicia restricciones de reserva de reg y roda de 10 por rampa UHI:', m + 1
    unidad = 0
    ! para las unidades hidro
    do u = 1 , NumUniHid
    !   si la unidad tiene bandas prohibidas
        if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
            unidad = unidad + 1
        endif
    !   para todos los intervalos
        do i = 1 , NTINTR
    !       si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
                if ( DispoUH ( u , i ) .eq. 1 ) then
    !               coeficiente de la variable de reserva de regulacion
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
                    k = k + 1
    !               si se permite hacer uso de reserva
                    if ( TipoEjecu .le. 1 ) then
    !                   coeficiente de variable de uso
                        aaMILP ( k ) = - 1.0
    !                   columna asociada
                        jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
                        k = k + 1
                    endif
    !               coeficiente de la variable de reserva rodante de diez
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                    k = k + 1
    !               lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = RamEmer10H ( u )
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Reserva de regulacion y rodante de 10 por rampa unidad de HI: '//trim(nombunih(u))//' intervalo: '//leti//'"'
                endif
            endif
        enddo
    enddo
endif

! restricciones de limites de reserva rodante de diez minutos y suplementaria
write ( 777,* ) 'Inicia restricciones de reserva rodante de diez y suplementar UHI:', m + 1
! para las unidades hidro
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible y cordinable en este periodo
!        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 ) then
!       si es unidad disponible en este periodo
        if ( DispoUH ( u , i ) .eq. 1 ) then
            if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "BCA" ) then
!            if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "SIS" ) then
!               coeficiente de la variable de reserva de regulacion
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
                k = k + 1
!               si se permite hacer uso de reserva
                if ( TipoEjecu .le. 1 ) then
!                   coeficiente de variable de uso
                    aaMILP ( k ) = - 1.0
!                   columna asociada
                    jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
            endif
!           coeficiente de la variable de reserva rodante de diez
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de reserva suplementaria
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRRSH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = RamEmerxH ( u )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva rodante de 10 y suplementaria UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        endif
   enddo
enddo


! restricciones de limites de reserva no rodante de 10 minutos
write ( 777,* ) 'Inicia restricciones de reserva no rodante de 10 minutos para UHI:', m + 1
! para las unidades hidro
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible, asignable  y cordinable en este periodo
        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 .and. AsignUH ( u , i ) .eq. 1 ) then
goto 777
!           limite inferior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva no rodante de 10 minutos UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
777 continue
!           limite superior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = OferResNR10H ( u, i )
            jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = OferResNR10H ( u, i )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva no rodante de 10 minutos UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva no rodante suplementaria
write ( 777,* ) 'Inicia restricciones de reserva no rodante suplementaria para UHI:', m + 1
! para las unidades hidro
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible, asignable  y cordinable en este periodo
        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 .and. AsignUH ( u , i ) .eq. 1 ) then
goto 778
!           limite inferior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNRSH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva no rodante suplementaria UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
778 continue
!           limite superior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNRSH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = OferResNRxH ( u, i )
            jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = OferResNRxH ( u, i )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva no rodante suplementaria UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva no rodante de diez minutos y suplementaria
write ( 777,* ) 'Inicia restricciones de reserva no rodante de 10 min y suplem UHI:', m + 1
! para las unidades hidro
do u = 1 , NumUniHid
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible, asignable y cordinable en este periodo
        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 .and. AsignUH ( u , i ) .eq. 1 ) then
            maximo =  max ( OferResNR10H ( u, i ), OferResNRxH ( u, i ) )
!           coeficiente de la variable de reserva no rodante de diez
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de reserva no rodante suplementaria
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNRSH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = maximo
            jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = maximo
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Reserva no rodante de 10 y suplementaria UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        endif
   enddo
enddo

200 format (i3)
    
return
end


    
subroutine LimEnerEH ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de energia a embalses hidro      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only: NOMEMB, NOUN

implicit none

Integer k, i, intervalo, j, m, u, embalse, planta

APResLimSH = m

write ( 777,* ) 'Inicia restricciones de limite maximo de energia en los embalses :', m + 1

do embalse = 1, NumEmbalses
!   para todos los embalses con limitacion de energia
    if ( RestEnergia ( embalse ) .eq. 1 ) then
!       para todas la plantas del embalse
        do i = 0, NumCenHEmb ( embalse ) - 1
            planta = ListaPlantasH ( ApunPlantaEmbalse ( embalse ) + i )
!           para todas las unidades de la panta
            do j = 0, NOUN ( planta ) - 1
                u = ListaUnidadesH ( ApunUnidadPlanta ( planta ) + j )
!               para todos los intervalos
                do intervalo = 1, NTINTR
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGH + u + (intervalo-1)*NumUniHid - 1
                    k = k + 1
                enddo
            enddo
        enddo
!       coeficiente de la variable artificial de energia
        aaMILP ( k ) = - 1.0
!       columna asociada
        jcolMILP( k ) = IAREE + embalse - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LimSEnerEmb ( embalse )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( 779,* ) m, ',', '"Limite maximo de energia en el embalse: '//trim(nomemb (embalse))//'"'
    endif
enddo

APResLimIH = m

write ( 777,* ) 'Inicia restricciones de limite minimo de energia en los embalses :', m + 1

do embalse = 1, NumEmbalses
!   para todos los embalses con limitacion de energia
    if ( RestEnergia ( embalse ) .eq. 1 ) then
!       para todas la plantas del embalse
        do i = 0, NumCenHEmb ( embalse ) - 1
            planta = ListaPlantasH ( ApunPlantaEmbalse ( embalse ) + i )
!           para todas las unidades de la panta
            do j = 0, NOUN ( planta ) - 1
                u = ListaUnidadesH ( ApunUnidadPlanta ( planta ) + j )
!               para todos los intervalos
                do intervalo = 1, NTINTR
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGH + u + (intervalo-1)*NumUniHid - 1
                    k = k + 1
                enddo
            enddo
        enddo
!       coeficiente de la variable artificial de energia
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IAREE + embalse - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LimIEnerEmb ( embalse )
!       sentidos de las restriciones
        sMILP ( m ) = 'G'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( 779,* ) m, ',', '"Limite minimo de energia en el embalse: '//trim(nomemb (embalse))//'"'
    endif
enddo

return
end    

    

subroutine RangOperH ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de asociacion entre asignacion de rango *
! y asignacion de operacion en unidades hidro                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2015                                                  *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, m, u, ro
character*3 leti

write ( 777,* ) 'Inicia restricciones de asociacion asignacion rangos y operac HI :', m + 1

! unidades hidro
do u = 1 , NumUniHid
    if ( NoRaOpH ( u ) .gt. 0 ) then
!       para todos los intervalos
        do i = 1 , NTINTR
!           para los rangos de la unidad
            do ro = 1, NoRaOpH ( u )
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2
                k = k + 1
            enddo
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = IAH + u + (i-1)*NumUniHid - 1
            k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'E'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Asociacion asignacion rangos y operacion unidad UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
        enddo
    endif
enddo

200 format (i3)
    
return
end    
