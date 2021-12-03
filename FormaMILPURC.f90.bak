  
! ---------------------------------------------------------------------
! Forman las restricciones del problema de asignacion y despacho de   *
! unidades (MILP), de rango continuo.                                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2017                                                     *
! ---------------------------------------------------------------------
Subroutine RestURC ( k, m )

use ParAUHE
use ProblemaAUHE

Implicit none

Integer k, m

! se forma la restriccion de nivel de generacion en unidades de rango continuo
call NivelGenRC ( k, m )

! se forma la restriccion de segmentos de ofertas de venta en unidades de rango continuo
call SegVenRC ( k, m )

! se cosidera la asignabilidad y disponibilidad del escenario para unidades de rango continuo
call EstadoUniRC

! se forman restricciones de limites de generacion en unidades de rango continuo
call LimGenORC ( k, m )

! se forman restricciones de transiciones factibles en unidades de rango continuo
call TransFacRC ( k, m )

! Si no es EXPOST
if ( TipoEjecu .ne. 2 ) then

!   se forman restricciones de tiempos minimos de operacion y paro en unidades de rango continuo
    call TieMParoOperRC ( k, m )
!    call TieMParoOperRC_new ( k, m )

!   se forman restricciones de condiciones durante el arranque en unidades de rango continuo
    call CondArranRC ( k, m )

endif

! se forman restricciones de rampas de operacion en unidades de rango continuo
call RampasOperRC ( k, m )

! se forman restricciones de costos variables de arranque en unidades de rango continuo
call CostVarArrRC ( k, m )

! si el problema contiene ofertas de reserva por zona o sistema
if ( SiOferComResZona .gt. 0 .or. SiOferComResSis .gt. 0 ) then
!   se forman restricciones de limites de reserva en unidades de rango continuo
    call LimResURC ( k, m )
endif

! si se desea considerar arranque no simultaneo de unidades 
if ( SiArrNoSimul .eq. 1 ) then
!   se forman restricciones de unidades que no pueden ser arrancadas simultaneamente
    call GruposArranqueAsincrono ( k, m )
endif

! se forman restricciones de unidades de propiedad conjunta
call RestUPC ( k, m )

! Si no es EXPOST
!if ( TipoEjecu .ne. 2 ) then
! se forman restricciones de tiempos minimos de regulacion en unidades de rango continuo
!    call TieMinRegRC ( k, m )
!endif

return
end

        
subroutine NivelGenRC ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de nivel de generacion en unidades de       *
! rango continuo                                                      *
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

Integer k, kv, m, i, u, s
character*3 leti

write ( 777,* ) 'Inicia restricciones de nivel de generacion en unidades de RangoC:', m + 1

! unidades de rango continuo
kv = IGABRC
do u = 1 , NumUniRC
! Para todos los intervalos
    do i = 1 , NTINTR
        if ( i .eq. 10 .and. u .eq. 176 ) then
            continue
        endif
        if ( CompSincRC ( u, i ) .eq. 0 ) then
    !       coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
    !       columna asociada
            jcolMILP( k ) = IGRC + u + (i-1)*NumUniRC - 1
            k = k + 1
    !       si la unidad tiene tiempo de arranque
            if ( TiempoArraURC ( u ) .gt. 0.0 ) then
    !          coeficiente de la variable de generacion durante arranque
               aaMILP ( k ) = -1.0
    !          columna asociada
               jcolMILP( k ) = IGDARC + u + (i-1)*NumUniRC - 1
               k = k + 1
            endif
    !       coeficiente de la variable de asignacion
            aaMILP ( k ) = -PotMinGRC(u,i)
    !       columna asociada
            jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
        endif
!       si son ofertas de costo a generacion minima
        if ( TipoOferta .eq. 1 ) then
!           para todos los segmentos de curva de ofertas de venta
            do s = 1, NumBloVRC( u, i )
!               coeficiente de la variable de generacion aceptada en el bloque
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = kv
                k = k + 1
                kv = kv + 1
            enddo
!       si son ofertas de costo en vacio
        else
            if ( NumBloVRC( u, i ) .gt. 0 ) then
                kv = kv + 1
            endif
!           para todos los segmentos de curva de ofertas de venta, excepto el primero
            do s = 2, NumBloVRC( u, i )
!               coeficiente de la variable de generacion aceptada en el bloque
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = kv
                k = k + 1
                 kv = kv + 1
            enddo
        endif
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'E'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m,',', '"Nivel de generacion en unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo

200 format (i3)
    
return
end
    

subroutine SegVenRC ( k, m )
! ---------------------------------------------------------------------
! Se forma la restriccion de segmentos de ofertas de venta en         *
! unidades de rango continuo.                                         *
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

Integer i, k, m, u, s, kv
character*3 leti

! unidades de rango continuo
kv = IGABRC
do u = 1 , NumUniRC
! Para todos los intervalos
    do i = 1 , NTINTR
!       si son ofertas de costo a generacion minima
        if ( TipoOferta .eq. 1 ) then
            do s = 1, NumBloVRC( u, i )
!               limite superior de la variable de generacion en el segmento
                lbMILP ( kv ) = 0.0
!               coeficiente de la variable de generacion
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = kv
                k = k + 1
!               coeficiente de variable de asignacion
                aaMILP ( k ) = -OferVenEnerRC ( u, s, i )
!               columna asociada
                jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 0.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                kv = kv + 1
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Segmentos de venta unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            enddo
!       si son ofertas de costo en vacio
		else
            if ( NumBloVRC( u, i ) .gt. 0 ) then
!               primer segmento
!               coeficiente de la variable de generacion durante arranque
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IGDARC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               coeficiente de variable de asignacion durante arranque
                aaMILP ( k ) = -OferVenEnerRC ( u, 1, i )
!               columna asociada
                jcolMILP( k ) = IADARC + u + (i-1)*NumUniRC - 1
                k = k + 1
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 0.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                kv = kv + 1
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Segmentos de venta unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                do s = 2, NumBloVRC( u, i )
!                   limite inferior de la variable de generacion en el segmento
                    lbMILP ( kv ) = 0.0
!                   coeficiente de la variable de generacion
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = kv
                    k = k + 1
!                   coeficiente de variable de asignacion
                    aaMILP ( k ) = -OferVenEnerRC ( u, s, i )
!                   columna asociada
                    jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
                    m = m + 1
!                   lado derecho de la restriccion
                    bMILP ( m ) = 0.0
!                   sentido de la restriccion
                    sMILP ( m ) = 'L'
!                   inicio de la siguiente restriccion
                    irowMILP ( m + 1 ) = k
                    kv = kv + 1
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Segmentos de venta unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                enddo
            endif
        endif
    enddo
enddo

200 format (i3)
    
return
end    


subroutine LimGenORC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de generacion operativo en       *
! unidades de rango continuo.                                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo de 2020                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, m, u, ro
character*3 leti

! inicio de este tipo de restricciones
IRLMRC = m + 1
write ( 777,* ) 'Inicia restricciones de limite maximo de generacion operativo URC:', m + 1

! unidades de rango continuo
do u = 1 , NumUniRC
! Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de aportacion a reserva rodante de 10 minutos
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de aportacion a reserva rodante suplementaria
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRRSRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de aportacion a reserva de regulacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRRERC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de variable de asignacion
        aaMILP ( k ) = -PotMaxGRC ( u, i )
!       columna asociada
        jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de variable de asignacion durante el arranque
        aaMILP ( k ) = - PotMinGRC ( u, i )
!       columna asociada
        jcolMILP( k ) = IADARC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       si se permite hacer uso de reserva
        if ( TipoEjecu .le. 1 ) then
!           coeficiente de variable de uso
            aaMILP ( k ) = - 1.0
!           columna asociada
            jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de variable de uso de regulacion
            aaMILP ( k ) = - 1.0
!           columna asociada
            jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
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
        write ( 779,* ) m,',', '"Limite maximo de generacion operativo unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
!       si es un compensador sincrono
        if ( CompSincRC ( u, i ) .eq. 2 ) then
!           punto base en 0
            ubMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = 0
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite maximo con reserva regulacion  URC:', m + 1

! unidades de rango continuo
do u = 1 , NumUniRC
!   Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de aportacion a reserva de regulacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IRRERC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de variable de asignacion durante el arranque
        aaMILP ( k ) = - PotMinGRC ( u, i )
!       columna asociada
        jcolMILP( k ) = IADARC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       si se desea considerar bandas prohibidas y la unidad las tiene
        if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpRC ( u )
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = -RaOpSupRC ( u, ro, i )
!               columna asociada
                jcolMILP( k ) = IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2
                k = k + 1
            enddo
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!               para los rangos de la unidad
                do ro = 1, NoRaOpRC ( u )
!                   coeficiente de variable de asignacion de reserva en rango operativo
                    aaMILP ( k ) = RaOpSupRC ( u, ro, i ) - RaRegSupRC  ( u, ro, i )
!                   columna asociada
                    jcolMILP( k ) = INVRERORC + IRERORC ( u, i ) + ro - 1
                    k = k + 1
                enddo
            endif
        else
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -PotMaxGRC ( u, i )
!           columna asociada
            jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = - PotMaxRRC ( u, i ) + PotMaxGRC ( u, i )
!               columna asociada
                jcolMILP( k ) = IVREGRC + INREGRC ( u, i ) - 1
                k = k + 1
            endif
        endif
!       si se permite hacer uso de reserva
        if ( TipoEjecu .le. 1 ) then
!           coeficiente de variable de uso de regulacion
            aaMILP ( k ) = - 1.0
!           columna asociada
            jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
            k = k + 1
        endif
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'L'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write(leti,200) i
        write ( 779,* ) m,',', '"Limite maximo con reserva regulacion  URC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

! unidades de rango continuo
do u = 1 , NumUniRC
    if ( u .eq. 189 ) then
        continue
    endif
!   Para todos los intervalos
    do i = 1 , NTINTR
!       si se desea considerar bandas prohibidas y la unidad las tiene y oferta regulacion
        if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 .and. CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpRC ( u )
!               coeficiente de variable de asignacion de reserva en rango operativo
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = INVRERORC + IRERORC ( u, i ) + ro - 1
                k = k + 1
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2
                k = k + 1
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 0.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                write(leti,200) i
                write ( 779,* ) m,',', '"Relacion entre variable binaria de regulacion y opera URC: '//trim(nombunirc(u))//' intervalo: '//leti//'"'
            enddo
        else
!           si la unidad no tiene rangos operativos y si tiene oferta de regulacion
            if ( NoRaOpRC ( u ) .eq. 0 .and. CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IVREGRC + INREGRC ( u, i ) - 1
                k = k + 1
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 0.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                write(leti,200) i
                write ( 779,* ) m,',', '"Relacion entre variable binaria de regulacion y opera URC: '//trim(nombunirc(u))//' intervalo: '//leti//'"'
            endif
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de generacion operativo URC:', m + 1

! unidades de rango continuo
do u = 1 , NumUniRC
    if ( u .eq. 189 ) then
        continue
    endif
! Para todos los intervalos
    do i = 1 , NTINTR
        if ( CompSincRC ( u, i ) .eq. 0 ) then
    !       coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
    !       columna asociada
            jcolMILP( k ) = IGRC + u + (i-1)*NumUniRC - 1
            k = k + 1
    !       coeficiente de aportacion a reserva de regulacion
            aaMILP ( k ) = -1.0
    !       columna asociada
            jcolMILP( k ) = IRRERC + u + (i-1)*NumUniRC - 1
            k = k + 1
    !       coeficiente de variable de asignacion durante el arranque
            aaMILP ( k ) = - PotSincURC ( u )
    !       si la unidad ya esta en proceso de arranque
            if ( lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) .eq. 1.0 ) then
                aaMILP ( k ) = - (GenCIURC ( u ) + RampArraURC ( u ))
            endif
    !       columna asociada
            jcolMILP( k ) = IADARC + u + (i-1)*NumUniRC - 1
            k = k + 1
    !       si se desea considerar bandas prohibidas y la unidad las tiene
            if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
    !           para los rangos de la unidad
                do ro = 1, NoRaOpRC ( u )
    !               coeficiente de variable de asignacion de rango operativo
                    aaMILP ( k ) = -RaOpInfRC ( u, ro, i )
    !               columna asociada
                    jcolMILP( k ) = IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2
                    k = k + 1
                enddo
    !           si la unidad oferta reserva de regulacion secundaria
                if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
    !               para los rangos de la unidad
                    do ro = 1, NoRaOpRC ( u )
    !                   coeficiente de variable de asignacion de reserva en rango operativo
                        aaMILP ( k ) = RaOpInfRC ( u, ro, i ) - RaRegInfRC  ( u, ro, i )
    !                   columna asociada
                        jcolMILP( k ) = INVRERORC + IRERORC ( u, i ) + ro - 1
                        k = k + 1
                    enddo
                endif
            else
    !           coeficiente de variable de asignacion
                aaMILP ( k ) = -PotMinGRC ( u, i )
    !           columna asociada
                jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           si la unidad oferta reserva de regulacion secundaria
                if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
    !               coeficiente de variable de asignacion de reserva
                    aaMILP ( k ) = - PotMinRRC ( u, i ) + PotMinGRC ( u, i )
    !               columna asociada
                    jcolMILP( k ) = IVREGRC + INREGRC ( u, i ) - 1
                    k = k + 1
                endif
            endif
!           si se permite hacer uso de reserva
            if ( TipoEjecu .le. 1 ) then
!               coeficiente de variable de uso de regulacion
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                k = k + 1
            endif
            m = m + 1
    !       lado derecho de la restriccion
            bMILP ( m ) = 0.0
    !       sentido de la restriccion
            sMILP ( m ) = 'G'
    !       inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write(leti,200) i
            write ( 779,* ) m,',', '"Limite minimo de generación operativo URC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'
        endif
!       si es un compensador sincrono
        if ( CompSincRC ( u, i ) .eq. 2 ) then
!           punto base en 0
            lbMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = 0
        endif      
    enddo
enddo

200 format (i3)

return
end    

Subroutine TransFacRC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de transiciones entre condiciones de        *
! operacion y paro en unidades de rango continuo.                     *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2018                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, m, u, IntIni, dia, intervalo
real*8  beta
character*3 leti

write ( 777,* ) 'Inicia restricciones de transiciones entre condiciones en unid RC:', m + 1

write ( 777,* ) 'Inicia transiciones factibles de primer intervalo                :', m + 1
! restricciones de transiciones factibles de primer intervalo
do u = 1 , NumUniRC
    if ( m .eq. 46054 ) then
        continue
    end if
    beta = 0.0
    if ( EstadoCIURC( u ) .gt. 0 ) then
!       la unidad esta encendida en condiciones iniciales
        beta = 1.0
    endif
!   coeficiente de asignacion en el intervalo
    aaMILP ( k ) = 1.0
    jcolMILP ( k ) = IARC + u - 1
    k = k + 1
!   coeficiente de arranque en el intervalo
    aaMILP ( k ) = -1.0
    jcolMILP ( k ) = IARRC + u - 1
    k = k + 1
!   coeficiente de paro en el intervalo
    aaMILP ( k ) = 1.0
    jcolMILP ( k ) = IPRC + u - 1
    k = k + 1
!   coeficiente de asignacion durante aranque en el intervalo
    aaMILP ( k ) = 1.0
    jcolMILP ( k ) = IADARC + u - 1
    k = k + 1
!   lados derechos de las restriciones
    m = m + 1
    bMILP ( m )   = 0.0 + beta
!   sentidos de las restriciones
    sMILP ( m ) = 'E'
!   apuntador al siguiente renglon
    irowMILP ( m + 1 ) = k
    write ( 779,* ) m,',', '"Transiciones factibles en intervalo inicial unidad de RC: '//trim(nombunirc(u))//'"'//'"'
enddo

write ( 777,* ) 'Inicia siguientes restricciones de transiciones factibles        :', m + 1

! siguientes restricciones de transiciones factibles
do u = 1 , NumUniRC
    do i = 2 , NTINTR
!       coeficiente de asignacion en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de asignacion en el intervalo anterior
        aaMILP ( k ) = -1.0
        jcolMILP ( k ) = IARC + u + (i-2)*NumUniRC - 1
        k = k + 1
!       coeficiente de arranque en el intervalo
        aaMILP ( k ) = -1.0
        jcolMILP ( k ) = IARRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de paro en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de asignacion durante aranque en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de asignacion durante aranque en el intervalo anterior
        aaMILP ( k ) = -1.0
        jcolMILP ( k ) = IADARC + u + (i-2)*NumUniRC - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        if ( m == 50256 ) then
            continue
        endif
        bMILP ( m )   = 0.0
!       sentidos de las restriciones
        sMILP ( m ) = 'E'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m,',', '"Transiciones factibles en siguientes intervalos unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo

write ( 777,* ) 'Inicia restricciones que obligan a un solo tipo de estado en U RC:', m + 1

! restricciones que obligan a un solo tipo de estado (arranque o paro)
! para todas las unidades
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de arranque en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IARRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       coeficiente de paro en el intervalo
        aaMILP ( k ) = 1.0
        jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m )   = 1.0
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m,',', '"Se obliga a un solo tipo de estado unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de maximo numero de paros en unidades de RC :', m + 1

! restricciones de maximo numero de paros en unidades de RC
! para todas las unidades
do u = 1 , NumUniRC
    IntIni = 1
!   para todos los dias del horizonte
    do dia = 1, DURDIA
!       para todos los intervalos del dia
        do intervalo = IntIni, IntIni + intdia (dia) - 1
!           coeficiente de paro en el intervalo
            aaMILP ( k ) = 1.0
            jcolMILP ( k ) = IPRC + u + (intervalo-1)*NumUniRC - 1
            k = k + 1
        enddo
!       lados derechos de las restriciones
        m = m + 1
!       bMILP ( m )   = NumMaxParoURC ( u )
        bMILP ( m )   = max ( 0, NumMaxParoURC ( u ) - NoActParRC ( u ) )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) dia
        write ( 779,* ) m,',', '"Maximo numero de paros unidad de RC: '//trim(nombunirc(u))//' dia: '//trim(leti)//'"'
        IntIni = IntIni + intdia ( dia )
    enddo
enddo

200 format (i3)
    
return
end

    
Subroutine TieMParoOperRC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de tiempos minimos de operacion y paro      *
! en unidades de rango continuo.                                      *
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

Integer k, i, l, m, u, TminOper, TminParo, inicio, fin, NoRest, tmin
character*3 leti

write ( 777,* ) 'Inicia restricciones de tiempos minimos de oper y paro en unid RC:', m + 1

! restricciones de tiempos minimos de operacion
do u = 1 , NumUniRC
    if ( u == 171 ) then
        continue
    endif
    TminOper = TminOperURC(u)
	TminParo = TminParoURC(u)
!   si es unidad en operacion
    if ( EstadoCIURC(u) .eq. 1 ) then
        inicio = 1
        if ( NumHCIURC(u) .lt. TminOper ) then
!           para las horas que le restan de operacion
            fin = TminOper - NumHCIURC(u)
            if ( fin .ge. NTINTR ) then
                fin = NTINTR
            endif
            do i = 1 , fin
!               si es unidad disponible en este periodo
	            if ( DispoURC ( u , i ) .gt. 0 ) then
!                   si es unidad no asignable en este periodo
!	                if ( AsignURC ( u , i ) .eq. 0 ) then
!                       la unidad debe estar encendida
                        lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
                        ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
!                    endif
                else
                    lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                    ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                endif
 		    enddo
		    inicio = i
        endif
!       periodos siguientes de operacion
        do i = inicio , NTINTR
!           si es una unidad disponible y asignable
            if ( DispoURC ( u, i ) .gt. 0 .and. AsignURC ( u, i ) .ge. 0 ) then
		        tmin = TiempoArraURC ( u ) + TminOper
		        if ( i + tmin -1 .gt. NTINTR ) then
		   	        tmin = tmin - ( i + TiempoArraURC ( u ) + TminOper - 1 - NTINTR )
			    endif
			    NoRest = 0
	            do l = i , i + tmin-1
!                   si la unidad esta no disponible
                    if (  DispoURC ( u, l ) .eq. 0 ) then
                        exit
                        NoRest = 1
                    endif
               enddo
               if ( NoRest .eq. 0 .and. i + TiempoArraURC ( u ) .le. i + tmin - 1 ) then
	              do l = i + TiempoArraURC ( u ) , i + tmin - 1
!                     variable de asignacion
                      aaMILP ( k ) = 1.0
                      jcolMILP ( k ) = IARC + u + (l-1)*NumUniRC - 1
                      k = k + 1
                  enddo
!                 variable de arranque
                  aaMILP ( k ) = TiempoArraURC ( u ) - tmin
                  jcolMILP ( k ) = IARRC + u + (i-1)*NumUniRC - 1
	              k = k + 1
!                 lados derechos de las restriciones
	              m = m + 1
                  bMILP ( m )   = 0.0
!                 sentidos de las restriciones
                  sMILP ( m ) = 'G'
!                 apuntador al siguiente renglon
                  irowMILP ( m + 1 ) = k
                  write ( leti, 200 ) i
                  write ( 779,* ) m,',', '"Tiempos minimos de operacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
               endif
            endif
        enddo
!       tiempos minimos de paro
        do i = 1 , NTINTR
!           si es unidad disponible en este periodo
		    if ( DispoURC ( u, i ) .ge. 0  ) then
		       tmin = TminParo
		       if ( i + TminParo - 1 .gt. NTINTR ) then
			      tmin = tmin - ( i + TminParo - 1 - NTINTR )
			   end if
			   NoRest = 0
	           do l = i , i + tmin - 1
!                   si la unidad es no asignable
                    if ( AsignURC ( u, l ) .eq. 0 ) then
!                       la unidad debe estar encendida
!                        NoRest = 1
!                        exit
                    endif
               enddo
               if ( NoRest .eq. 0 .and. i .le. i + tmin - 1 ) then
                    do l = i , i + tmin - 1
!                       variable de asignacion
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IARC + u + (l-1)*NumUniRC - 1
                        k = k + 1
                    enddo
!                   variable de paro
                    aaMILP ( k ) = tmin
                    jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
	                k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m )   = tmin
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Tiempos minimos de paro unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                endif
            endif
        enddo

!       tiempos minimos de paro
        do i = 1 , NTINTR
!           si es unidad disponible en este periodo
		    if ( DispoURC ( u, i ) .ge. 0  ) then
		       tmin = TminParo
		       if ( i + TminParo - 1 .gt. NTINTR ) then
			      tmin = tmin - ( i + TminParo - 1 - NTINTR )
			   end if
			   NoRest = 0
	           do l = i , i + tmin - 1
!                   si la unidad es no asignable
                    if ( AsignURC ( u, l ) .eq. 0 ) then
!                       la unidad debe estar encendida
!                        NoRest = 1
!                        exit
                    endif
               enddo
               if ( NoRest .eq. 0 .and. i .le. i + tmin - 1 ) then
                    do l = i , i + tmin - 1
!                       variable de asignacion
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IADARC + u + (l-1)*NumUniRC - 1
                        k = k + 1
                    enddo
!                   variable de paro
                    aaMILP ( k ) = tmin
                    jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
	                k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m )   = tmin
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Tiempos minimos de paro antes de sincr unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                endif
            endif
        enddo
        
        
    else
!       si es unidad en paro
        if ( EstadoCIURC ( u ) .eq. 0 ) then
            inicio = 1
!           si la horas que lleva en paro son menores a su tiempo mínimo
	        if ( NumHCIURC(u) .lt. TminParo ) then
!               para las horas que le restan de paro mínimo
                fin = TminParo - NumHCIURC(u)
                if ( fin .ge. NTINTR ) then
                    fin = NTINTR
                endif
                do i = 1 , fin
!                   si es una unidad no asignable
	                if ( AsignURC ( u, i ) .eq. 0 ) then
!                       la unidad debe estar encendida
                        lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
                        ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
                        goto 100
                    else
                        lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                        ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0                        
                        lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                        ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0                        
			        endif
 	  	       enddo
		   	   inicio = i
            endif
100         continue
!           periodos siguientes de paro
            do i = inicio , NTINTR
!               si es una unidad disponible y asignable
                if ( DispoURC ( u, i ) .gt. 0 .and. AsignURC ( u, i ) .ge. 0 ) then
		            tmin = TminParo
		            if ( i + TminParo - 1 .gt. NTINTR ) then
			            tmin = tmin - ( i + TminParo - 1 - NTINTR )
		            end if
			        NoRest = 0
	                do l = i , i + tmin-1
!                       si es una unidad no asignable
                        if ( AsignURC ( u, l ) .eq. 0 ) then
!                           la unidad debe estar encendida
!                            NoRest = 1
!                            exit
                        endif
                    enddo
                    if ( NoRest .eq. 0 .and. i .le. i + tmin - 1 ) then
                        do l = i , i + tmin - 1
!                           variable de asignacion
                            aaMILP ( k ) = 1.0
                            jcolMILP ( k ) = IARC + u + (l-1)*NumUniRC - 1
                            k = k + 1
	                    enddo
!                       variable de paro
                        aaMILP ( k ) = tmin
                        jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
	                    k = k + 1
!                       lados derechos de las restriciones
	                    m = m + 1
                        bMILP ( m )   = tmin
!                       sentidos de las restriciones
                        sMILP ( m ) = 'L'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m,',', '"Tiempos minimos de paro unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                    endif
                endif
            enddo

!           periodos siguientes de paro
            do i = inicio , NTINTR
!               si es una unidad disponible y asignable
                if ( DispoURC ( u, i ) .gt. 0 .and. AsignURC ( u, i ) .ge. 0 ) then
		            tmin = TminParo
		            if ( i + TminParo - 1 .gt. NTINTR ) then
			            tmin = tmin - ( i + TminParo - 1 - NTINTR )
		            end if
			        NoRest = 0
	                do l = i , i + tmin-1
!                       si es una unidad no asignable
                        if ( AsignURC ( u, l ) .eq. 0 ) then
!                           la unidad debe estar encendida
!                            NoRest = 1
!                            exit
                        endif
                    enddo
                    if ( NoRest .eq. 0 .and. i .le. i + tmin - 1 ) then
                        do l = i , i + tmin - 1
!                           variable de asignacion
                            aaMILP ( k ) = 1.0
                            jcolMILP ( k ) = IADARC + u + (l-1)*NumUniRC - 1
                            k = k + 1
	                    enddo
!                       variable de paro
                        aaMILP ( k ) = tmin
                        jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
	                    k = k + 1
!                       lados derechos de las restriciones
	                    m = m + 1
                        bMILP ( m )   = tmin
!                       sentidos de las restriciones
                        sMILP ( m ) = 'L'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m,',', '"Tiempos minimos de paro antes de sincr unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                    endif
                endif
            enddo

!           tiempos minimos de operacion
            do i = 1, NTINTR
!               si es una unidad disponible y asignable
                if ( DispoURC ( u, i ) .gt. 0 .and. AsignURC ( u, i ) .ge. 0 ) then
		            tmin = TminOper + TiempoArraURC ( u )
		            if ( i + tmin - 1 .gt. NTINTR ) then
		   	            tmin = tmin - ( i + TiempoArraURC ( u ) + TminOper - 1 - NTINTR )
			        endif
			        NoRest = 0
	                do l = i , i + tmin-1
!                       si la unidad esta no disponible
                        if (  DispoURC ( u, l ) .eq. 0 ) then
!                            NoRest = 1
                            exit
                        endif
                    enddo
                    if ( NoRest .eq. 0 .and. i + TiempoArraURC ( u ) .le. i + tmin - 1 ) then
	                    do l = i + TiempoArraURC ( u ) , i + tmin - 1
!                           variable de asignacion
                            aaMILP ( k ) = 1.0
                            jcolMILP ( k ) = IARC + u + (l-1)*NumUniRC - 1
                            k = k + 1
                        enddo
!                       variable de arranque
                        aaMILP ( k ) = TiempoArraURC ( u ) - tmin
                        jcolMILP ( k ) = IARRC + u + (i-1)*NumUniRC - 1
	                    k = k + 1
!                       lados derechos de las restriciones
                        m = m + 1
                        if (m == 47827 ) then
                            continue
                        endif
                        bMILP ( m )   = 0.0
!                       sentidos de las restriciones
                        sMILP ( m ) = 'G'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m,',', '"Tiempos minimos de operacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                    endif
                endif
            enddo
        endif
    endif
enddo

200 format (i3)
    
return
end


Subroutine TieMParoOperRC_new ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de tiempos minimos de operacion y paro      *
! en unidades de rango continuo.                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2020                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, l, m, u, TminOper, TminParo, inicio, fin, tmin
character*3 leti

write ( 777,* ) 'Inicia restricciones de tiempos minimos de oper y paro en unid RC:', m + 1

! restricciones de tiempos minimos de operacion
do u = 1 , NumUniRC
    if ( u == 137 ) then
        continue
    endif
    TminOper = TminOperURC(u)
        TminParo = TminParoURC(u)
!   si es unidad en operacion
    if ( EstadoCIURC(u) .eq. 1 ) then
        inicio = 1
        if ( NumHCIURC(u) .lt. TminOper ) then
!           para las horas que le restan de operacion
            fin = TminOper - NumHCIURC(u)
            if ( fin .ge. NTINTR ) then
                fin = NTINTR
            endif
            do i = 1 , fin
!               si es unidad disponible en este periodo
                    if ( DispoURC ( u , i ) .gt. 0 ) then
!                   la unidad debe estar encendida
                    lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
                    ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
                else
                    lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                    ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                endif
                    enddo
                    inicio = i
        endif
!       periodos siguientes de operacion
        do i = inicio , NTINTR
            tmin = i - TiempoArraURC ( u ) - TminOper + 1
!            tmin = i - TminOper + 1
            if ( tmin .ge. 1 ) then
                do l = tmin, i
    !               variable de arranque
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IARRC + u + (l-1)*NumUniRC - 1
                    k = k + 1
                enddo
!               variable de asignacion
                aaMILP ( k ) = -1.0
                jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               variable de asignacion durante sincronizacion
                aaMILP ( k ) = -1.0
                jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 0.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Tiempos minimos de operacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        enddo

!       tiempos minimos de paro
        do i = 1 , NTINTR
            tmin = i - TminParo + 1
            if ( tmin .ge. 1 ) then
                do l = tmin, i
    !               variable de paro
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IPRC + u + (l-1)*NumUniRC - 1
                    k = k + 1
                enddo
    !           variable de asignacion
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 1.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Tiempos minimos de paro unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        enddo

!       tiempos minimos de paro
        do i = 1 , NTINTR
            tmin = i - TminParo + 1
            if ( tmin .ge. 1 ) then
                do l = tmin, i
    !               variable de paro
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IPRC + u + (l-1)*NumUniRC - 1
                    k = k + 1
                enddo
    !           variable de asignacion durante sincronizacion
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 1.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Tiempos minimos de paro antes de sincr unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        enddo
        
    else
!       si es unidad en paro
        if ( EstadoCIURC ( u ) .eq. 0 ) then
            inicio = 1
!           si la horas que lleva en paro son menores a su tiempo mínimo
                if ( NumHCIURC(u) .lt. TminParo ) then
!               para las horas que le restan de paro mínimo
                fin = TminParo - NumHCIURC(u)
                if ( fin .ge. NTINTR ) then
                    fin = NTINTR
                endif
                do i = 1 , fin
                    lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                    ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0                        
                    lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
                    ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0                        
                       enddo
                           inicio = i
            endif
!           periodos siguientes de paro
            do i = inicio , NTINTR
                tmin = i - TminParo + 1
                if ( tmin .ge. 1 ) then
                    do l = tmin, i
    !                   variable de paro
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IPRC + u + (l-1)*NumUniRC - 1
                        k = k + 1
                    enddo
    !               variable de asignacion
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               lados derechos de las restriciones
                    m = m + 1
                    bMILP ( m )   = 1.0
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Tiempos minimos de paro unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                endif
            enddo

!           periodos siguientes de paro
            do i = inicio , NTINTR
                tmin = i - TminParo + 1
                if ( tmin .ge. 1 ) then
                    do l = tmin, i
    !                   variable de paro
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IPRC + u + (l-1)*NumUniRC - 1
                        k = k + 1
                    enddo
    !               variable de asignacion durante sincronizacion
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               lados derechos de las restriciones
                    m = m + 1
                    bMILP ( m )   = 1.0
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Tiempos minimos de paro antes de sincr unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                endif
            enddo

!           tiempos minimos de operacion
            do i = 1, NTINTR
                tmin = i - TiempoArraURC ( u ) - TminOper + 1
!                tmin = i - TminOper + 1
                if ( tmin .ge. 1 ) then
                    do l = tmin, i
    !                   variable de arranque
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IARRC + u + (l-1)*NumUniRC - 1
                        k = k + 1
                    enddo
    !               variable de asignacion
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   variable de asignacion durante sincronizacion
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               lados derechos de las restriciones
                    m = m + 1
                    bMILP ( m )   = 0.0
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Tiempos minimos de operacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                endif
            enddo
        endif
    endif
enddo

200 format (i3)
    
return
end


Subroutine CondArranRC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de condiciones durante el arranque          *
! en unidades de rango continuo.                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2017                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, ii, ini, l, m, u, fin, entra
character*3 leti

write ( 777,* ) 'Inicia restricciones de condiciones durante el arranque en uni RC:', m + 1

! para cada unidad
do u = 1, NumUniRC
!   si la unidad tiene tiempo de arranque
    if ( TiempoArraURC ( u ) .gt. 0 ) then
!       para cada intervalo de planeacion
        do i = 1 , NTINTR
!           si la unidad es disponible y asignable
            if ( DispoURC ( u, i ) .eq. 1 .and. AsignURC ( u, i ) .eq. 1 ) then
!            if ( DispoURC ( u, i ) .eq. 1 ) then
!               restriccion de la asignabilidad durante el tiempo de arranque
100             fin = i + TiempoArraURC ( u ) - 1
                if (  fin .gt. NTINTR ) then
                    fin = NTINTR
                end if
                do l = i , fin
!                   variable de asignacion
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IARC + u + (l-1)*NumUniRC - 1
                    k = k + 1
                enddo
!               variable de arranque
                aaMILP ( k ) = TiempoArraURC ( u )
                jcolMILP ( k ) = IARRC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               lados derechos de las restriciones
	            m = m + 1
                if ( m == 4883 ) then
                    continue
                endif
                bMILP ( m )   = TiempoArraURC ( u )
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Condiciones durante el arranque en unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                
!               restriccion de la asignabilidad de sincronizacion durante el tiempo de arranque
                fin = i + TiempoArraURC ( u ) - 1
                if (  fin .gt. NTINTR-1 ) then
                    fin = NTINTR-1
                end if
                do l = i , fin
!                   variable de asignacion durante el arranque
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IADARC + u + (l-1)*NumUniRC - 1
                    k = k + 1
                enddo
!               variable de arranque
                aaMILP ( k ) = -TiempoArraURC ( u )
                jcolMILP ( k ) = IARRC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m )   = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Condiciones durante el arranque en unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            else
                ini = i - TiempoArraURC ( u )
                if ( ini .ge. 1 ) then
                    entra = 0
                    do ii = ini, i - 1
                        if ( DispoURC ( u, ii ) .eq. 0  .or. AsignURC ( u, ii ) .eq. 0 ) then
                            entra = entra + 1
                        endif
                    enddo
                    if ( entra .eq. 0 ) goto 100
                endif
            endif
        enddo
    else
!       para cada intervalo de planeacion
        do i = 1 , NTINTR
!           no puede entrar en sincronizacion la unidad
            ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
        enddo
    endif
enddo

! para cada unidad
do u = 1, NumUniRC
    if ( u == 128 ) then
        continue
    endif
!   si la unidad tiene tiempo de arranque
    if ( TiempoArraURC ( u ) .gt. 0.0 ) then
!       para cada intervalo de planeacion
        do i = 1 , NTINTR
!           si la unidad es disponible
            if ( DispoURC ( u, i ) .eq. 1 ) then
    !           restriccion de limite inferior en generacion de sincronizacion
    !           coeficiente de generacion durante sincronizacion 
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IGDARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           coeficiente de asignacion durante sincronizacion 
                aaMILP ( k ) = - PotSincURC ( u )
                jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 0.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'G'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Limite inferior de sincronizacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'

    !           restriccion de limite superior en generacion de sincronizacion
    !           coeficiente de generacion durante sincronizacion 
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IGDARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           coeficiente de asignacion durante sincronizacion 
                aaMILP ( k ) = -PotMinGRC ( u, i )
                jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                if ( m == 63315 ) then
                    continue
                endif
                bMILP ( m )   = 0.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Limite superior de sincronizacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    !           Para el primer intervalo
                if ( i .eq. 1 ) then            
    !               restriccion que indica que la unidad no puede sincronizarse si esta en operacion en el intervalo anterior
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               lados derechos de las restriciones
                    m = m + 1
                    if ( m == 69135 ) then
                        continue
                    endif
                    bMILP ( m )   = 1.0
                    if ( EstadoCIURC( u ) .le. 1 ) then
                        bMILP ( m )   = bMILP ( m ) - EstadoCIURC( u )
                    endif
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Indica que la unidad no puede sincronizarse si esta en operacion en el intervalo anterior unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                else
    !               restriccion que indica que la unidad no puede sincronizarse si esta en operacion en el intervalo anterior
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IARC + u + (i-2)*NumUniRC - 1
                    k = k + 1
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               lados derechos de las restriciones
                    m = m + 1
                    if ( m == 63315 ) then
                        continue
                    endif
                    bMILP ( m )   = 1.0
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Indica que la unidad no puede sincronizarse si esta en operacion en el intervalo anterior unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
                end if   
       
     !          restriccion que indica que la unidad no puede sincronizarse y estar en operacion al mismo tiempo
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 1.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Indica que la unidad no puede sincronizarse y estar en operacion al mismo tiempo unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'

    !           restricciones de rampa de subida durante el proceso de arranque
    !           si es el primer intervalo
                if ( i .eq. 1 ) then
    !               variable de generacion durante tiempo de arranque del intervalo actual
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IGDARC + u - 1
                    k = k + 1
    !               variable de asignacion durante tiempo de arranque del intervalo actual
                    aaMILP ( k ) = - PotSincURC ( u )
    !               si la unidad ya esta en proceso de arranque
                    if ( lbMILP ( IADARC + u - 1 ) .eq. 1.0 ) then
                        aaMILP ( k ) = - PotMinRRC ( u, 1 )
                    endif
                    jcolMILP ( k ) = IADARC + u - 1
                    k = k + 1
                else
    !               siguientes intervalos
    !               variable de generacion durante tiempo de arranque del intervalo actual
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IGDARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               variable de generacion durante tiempo de arranque del intervalo anterior
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k ) = IGDARC + u + (i-2)*NumUniRC - 1
                    k = k + 1
    !               variable de asignacion durante tiempo de arranque del intervalo actual
                    aaMILP ( k ) = - PotSincURC ( u )
    !               si la unidad ya esta en proceso de arranque
                    if ( lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) .eq. 1.0 ) then
                        aaMILP ( k ) = - PotMinRRC ( u, i )
                    endif
                    jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               variable de asignacion durante tiempo de arranque del intervalo anterior
    !               siempre y cuando la unidad no este ya en proceso de arranque
                    if ( abs(PotSincURC ( u ) - RampArraURC ( u )) .gt. 1e-5 .and. lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) .ne. 1.0 ) then
                        aaMILP ( k ) = PotSincURC ( u ) - RampArraURC ( u )
                        jcolMILP ( k ) = IADARC + u + (i-2)*NumUniRC - 1
                        k = k + 1
                    endif
                endif
    !           lados derechos de las restriciones
                m = m + 1
                if ( m == 71922 ) then
                    continue
                endif
                bMILP ( m )   = 0.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Rampa de subida durante el proceso de arranque unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif            
        enddo
    endif
enddo

write ( 777,* ) 'Inicia restricciones de aumento de rampa durante arranque de U RC:', m + 1

! para cada unidad
do u = 1, NumUniRC
!   si la unidad tiene tiempo de arranque
    if ( TiempoArraURC ( u ) .gt. 0 ) then
!       para cada intervalo de planeacion
        do i = 2 , NTINTR
            if ( m == 80544) then
                continue
            endif
!           si la unidad es disponible
            if ( DispoURC ( u, i ) .eq. 1 ) then
    !           variable de generacion durante tiempo de arranque del intervalo actual
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IGDARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           variable de generacion durante tiempo de arranque del intervalo anterior
                aaMILP ( k ) = -1.0
                jcolMILP ( k ) = IGDARC + u + (i-2)*NumUniRC - 1
                k = k + 1
    !           coeficiente de asignacion durante operacion 
                aaMILP ( k ) = PotMinGRC ( u, i )
                jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           variable de asignacion durante tiempo de arranque del intervalo actual
                aaMILP ( k ) = -RampArraURC ( u )
                jcolMILP ( k ) = IADARC + u + (i-2)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = -1.0e-5
    !           sentidos de las restriciones
                sMILP ( m ) = 'G'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Aumento de rampa durante arranque unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        enddo
    endif
enddo

200 format (i3)
    
return
end

    
Subroutine RampasOperRC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de rampas de operacion en unidades de rango *
! continuo.                                                           *
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

Integer k, kv, i, inicio, j, l, m, u, ant, act, s
real*8  beta
character*3 leti

write ( 777,* ) 'Inicia restricciones de rampas de subida para intervalo inicialRC:', m + 1

kv = IGABRC - 1
! restricciones de limites de rampas de subida para intervalo inicial
do u = 1 , NumUniRC
    inicio = 0
    do j = 1, NTINTR
        do l = 1, NumBloVRC( u, j )
            inicio = inicio + 1
        enddo
    enddo
!   si la unidad esta disponible y es cordinable
    if ( RampaSubURC ( u ) .gt. 0 .and. DispoURC ( u, 1 ) .ne. 0 .and. CoordURC ( u, 1 ) .ne. 0 ) then
	    beta = 0.0
!       si la unidad esta encendida en condiciones iniciales
        if ( EstadoCIURC ( u ) .gt. 0 ) then
!            beta = RampaSubURC ( u ) + GenCIURC ( u ) - PotMinGRC ( u, 1 )
            beta = GenCIURC ( u ) - PotMinGRC ( u, 1 )
            if ( abs(beta) .lt. 1e-3 ) then
                beta = 0.0
            endif
        endif
!       para todos los segmentos de curva de ofertas de venta
        do s = 1, NumBloVRC( u, 1 )
!           coeficiente de la variable de generacion aceptada en el bloque
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kv + s
            k = k + 1
        enddo
!       coeficiente de asignacion en el intervalo actual
        m = m + 1
!       lados derechos de las restriciones
!        bMILP ( m )   = beta
        bMILP ( m )   = beta + RampaSubURC ( u )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( 779,* ) m,',', '"Rampas de subida para intervalo inicial unidad de RC: '//trim(nombunirc(u))//'"'//'"'
    endif
    kv = kv + inicio
enddo

write ( 777,* ) 'Inicia siguientes restricciones de limites de rampas de subida RC:', m + 1

kv = IGABRC - 1
! siguientes restricciones de limites de rampas de subida
do u = 1 , NumUniRC
    inicio = 0
    do j = 1, NTINTR
        do l = 1, NumBloVRC( u, j )
            inicio = inicio + 1
        enddo
    enddo
    if ( RampaSubURC ( u ) .gt. 0.0 ) then
        ant = 0
        act = 0
        do i = 2 , NTINTR
            do l = 1, NumBloVRC( u, i -1 )
                act = act + 1
            enddo     
!           si la unidad esta disponible y es cordinable
            if ( DispoURC ( u, i ) .ne. 0 .and. CoordURC ( u, i ) .ne. 0 ) then
!               coeficiente de asignacion en el intervalo anterior
                if ( abs(PotMinGRC ( u, i ) - PotMinGRC ( u, i-1 ) - RampaSubURC ( u )) .gt. 1e-3 ) then
!                    aaMILP ( k ) = PotMinGRC ( u, i ) - PotMinGRC ( u, i-1 ) - RampaSubURC ( u )
                    aaMILP ( k ) = PotMinGRC ( u, i ) - PotMinGRC ( u, i-1 )
                    jcolMILP ( k ) = IARC + u + (i-2)*NumUniRC - 1
	                k = k + 1
                endif
!               para todos los segmentos de curva de ofertas de venta en el intervalo
                do s = 1, NumBloVRC( u, i )
!                   coeficiente de la variable de generacion aceptada en el bloque
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = kv + act + s
                    k = k + 1
                enddo
!               para todos los segmentos de curva de ofertas de venta en el intervalo anterior
                do s = 1, NumBloVRC( u, i-1 )
!                   coeficiente de la variable de generacion aceptada en el bloque
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = kv + ant + s
                    k = k + 1
                enddo
!               coeficiente de asignacion en el intervalo actual
!               lados derechos de las restriciones
	            m = m + 1
                if ( m == 5065 ) then
                    continue
                endif
!                bMILP ( m )   = 0.0
                bMILP ( m )   = RampaSubURC ( u )
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Rampas de subida en siguientes intervalos unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
            ant = act
        enddo
    endif
    kv = kv + inicio
enddo

write ( 777,* ) 'Inicia restricciones de rampas de bajada para intervalo inicialRC:', m + 1

kv = IGABRC - 1
! restricciones de limites de rampas de bajada para intervalo inicial
do u = 1 , NumUniRC
    inicio = 0
    do j = 1, NTINTR
        do l = 1, NumBloVRC( u, j )
            inicio = inicio + 1
        enddo
    enddo
!   si la unidad esta disponible y es cordinable
    if ( RampaBajURC ( u ) .gt. 0 .and. DispoURC ( u, 1 ) .ne. 0 .and. CoordURC ( u, 1 ) .ne. 0 ) then
	    beta = 0.0
!       si la unidad esta encendida en condiciones iniciales
        if ( EstadoCIURC ( u ) .gt. 0 ) then
            beta = PotMinGRC ( u, 1 ) - GenCIURC ( u )
        endif
!       coeficiente de asignacion
!        aaMILP ( k ) = - RampaBajURC ( u )
!        jcolMILP ( k ) = IARC + u - 1
!        k = k + 1
!       coeficiente de paro
!        aaMILP ( k ) = - RampaBajURC ( u )
!        jcolMILP ( k ) = IPRC + u - 1
!        k = k + 1
!       para todos los segmentos de curva de ofertas de venta
        do s = 1, NumBloVRC( u, 1 )
!           coeficiente de la variable de generacion aceptada en el bloque
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = kv + s
            k = k + 1
        enddo
        m = m + 1
        if ( m == 67909 ) then
            continue
        endif
!       lados derechos de las restriciones
!        bMILP ( m )   = beta
        bMILP ( m )   = beta + RampaBajURC ( u )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( 779,* ) m,',', '"Rampas de bajada para intervalo inicial unidad de RC: '//trim(nombunirc(u))//'"'//'"'
    endif
    kv = kv + inicio
enddo

write ( 777,* ) 'Inicia siguientes restricciones de limites de rampas de bajada RC:', m + 1

kv = IGABRC - 1
! siguientes restricciones de limites de rampas de subida
do u = 1 , NumUniRC
    inicio = 0
    do j = 1, NTINTR
        do l = 1, NumBloVRC( u, j )
            inicio = inicio + 1
        enddo
    enddo
    if ( RampaBajURC ( u ) .gt. 0.0 ) then
        ant = 0
        act = 0
        do i = 2 , NTINTR
            do l = 1, NumBloVRC( u, i -1 )
                act = act + 1
            enddo     
!           si la unidad esta disponible y es cordinable
            if ( DispoURC ( u, i ) .ne. 0 .and. CoordURC ( u, i ) .ne. 0 ) then
!               coeficiente de asignacion en el intervalo anterior
                if ( abs(PotMinGRC ( u, i-1 ) - PotMinGRC ( u, i )) .gt. 1e-3 ) then
                    aaMILP ( k ) = PotMinGRC ( u, i-1 ) - PotMinGRC ( u, i )
                    jcolMILP ( k ) = IARC + u + (i-2)*NumUniRC - 1
	                k = k + 1
                endif
!               coeficiente de asignacion en el intervalo actual
!                aaMILP ( k ) = - RampaBajURC ( u )
!                jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
!	             k = k + 1
!               coeficiente de paro
!                aaMILP ( k ) = - RampaBajURC ( u )
!                jcolMILP ( k ) = IPRC + u + (i-1)*NumUniRC - 1
!                k = k + 1
!               para todos los segmentos de curva de ofertas de venta en el intervalo
                do s = 1, NumBloVRC( u, i )
!                   coeficiente de la variable de generacion aceptada en el bloque
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = kv + act + s
                    k = k + 1
                enddo
!               para todos los segmentos de curva de ofertas de venta en el intervalo anterior
                do s = 1, NumBloVRC( u, i-1 )
!                   coeficiente de la variable de generacion aceptada en el bloque
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = kv + ant + s
                    k = k + 1
                enddo
!               lados derechos de las restriciones
	            m = m + 1
!                bMILP ( m )   = 0.0
                bMILP ( m )   = RampaBajURC ( u )
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Rampas de bajada en siguientes intervalos unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
            ant = act
        enddo
    endif
    kv = kv + inicio
enddo

200 format (i3)
    
return
end


Subroutine CostVarArrRC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de costos variables de arranque en unidades *
! de rango continuo.                                                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Diciembre de 2014                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, ka, i, s, m, u, tmin, tmax, tau, ir, inicio, numci
character*3 leti, lets

! restricciones de tipos de arranque 
write ( 777,* ) 'Inicia restricciones de costos variables de arranque             :', m + 1
ka = IBOARC - 1
do u = 1 , NumUniRC
    if ( u == 31 ) then
        continue
    endif
    inicio = NmBloArrURC ( u )*NTINTR
!   si existen segmentos de arranque
    if ( NmBloArrURC ( u ) .gt. 0 ) then
        do i = 1 , NTINTR
!           si la unidad es disponible y asignable
!            if ( DispoURC ( u, i ) .eq. 1 .and. AsignURC ( u, i ) .eq. 1 ) then
            if ( DispoURC ( u, i ) .eq. 1 ) then
    !           para todos los segmentos de arranque, excepto el final
                do s = 1, NmBloArrURC ( u ) - 1
    !               coeficiente de la variable de tipo de arranque
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = ka + (i-1)*NmBloArrURC ( u ) + s
                    k = k + 1
    !               subconjunto I'
                    tmin = i - TieminicioarrRCS ( u, s+1 ) + 1
                    tmax = i - TieminicioarrRCS ( u, s )
                    if ( tmax .ge. i ) then
                       tmax = i - 1
                    end if
	                m = m + 1
    !               lados derechos de las restriciones
                    bMILP ( m ) = 0.0
                    ir = 0
		            if ( ( tmin .gt. 0 .and. tmax .gt. 0 ) .or. ( tmin .le.0 .and. tmax .gt.0 ) ) then
		                if ( tmin .le.0 .and. tmax .gt.0 ) then
		                    tmin = 1
		                endif
		                do tau = tmin, tmax
    !                       coeficiente de la variable de paro
    !                       si la unidad esta disponible
                            ir = 1
                            if ( tau - 1 .ge. 1 ) then
                                if ( DispoURC ( u, tau - 1 ) .eq. 0  ) then
                                   ir = 0
                                endif
                            else
                                if ( EstadoCIURC ( u ) .eq. 0  ) then
                                   ir = 0
                                endif
                            endif
!                            if ( ir .eq. 1 .and. AsignURC ( u, tau ) .gt. 0 ) then
                                aaMILP ( k ) = - 1.0
                                jcolMILP ( k  ) = IPRC + u + (tau-1)*NumUniRC - 1
                                k = k + 1
!                            end if
		                enddo
                    endif
                    tmin = i - TieminicioarrRCS ( u, s+1 ) + 1
                    tmax = i - TieminicioarrRCS ( u, s )
                    numci = NumHCIURC ( u )
                    if ( tmin .le.0 .or. tmax .le.0 ) then
    !                   si la unidad esta en paro en condiciones iniciales
                        if ( EstadoCIURC ( u ) .eq. 0 ) then !!! JLCM
!                        if ( EstadoCIURC ( u ) .eq. 0 .or. ir .eq. 0 ) then
                            if ( ir .eq. 0 .and. EstadoCIURC ( u ) .ne. 0 ) then
                                numci = 0
                            endif
                            tmax = TieminicioarrRCS ( u, s+1 ) - 1
                            tmin = TieminicioarrRCS ( u, s )
!                            if ( ( numci + i - 1 ) .ge. tmin .and. ( numci + i - 1 ) .le. tmax ) then
                            if ( ( NumHCIURC ( u ) + i - 1 ) .ge. tmin .and. ( NumHCIURC ( u ) + i - 1 ) .le. tmax ) then
    !                           lado derecho de la restricion
                                bMILP ( m ) = 1.0
                            endif
                        endif
                    endif
             
    !               sentidos de la restricion
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( lets, 200 ) s
                    write ( 779,* ) m,',', '"Costos variables de arranque unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//' bloque '//trim(lets)//'"'//'"'
                enddo
          
    !           coeficiente de la variable de arranque del ultimo segmento
                aaMILP ( k ) = 1.0
                jcolMILP ( k  ) = ka + (i-1)*NmBloArrURC ( u ) + s
                k = k + 1
                tmax = i - TieminicioarrRCS ( u, s )
    !           lado derecho de la restricion
	            m = m + 1
                bMILP ( m ) = 0.0
	            if ( tmax .gt. 0 ) then
	                do tau = 1, tmax
    !                   coeficiente de la variable de paro
    !                   si la unidad esta disponible
                        ir = 1
                        if ( tau - 1 .ge. 1 ) then
                            if ( DispoURC ( u, tau - 1 ) .eq. 0  ) then
                                ir = 0
                            endif
                        else
                            if ( EstadoCIURC ( u ) .eq. 0  ) then
                                ir = 0
                            endif
                        endif
!                        if ( ir .eq. 1 .and. AsignURC ( u, tau ) .gt. 0 ) then
                            aaMILP ( k ) = - 1.0
                            jcolMILP ( k  ) = IPRC + u + (tau-1)*NumUniRC - 1
                            k = k + 1
!                        end if
                    enddo
                endif
    !           si la unidad esta en paro en condiciones iniciales
                if ( EstadoCIURC ( u ) .eq. 0 ) then
                    tmax = TieminicioarrRCS ( u, s )
                    if ( ( NumHCIURC ( u ) + i - 1 ) .ge. tmax ) then
    !                   lado derecho de la restricion
                        bMILP ( m ) = 1.0
                    endif
                endif

    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                    write ( lets, 200 ) NmBloArrURC ( u )
                    write ( 779,* ) m,',', '"Costos variables de arranque unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//' bloque '//trim(lets)//'"'//'"'
            endif
        enddo
    endif
    ka = ka + inicio
enddo

! restricciones que asocian la variable de arranque con los segmentos de arranque
write ( 777,* ) 'Inicia restricciones de arranque con segmentos de arranque       :', m + 1
ka = IBOARC - 1
do u = 1 , NumUniRC
    inicio = NmBloArrURC ( u )*NTINTR
!   si existen segmentos de arranque
    if ( NmBloArrURC ( u ) .gt. 0 ) then
        do i = 1 , NTINTR
!           si la unidad es disponible
            if ( DispoURC ( u, i ) .eq. 1 ) then
    !           para todos los segmentos de arranque
                do s = 1,  NmBloArrURC ( u )
    !               coeficientes de las variables de segmentos de arranque
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k  ) = ka + (i-1)*NmBloArrURC ( u ) + s
                    k = k + 1		       
                enddo
    !           coeficiente de la variable de arranque
                aaMILP ( k ) = 1.0
                jcolMILP ( k  ) = IARRC + u + (i-1)*NumUniRC - 1
                k = k + 1
    !           lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = 0.0
    !           sentidos de las restriciones
                sMILP ( m ) = 'L'
    !           apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Asocian la variable de arranque con los segmentos de arranque unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        enddo
    endif
    ka = ka + inicio
enddo

200 format (i3)
    
return
end
     

Subroutine EstadoUniRC
! ---------------------------------------------------------------------
! Restricciones sobre estados de las unidades de rango continuo       *
! en el problema de asignacion (MILP).                                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2017                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, u, j, nint

! para las unidades de rango continuo
do u = 1 , NumUniRC
    if ( u == 171 ) then
        continue
    endif
    do i = 1 , NTINTR
!       si es unidad no disponible en este periodo
        if ( DispoURC ( u , i ) .eq. 0 ) then
!           la unidad no puede generar
            lbMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = 0.0
            ubMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           la unidad no se puede operar
            lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
            ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           la unidad no se puede sincronizar
            lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
            ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           si es unidad disponible y no asignables en este periodo
        else
            if ( AsignURC ( u , i ) .eq. 0 ) then
!               la unidad debe operar
                lbMILP ( IARC + u + (i-1)*NumUniRC -1 ) = 1.0
                ubMILP ( IARC + u + (i-1)*NumUniRC -1 ) = 1.0
            endif
        endif
!       Si es un EXPOST
        if ( TipoEjecu .eq. 2 ) then
            lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
            ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0.0
            lbMILP ( IGDARC + u + (i-1)*NumUniRC - 1 ) = 0.0
            ubMILP ( IGDARC + u + (i-1)*NumUniRC - 1 ) = 0.0
        endif            
    enddo
!   Si la unidad esta arrancando y no es un EXPOST
    if ( EstadoCIURC ( u ) .eq. 11 .and. TipoEjecu .ne. 2 ) then
!       tiempo restante que falta para sincronizarse
        do i = 1 , TiempoArraURC ( u ) - NumHCIURC ( u )
            lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 1.0
            ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 1.0
        enddo
        nint = min( TminOperURC(u), NTINTR - i )
!       tiempo que debe permancer encendida para cumplir su tiempo minimo de operacion dentro del horizonte
        do j = 1 , nint
            lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
            ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1.0
            i = i + 1
        enddo
    endif            
enddo

return
end

    
Subroutine LimResURC ( k, m )
! ---------------------------------------------------------------------
! Restricciones reserva rodante de diez minutos de unidades de rango  *
! continuo, en el problema de asignacion (MILP).                      *
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

INTEGER   i, k, m, u, unidad, ro
real*8    maximo, minimo
character*3 leti


! restricciones de limites de reserva rodante de 10 minutos
write ( 777,* ) 'Inicia restricciones de reserva rodante de 10 minutos U de R Cont:', m + 1
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible y cordinable en este periodo
!        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 ) then
!       si es unidad disponible en este periodo
        if ( DispoURC ( u , i ) .eq. 1 ) then
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = - CalOferResR10RC ( u, i )
            jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva rodante de 10 minutos unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva rodante suplementaria
write ( 777,* ) 'Inicia restricciones de reserva suplementaria para unidades de RC:', m + 1
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible y cordinable en este periodo
!        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 ) then
!       si es unidad disponible en este periodo
        if ( DispoURC ( u , i ) .eq. 1 ) then
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRRSRC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = - CalOferResRxRC ( u, i )
            jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva suplementaria unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva de regulacion secundaria
write ( 777,* ) 'Inicia restricciones de limite maximo reserva de regulacion   URC:', m + 1
unidad = 0
! para las unidades de rango continuo
do u = 1 , NumUniRC
    if ( u .eq. 189 ) then
        continue
    endif
!   si la unidad tiene bandas prohibidas
    if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
        unidad = unidad + 1
    endif
!   para todos los intervalos
    do i = 1 , NTINTR
!       si la unidad oferta reserva de regulacion secundaria
        if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!           si es unidad disponible y cordinable en este periodo
!            if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 ) then
            if ( DispoURC ( u , i ) .eq. 1 ) then
!               coeficiente de la variable de reserva
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               si se permite hacer uso de reserva
                if ( TipoEjecu .le. 1 ) then
!                   coeficiente de variable de uso
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                    k = k + 1
                endif
!               si se desea considerar bandas prohibidas y la unidad las tiene
                if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
!                   para los rangos de la unidad
                    do ro = 1, NoRaOpRC ( u )
!                       coeficiente de variable de asignacion de rango operativo
                        aaMILP ( k ) = -CalOferRegRORC  ( u, ro, i )
!                       columna asociada
                        jcolMILP( k ) = INVRERORC + IRERORC ( u, i ) + ro - 1
                        k = k + 1
                    enddo
!                   Si la unidad tiene dos rangos de regulacion
                    if ( NoRaOpRC ( u ) .eq. 3 .and. (RaRegSupRC  ( u, 1, i ) .eq. RaRegInfRC  ( u, 2, i )) .and. &
                         (RaRegSupRC  ( u, 2, i ) .eq. RaRegInfRC  ( u, 3, i )) ) then
!                       la unidad no debe regular en el rango 2
                        ubMILP ( INVRERORC + IRERORC ( u, i ) + 1 ) = 0.0
                    endif
                else
!                   coeficiente de variable de asignacion de reserva
                    aaMILP ( k ) = - CalOferResRegRC ( u, i )
!                   columna asociada
                    jcolMILP( k ) = IVREGRC + INREGRC ( u, i ) - 1
                    k = k + 1
! 					si la solucion es en variables enteras
					if ( TipoProblema .eq. 1 ) then
                    	ctypeMILP ( IVREGRC + INREGRC ( u, i ) - 1 ) = 'B'
					endif
!                   cota inferior de la variable
                    lbMILP ( IVREGRC + INREGRC ( u, i ) - 1 ) = 0.0
!                   cota superior de la variable
                    ubMILP ( IVREGRC + INREGRC ( u, i ) - 1 ) = 1.0
                endif
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Limite maximo reserva regulacion de unidad RC  '//trim(nombunirc(u))//' intervalo: '//leti//'"'
            endif
        else
!           no existe reserva de regulacion para la unidad
            ubMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) = 0.0
        endif
   enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo reserva de regulacion   URC:', m + 1
unidad = 0
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   si la unidad tiene bandas prohibidas
    if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
        unidad = unidad + 1
    endif
!   para todos los intervalos
    do i = 1 , NTINTR
!       si la unidad oferta reserva de regulacion secundaria
        if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!           si es unidad disponible y cordinable en este periodo
!            if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 ) then
            if ( DispoURC ( u , i ) .eq. 1 ) then
!               coeficiente de la variable de reserva
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               si se permite hacer uso de reserva
                if ( TipoEjecu .le. 1 ) then
!                   coeficiente de variable de uso
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                    k = k + 1
                endif
!               si se desea considerar bandas prohibidas y la unidad las tiene
                if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
!                   para los rangos de la unidad
                    do ro = 1, NoRaOpRC ( u )
                        minimo = MrreURC ( u, i )
!                        if ( minimo .gt. CalOferRegRORC  ( u, ro, i ) ) then
!                            minimo = CalOferRegRORC  ( u, ro, i )
!                        endif
!                       coeficiente de variable de asignacion de rango operativo
                        aaMILP ( k ) = -minimo
!                       columna asociada
                        jcolMILP( k ) = INVRERORC + IRERORC ( u, i ) + ro - 1
                        k = k + 1
                    enddo
                else
                    minimo = MrreURC ( u, i )
!                    if ( minimo .gt. CalOferResRegRC ( u, i ) ) then
!                        minimo = CalOferResRegRC ( u, i )
!                    endif
!                   coeficiente de variable de asignacion de reserva
                    aaMILP ( k ) = - minimo
!                   columna asociada
                    jcolMILP( k ) = IVREGRC + INREGRC ( u, i ) - 1
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
                write ( 779,* ) m,',', '"Limite minimo reserva regulacion de unidad RC  '//trim(nombunirc(u))//' intervalo: '//leti//'"'
            endif
        endif
   enddo
enddo

!if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "BCA" ) then
if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "SIS" ) then
    ! restricciones de limites de reserva de regulacion secundaria y rodante de 10 minutos por rampa de 10 minutos
    write ( 777,* ) 'Inicia restricciones de reserva de reg y roda de 10 por rampa URC:', m + 1
    unidad = 0
    ! para las unidades de rango continuo
    do u = 1 , NumUniRC
    !   si la unidad tiene bandas prohibidas
        if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
            unidad = unidad + 1
        endif
    !   para todos los intervalos
        do i = 1 , NTINTR
    !       si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
                if ( DispoURC ( u , i ) .eq. 1 ) then
    !               coeficiente de la variable de reserva de regulacion
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               coeficiente de variable de uso de regulacion
                    aaMILP ( k ) = - 1.0
    !               columna asociada
                    jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               coeficiente de la variable de reserva rodante de diez
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               si se permite hacer uso de reserva en AUGC
                    if ( SiRelRod .eq. 1 ) then
    !                   coeficiente de variable de uso rodante
                        aaMILP ( k ) = - 1.0
    !                   columna asociada
                        jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
    !               lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = RamEmer10RC ( u )
    !               sentidos de las restriciones
                    sMILP ( m ) = 'L'
    !               apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m,',', '"Reserva de regulacion y rodante de 10 por rampa unidad de RC: '//trim(nombunirc(u))//' intervalo: '//leti//'"'
                endif
            endif
        enddo
    enddo
endif

! restricciones de limites de reserva rodante de diez minutos y suplementaria
write ( 777,* ) 'Inicia restricciones de reserva rodante de diez y suplementar URC:', m + 1
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible en este periodo
        if ( DispoURC ( u , i ) .eq. 1 ) then
!            if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "BCA" ) then
            if ( SiRegEnRod .eq. 1 .and. trim(nomsis(1)) .eq. "SIS" ) then
!               coeficiente de la variable de reserva de regulacion
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                k = k + 1
!               coeficiente de variable de uso de regulacion
                aaMILP ( k ) = - 1.0
!               columna asociada
                jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                k = k + 1
            endif
!           coeficiente de la variable de reserva rodante de diez
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de reserva suplementaria
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRRSRC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           si se permite hacer uso de reserva en AUGC
            if ( SiRelRod .eq. 1 ) then
!               coeficiente de variable de uso rodante
                aaMILP ( k ) = - 1.0
!               columna asociada
                jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
                k = k + 1
            endif
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = RamEmerxRC ( u )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva rodante de 10 minutos y suplementaria unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva no rodante de 10 minutos
write ( 777,* ) 'Inicia restricciones de reserva no rodante de 10 minutos para URC:', m + 1
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible, asignable  y cordinable en este periodo
        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 .and. AsignURC ( u , i ) .eq. 1 ) then
goto 777
!           limite inferior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva no rodante de 10 minutos unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
777 continue
!           limite superior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = OferResNR10RC ( u, i )
            jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion en sincronizacion
            aaMILP ( k  ) = OferResNR10RC ( u, i )
            jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = OferResNR10RC ( u, i )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva no rodante de 10 minutos unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva no rodante suplementaria
write ( 777,* ) 'Inicia restricciones de reserva no rodante suplementaria para URC:', m + 1
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible, asignable  y cordinable en este periodo
        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 .and. AsignURC ( u , i ) .eq. 1 ) then
goto 778
!           limite inferior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNRSRC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva no rodante suplementaria unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
778 continue
!           limite superior
!           coeficiente de la variable de reserva
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNRSRC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = OferResNRxRC ( u, i )
            jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion en sincronizacion
            aaMILP ( k  ) = OferResNRxRC ( u, i )
            jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = OferResNRxRC ( u, i )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva no rodante suplementaria unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
   enddo
enddo

! restricciones de limites de reserva no rodante de diez minutos y suplementaria
write ( 777,* ) 'Inicia restricciones de reserva no rodante de 10 min y suplem URC:', m + 1
! para las unidades de rango continuo
do u = 1 , NumUniRC
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible, asignable y cordinable en este periodo
        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 .and. AsignURC ( u , i ) .eq. 1 ) then
            maximo =  max ( OferResNR10RC ( u, i ), OferResNRxRC ( u, i ) )
!           coeficiente de la variable de reserva rodante de diez
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de reserva suplementaria
            aaMILP ( k  ) = 1.0
            jcolMILP ( k ) = IRNRSRC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion
            aaMILP ( k  ) = maximo
            jcolMILP ( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de asignacion en sincronizacion
            aaMILP ( k  ) = maximo
            jcolMILP ( k ) = IADARC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
	        m = m + 1
            bMILP ( m ) = maximo
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Reserva no rodante de 10 y suplementaria unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
   enddo
enddo

200 format (i3)
    
return
end



subroutine RangOperRC ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de asociacion entre asignacion de rango *
! y asignacion de operacion en unidades de rango continuo             *
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

write ( 777,* ) 'Inicia restricciones de asociacion asignacion rangos y operac RC :', m + 1

! unidades de rango continuo
do u = 1 , NumUniRC
    if ( NoRaOpRC ( u ) .gt. 0 ) then
!       para todos los intervalos
        do i = 1 , NTINTR
!           para los rangos de la unidad
            do ro = 1, NoRaOpRC ( u )
!               coeficiente de variable de asignacion de rango operativo
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2
                k = k + 1
            enddo
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
            k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'E'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Asociacion asignacion rangos y operacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        enddo
    endif
enddo

200 format (i3)
    
return
end    

subroutine GruposArranqueAsincrono ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de grupos de generadores con arranque       *
! no simultaneo                                                       *
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

integer i, j, k, m, n, p, u, u2, noentra
character*3 leti

write ( 777,* ) 'Inicia restricciones de arranque no simultaneo de unidades de RC :', m + 1

do p = 1, NumGruArr
    do i = 1, NTINTR
!       Se revisa si no existe asignación forzada para dos o mas unidades
        noentra = 0
        do j = 1, numunigruarr(p)
            u = unigruarr(p,j)
            if ( AsignURC (u, i) .eq. 0 ) then
                noentra = noentra + 1
            endif
        enddo
!       si no existe mas de una unidad no asignable en el grupo
!        if ( noentra .le. 1 ) then
!           Forma restriccion que no permite el arranque simultaneo de las unidades del grupo       
            do j = 1, numunigruarr(p)
                u = unigruarr(p,j)
                aaMILP(k) = 1
                jcolMILP(k) = IARRC + (i-1)*NumUniRC + u - 1
                k = k + 1
            enddo
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 1.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"No permite el arranque simultaneo de las unidades del grupo  '//trim(NomGruArr(p))//' intervalo: '//leti//'"'
       
!           Forma restriccion que impide la sincronización de otra unidad en la planta, 
!           hasta que acabe la sincronización de una ya arrancada
            do j = 1, numunigruarr(p)
                u = unigruarr(p,j)
                aaMILP(k) = -1
                jcolMILP(k) = IARC + (i-1)*NumUniRC + u - 1
                k = k + 1
                aaMILP(k) = 1.0
                jcolMILP(k) = IADARC + (i-1)*NumUniRC + u - 1
                k = k + 1
                do n = 1,  numunigruarr(p)
                    if ( j .ne. n ) then
                        u2 = unigruarr(p,n)
                        aaMILP(k) = 1.0
                        jcolMILP(k) = IARRC + (i-1)*NumUniRC + u2 - 1
                        k = k + 1
                    endif
                enddo
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 1.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m,',', '"Sincronizacion no simultanea de las unidades del grupo  '//trim(NomGruArr(p))//' intervalo: '//leti
                
            enddo
!        endif
    enddo
enddo

200 format(i3)
    
return
end


subroutine RestUPC_old ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de unidades de propiedad conjunta (UPC).    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo 2017                                                           *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE

implicit none

integer i, k, m, p, u
character*3 leti, letp

write ( 777,* ) 'Inicia restricciones de unidades de propiedad conjunta (UPS) RC  :', m + 1

do p = 1, NumUPC
    do i = 1, NTINTR
!       Unidad complementaria 1
        if ( indiceuc1 ( p ) .gt. 0 ) then
!           unidad principal
            u = indiceup ( p )
            aaMILP(k) = -1
            jcolMILP(k) = IARC + (i-1)*NumUniRC + u - 1
            k = k + 1
!           unidad complementaria 1
            u = indiceuc1 ( p )
            aaMILP(k) = 1
            jcolMILP(k) = IARC + (i-1)*NumUniRC + u - 1
            k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i; write ( letp, 200 ) p
            write ( 779,* ) m,',', '"Unidad complementaria 1 de propiedad conjunta del grupo '//trim(letp)//' intervalo: '//leti//'"'
        endif
!       Unidad complementaria 2
        if ( indiceuc2 ( p ) .gt. 0 ) then
!           unidad principal
            u = indiceup ( p )
            aaMILP(k) = -1
            jcolMILP(k) = IARC + (i-1)*NumUniRC + u - 1
            k = k + 1
!           unidad complementaria 2
            u = indiceuc2 ( p )
            aaMILP(k) = 1
            jcolMILP(k) = IARC + (i-1)*NumUniRC + u - 1
            k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i; write ( letp, 200 ) p
            write ( 779,* ) m,',', '"Unidad complementaria 2 de propiedad conjunta del grupo '//trim(letp)//' intervalo: '//leti//'"'
        endif       
    enddo
enddo

200 format(i3)
    
return
end


Subroutine TieMinRegRC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de tiempos minimos de regulacion para       *
! unidades de rango continuo.                                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2019                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, l, m, u, TminReg, tmin
character*3 leti

write ( 777,* ) 'Inicia restricciones de tiempos minimos de regulacion en unid RC:', m + 1

! restricciones de tiempos minimos de operacion
do u = 1 , NumUniRC
    if ( u == 137 ) then
        continue
    endif
    TminReg = 3
!   tiempos minimos de operacion
    do i = 1, NTINTR
!       si existe oferta de regulacion de la unidad
        if ( OferResRegRC ( u, i ) .gt. 0 ) then
            tmin = TminReg
            if ( i + tmin - 1 .gt. NTINTR ) then
                tmin = tmin - ( i + TminReg - 1 - NTINTR )
            endif
            do l = i, i + tmin - 1
!               variable de asignacion de regulacion en los intervalos
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IVREGRC + INREGRC ( u, l ) - 1
                k = k + 1
            enddo
!           variable de asignacion de regulacion en el intervalo
            aaMILP ( k ) = -tmin
            jcolMILP ( k ) = IVREGRC + INREGRC ( u, i ) - 1
            k = k + 1
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m )   = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Tiempos minimos de regulacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        endif
    enddo
enddo

200 format (i3)
    
return
end

subroutine RestUPC ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de unidades de propiedad conjunta (UPC).    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril 2020                                                          *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE

implicit none

integer i, k, m, p, u
character*3 leti, letp

write ( 777,* ) 'Inicia restricciones de unidades de propiedad conjunta (UPS) RC  :', m + 1

do p = 1, NumUPC
    do i = 1, NTINTR
       u = indiceup ( p )
!      coeficiente de aportacion a reserva rodante de 10 minutos
       aaMILP ( k ) = 1.0
!      columna asociada
       jcolMILP( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
       k = k + 1
!      coeficiente de aportacion a reserva rodante suplementaria
       aaMILP ( k ) = 1.0
!      columna asociada
       jcolMILP( k ) = IRRSRC + u + (i-1)*NumUniRC - 1
       k = k + 1
!      coeficiente de aportacion a reserva de regulacion
       aaMILP ( k ) = 1.0
!      columna asociada
       jcolMILP( k ) = IRRERC + u + (i-1)*NumUniRC - 1
       k = k + 1
       m = m + 1
       bMILP ( m ) = ofcapupc ( p, i )
!      sentido de la restriccion
       sMILP ( m ) = 'L'
!      inicio de la siguiente restriccion
       irowMILP ( m + 1 ) = k
       write ( leti, 200 ) i; write ( letp, 200 ) p
       write ( 779,* ) m,',', '"Limite de reserva rodante de unidad de propiedad conjunta=>['//trim(letp)//'] intervalo: '//leti//'"'
    enddo
enddo

200 format(i3)
    
return
end
