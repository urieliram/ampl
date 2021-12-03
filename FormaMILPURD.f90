  
! ---------------------------------------------------------------------
! Forman las restricciones del problema de asignacion y despacho de   *
! unidades (MILP), de rango discontinuo.                              *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2017                                                     *
! ---------------------------------------------------------------------
Subroutine RestURD ( k, m, sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

Integer k, m, sistema

! se forma la restriccion de nivel de generacion en unidades de rango discontinuo
call NivelGenRD ( k, m ) 

! se forma la restriccion de segmentos de ofertas de venta en unidades de rango discontinuo
call SegVenRD ( k, m )

! se cosidera la asignabilidad y disponibilidad del escenario para unidades de rango discontinuo
call EstadoUniRD

! se forman restricciones de limites de generacion en unidades de rango discontinuo
call LimGenORD ( k, m )

! se forma restriccion de transiciones factibles entre modos de operacion
call TFactModoOperRD ( k, m )

! Si no es EXPOST
if ( TipoEjecu .ne. 2 ) then

!   se forman restricciones de tiempos minimos de operacion y paro en unidades de rango discontinuo
    call TieMParoOperRD ( k, m )

!   se forman restricciones de condiciones durante el arranque en unidades de rango discontinuo
    call CondArranRD ( k, m )

endif

! se forman restricciones de rampas de operacion en unidades de rango discontinuo
call RampasOperRD ( k, m )

! se forman restricciones de costos variables de arranque en unidades de rango discontinuo
call CostVarArrRD ( k, m )

! si el problema contiene ofertas de reserva por zona o sistema
if ( SiOferComResZona .gt. 0 .or. SiOferComResSis .gt. 0 ) then
!   se forman restricciones de limites de reserva en unidades de rango discontinuo
    call LimResURD ( k, m )
endif

return
end

        
subroutine NivelGenRD ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de nivel de generacion en unidades de       *
! rango discontinuo                                                   *
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

Integer k, kv, m, modo, i, u, s
character*3 leti

write ( 777,* ) 'Inicia restricciones de nivel de generacion en unidades de RangoD:', m + 1

! unidades de rango discontinuo
kv = IGABRD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 2, NumModRD(u)
!           si son ofertas de costo a generacion minima
		    if ( TipoOferta .eq. 1 ) then
!               para todos los segmentos de curva de ofertas de venta
                do s = 1, NumBloVRD( u, modo, i )
!                   coeficiente de la variable de generacion en el segmento
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = kv
                    k = k + 1
                    kv = kv + 1
                enddo
!           si son ofertas de costo en vacio
		    else
                if ( NumBloVRD( u, modo, i ) .gt. 0 ) then
                    kv = kv + 1
                endif
                kv = kv + 1
!               para todos los segmentos de curva de ofertas de venta
                do s = 2, NumBloVRD( u, modo, i )
!                   coeficiente de la variable de generacion en el segmento
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = kv
                    k = k + 1
                    kv = kv + 1
                enddo
            endif
!           coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IGRD + INIURDI ( u, i ) + modo - 1
            k = k + 1
!           si la unidad tiene tiempo de arranque en este modo
            if ( TransFacti  ( u, 1, modo ) .gt. 0.0 ) then
!               coeficiente de la variable de generacion durante arranque
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
            endif
!           coeficiente de la variable de asignacion
            aaMILP ( k ) = -PotMinGRD( u, modo, i )
!           columna asociada
            jcolMILP( k ) = IARD + INIURDI ( u, i ) + modo - 1
            k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'E'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Nivel de generacion operativo unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
        enddo
    enddo
enddo

200 format (i3)
    
return
end
    

subroutine SegVenRD ( k, m )
! ---------------------------------------------------------------------
! Se forma la restriccion de segmentos de ofertas de venta en         *
! unidades de rango discontinuo.                                      *
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

Integer i, k, m, modo, u, s, kv
character*3 leti

! unidades de rango discontinuo
kv = IGABRD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 1, NumModRD(u)
!           si son ofertas de costo a generacion minima
		    if ( TipoOferta .eq. 1 ) then
!               para todos los segmentos de curva de ofertas de venta
                do s = 1, NumBloVRD( u, modo, i )
!                   limite superior de la variable de generacion en el segmento
                    lbMILP ( kv ) = 0.0
!                   coeficiente de la variable de generacion
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = kv
                    k = k + 1
                    kv = kv + 1
!                   coeficiente de variable de asignacion
                    aaMILP ( k ) = -OferVenEnerRD ( u, modo, s, i )
!                   columna asociada
                    jcolMILP( k ) = IARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
                    m = m + 1
!                   lado derecho de la restriccion
                    bMILP ( m ) = 0.0
!                   sentido de la restriccion
                    sMILP ( m ) = 'L'
!                   inicio de la siguiente restriccion
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Segmentos de venta de unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                enddo
!           si son ofertas de costo en vacio
		    else
                if ( NumBloVRD( u, modo, i ) .gt. 0 ) then
!                   primer segmento
!                   coeficiente de la variable de generacion durante arranque
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de variable de asignacion durante arranque
                    aaMILP ( k ) = -OferVenEnerRD ( u, modo, 1, i )
!                   columna asociada
                    jcolMILP( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
                    m = m + 1
!                   lado derecho de la restriccion
                    bMILP ( m ) = 0.0
!                   sentido de la restriccion
                    sMILP ( m ) = 'L'
!                   inicio de la siguiente restriccion
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Segmentos de venta de unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                    kv = kv + 1
!                   para todos los segmentos de curva de ofertas de venta
                    do s = 2, NumBloVRD( u, modo, i )
!                       limite superior de la variable de generacion en el segmento
                        lbMILP ( kv ) = 0.0
!                       coeficiente de la variable de generacion
                        aaMILP ( k ) = 1.0
!                       columna asociada
                        jcolMILP( k ) = kv
                        k = k + 1
                        kv = kv + 1
!                       coeficiente de variable de asignacion
                        aaMILP ( k ) = -OferVenEnerRD ( u, modo, s, i )
!                       columna asociada
                        jcolMILP( k ) = IARD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                        m = m + 1
!                       lado derecho de la restriccion
                        bMILP ( m ) = 0.0
!                       sentido de la restriccion
                        sMILP ( m ) = 'L'
!                       inicio de la siguiente restriccion
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m, ',', '"Segmentos de venta de unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                    enddo
                endif
            endif
        enddo
    enddo
enddo

200 format (i3)
    
return
end    

    

subroutine LimGenORD ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de generacion operativo en       *
! unidades de rango discontinuo.                                      *
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

Integer k, kg, kr10, krs, kre, ka, kada, i, m, modo, u
character*3 leti

! inicio de este tipo de restricciones
IRLMRD = m + 1
write ( 777,* ) 'Inicia restricciones de limite maximo de generacion operativo URD:', m + 1

! unidades de rango discontinuo
kg = IGRD
kr10 = IRR10RD
krs = IRRSRD
kre = IRRERD
ka = IARD
kada = IADARD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 1, NumModRD(u)
            if ( m == 10413 ) then 
                continue
            endif
!           coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kg
            k = k + 1
            kg = kg + 1
!           coeficiente de aportacion a reserva rodante de 10 minutos
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kr10
            k = k + 1
            kr10 = kr10 + 1
!           coeficiente de aportacion a reserva rodante suplementaria
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = krs
            krs = krs + 1
            k = k + 1
!           coeficiente de aportacion a reserva de regulacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kre
            k = k + 1
            kre = kre + 1
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -PotMaxGRD ( u, modo, i )
!           columna asociada
            jcolMILP( k ) = ka
            k = k + 1
            ka = ka + 1
!           coeficiente de variable de asignacion durante el arranque
            aaMILP ( k ) = - PotMinGRD ( u, modo, i )
!           columna asociada
            jcolMILP( k ) = kada
            k = k + 1
            kada = kada + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Limite maximo de generacion operativo unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
        enddo
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite maximo con reserva regulacion  URD:', m + 1

! unidades de rango discontinuo
kg = IGRD
ka = IARD
kre = IRRERD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 1, NumModRD(u)
            if ( m == 10413 ) then 
                continue
            endif
!           coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kg
            k = k + 1
            kg = kg + 1
!           coeficiente de aportacion a reserva de regulacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kre
            k = k + 1
            kre = kre + 1
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -PotMaxGRD ( u, modo, i )
!           columna asociada
            jcolMILP( k ) = ka
            k = k + 1
            ka = ka + 1
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegRD ( u, modo, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = - PotMaxRRD ( u, modo, i ) + PotMaxGRD ( u, modo, i )
!               columna asociada
                jcolMILP( k ) = IVREGRD + INREGRD ( u, modo, i ) - 1
!               variable de asignacion de reserva
                ctypeMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 'B'
!               cota inferior de la variable
                lbMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1) = 0.0
!               cota superior de la variable
                ubMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 1.0
                k = k + 1
            endif
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Limite maximo con reserva regulacion URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
        enddo
    enddo
enddo

! unidades de rango discontinuo
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 1, NumModRD(u)
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegRD ( u, modo, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IVREGRD + INREGRD ( u, modo, i ) - 1
                k = k + 1
!               coeficiente de variable de asignacion
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
                m = m + 1
!               lado derecho de la restriccion
                bMILP ( m ) = 0.0
!               sentido de la restriccion
                sMILP ( m ) = 'L'
!               inicio de la siguiente restriccion
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Asignacion de reserva y operacion en URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            endif
        enddo
    enddo
enddo
           

write ( 777,* ) 'Inicia restricciones de limite minimo de generacion operativo URD:', m + 1

! unidades de rango discontinuo
kg = IGRD
ka = IARD
kada = IADARD
kre = IRRERD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 1, NumModRD(u)
            if ( m == 10413 ) then 
                continue
            endif
!           coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kg
            k = k + 1
            kg = kg + 1
!           coeficiente de aportacion a reserva de regulacion
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = kre
            k = k + 1
            kre = kre + 1
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -PotMinGRD ( u, modo, i )
!           columna asociada
            jcolMILP( k ) = ka
            k = k + 1
            ka = ka + 1
!           coeficiente de variable de asignacion durante el arranque
            aaMILP ( k ) = - PotSincURD ( u, modo )
    !       si la unidad ya esta en proceso de arranque
            if ( lbMILP ( kada ) .eq. 1.0 ) then
                aaMILP ( k ) = - (GenCIURD ( u, modo ) + RampArraURD ( u, modo ))
            endif
!           columna asociada
            jcolMILP( k ) = kada
            k = k + 1
            kada = kada + 1
!           si la unidad oferta reserva de regulacion secundaria
            if ( CalOferResRegRD ( u, modo, i ) .gt. 0.0 ) then
!               coeficiente de variable de asignacion de reserva
                aaMILP ( k ) = - PotMinRRD ( u, modo, i ) + PotMinGRD ( u, modo, i )
!               columna asociada
                jcolMILP( k ) = IVREGRD + INREGRD ( u, modo, i ) - 1
!               variable de asignacion de reserva
                ctypeMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 'B'
!               cota inferior de la variable
                lbMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1) = 0.0
!               cota superior de la variable
                ubMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 1.0
                k = k + 1
            endif
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'G'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Limite minimo de generacion operativo unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
        enddo
    enddo
enddo

200 format (i3)
    
return
end    

    
  
Subroutine TieMParoOperRD ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de tiempos minimos de operacion y paro      *
! en unidades de rango discontinuo.                                   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, l, m, u, TminOper, inicio, fin, NoRest, tmin, &
        modo, modoO, modoD, bandera, intervalo, beta, t
character*3 leti

write ( 777,* ) 'Inicia restricciones de tiempos min de oper en modos arrancabl RD:', m + 1

! restricciones de tiempos minimos de operacion
! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos de operacion arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
            TminOper = TminModoURD ( u, modo )
!           si es unidad en operacion en este modo
            if ( EstadoCIURD ( u, modo ) .eq. 1 ) then
                inicio = 1
                if ( NumHCIURD ( u, modo ) .lt. TminOper ) then
!                   para las horas que le restan de operacion
                    fin = TminOper - NumHCIURD ( u, modo )
                    if ( fin .ge. NTINTR ) then
                        fin = NTINTR
                    endif
                    bandera = 0
                    do i = 1 , fin
!                       para todos los posibles modos de operacion distintos al actual
                        do modoD = 1, NumModRD ( u )
                            if ( modo .ne. modoD ) then
!                               si es una unidad no asignable en un modo distinto
                                if ( DispoURD ( u, modoD, i ) .eq. 1 .and. AsignURD ( u, modoD, i ) .eq. 0 ) then
                                    lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                                    ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                                    bandera =  bandera  + 1
                                endif
                            endif
                        enddo
                    enddo
!                   si es posible mantener el modo actual en las horas que faltan
                    if ( bandera .eq. 0 ) then
                        do i = 1 , fin
!                           la unidad permanecer en el mismo modo
                            lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                            ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                        enddo
                    endif
		            inicio = i
                endif
!               periodos siguientes de operacion
                do i = inicio , NTINTR
!                   si es una unidad disponible
                    if ( DispoURD ( u , modo, i ) .gt. 0 ) then
		                tmin = TiempoTrans ( u, 1, modo ) + TminOper
		                if ( i + tmin -1 .gt. NTINTR ) then
		   	                tmin = tmin - ( i + TiempoTrans ( u, 1, modo ) + TminOper - 1 - NTINTR )
			            endif
			            NoRest = 0
	                    do l = i , i + tmin-1
!                           si la unidad esta no disponible
                            if (  DispoURD ( u , modo, i ) .eq. 0 .or. ubMILP (IARD + INIURDI ( u, l ) + modo - 1) .eq. 0 ) then
                                NoRest = 1
                                exit
                            endif
                        enddo
                        if ( NoRest .eq. 0 ) then
	                        do l = i + TiempoTrans ( u, 1, modo ) , i + tmin - 1
!                               variable de asignacion
                                aaMILP ( k ) = 1.0
                                jcolMILP ( k ) = IARD + INIURDI ( u, l ) + modo - 1
                                k = k + 1
                            enddo
!                           variable de arranque
                            aaMILP ( k ) = TiempoTrans ( u, 1, modo ) - tmin
                            jcolMILP ( k ) = IARRD + INIURDI ( u, i ) + modo - 1
	                        k = k + 1
!                           lados derechos de las restriciones
	                        m = m + 1
                            bMILP ( m )   = 0.0
!                           sentidos de las restriciones
                            sMILP ( m ) = 'G'
!                           apuntador al siguiente renglon
                            irowMILP ( m + 1 ) = k
                            write ( leti, 200 ) i
                            write ( 779,* ) m, ',', '"Tiempos min de oper en modos arrancablle URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                        endif
                    endif
                enddo
            else
                TminOper = TminModoURD ( u, modo )
!               periodos de operacion
                do i = 1 , NTINTR
!                   si es una unidad disponible
                    if ( DispoURD ( u , modo, i ) .gt. 0 ) then
		                tmin = TiempoTrans ( u, 1, modo ) + TminOper
		                if ( i + tmin -1 .gt. NTINTR ) then
		   	                tmin = tmin - ( i + TiempoTrans ( u, 1, modo ) + TminOper - 1 - NTINTR )
			            endif
			            NoRest = 0
	                    do l = i , i + tmin-1
!                           si la unidad esta no disponible
                            if (  DispoURD ( u , modo, i ) .eq. 0 .or. ubMILP (IARD + INIURDI ( u, l ) + modo - 1) .eq. 0 ) then
                                NoRest = 1
                                exit
                            endif
                        enddo
                        if ( NoRest .eq. 0 ) then
	                        do l = i + TiempoTrans ( u, 1, modo ) , i + tmin - 1
!                               variable de asignacion
                                aaMILP ( k ) = 1.0
                                jcolMILP ( k ) = IARD + INIURDI ( u, l ) + modo - 1
                                k = k + 1
                            enddo
!                           variable de arranque
                            aaMILP ( k ) = TiempoTrans ( u, 1, modo ) - tmin
                            jcolMILP ( k ) = IARRD + INIURDI ( u, i ) + modo - 1
	                        k = k + 1
!                           lados derechos de las restriciones
	                        m = m + 1
                            bMILP ( m )   = 0.0
!                           sentidos de las restriciones
                            sMILP ( m ) = 'G'
!                           apuntador al siguiente renglon
                            irowMILP ( m + 1 ) = k
                            write ( leti, 200 ) i
                            write ( 779,* ) m, ',', '"Tiempos min de oper en modos arrancablle URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                        endif
                    endif
                enddo
            endif
        endif
    enddo
enddo


write ( 777,* ) 'Inicia restricciones de tiempos min de oper en modos Orig noarrRD:', m + 1

! restricciones de tiempos minimos de operacion
! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos de operacion, excepto el apagado (modo 1)
    do modoO = 2, NumModRD ( u )
!       para todos los modos factibles
        do modoD = 2, NumModRD ( u )
            if ( TransFacti ( u, modoO, modoD ) .gt. 0 .and. modoO .ne. modoD ) then
!               periodos de operacion
                do i = 1, NTINTR
                    beta = 0
                    intervalo = i - TminModoURD ( u, modoO )
!                   si es necesario ir al pasado
                    if ( intervalo .lt. 1 ) then
                        if ( EstadoCIURD ( u, modoO ) .eq. modoO ) then
                            beta = beta + NumHCIURD ( u, modoO )
                            t = TminModoURD ( u, modoO ) + 1 - i
                            if ( t .gt. beta .and. DispoURD ( u, modoO, i ) .eq. 1 .and. AsignURD ( u, modoO, i ) .eq. 0 ) then
                                lbMILP ( IARD + INIURDI ( u, i ) + modoO - 1 ) = 1.0
                                ubMILP ( IARD + INIURDI ( u, i ) + modoO - 1 ) = 1.0
!                                if ( DispoURD ( u, modoD, i ) .eq. 1 .and. AsignURD ( u, modoD, i ) .eq. 0 ) then
!                                    lbMILP ( IARD + INIURDI ( u, i ) + modoD - 1 ) = 1.0
!                                    ubMILP ( IARD + INIURDI ( u, i ) + modoD - 1 ) = 1.0
!                                    lbMILP ( IARD + INIURDI ( u, i ) + modoO - 1 ) = 0.0
!                                    ubMILP ( IARD + INIURDI ( u, i ) + modoO - 1 ) = 0.0
!                                endif
                            else
!                               variable de arranque
                                aaMILP ( k ) = -t
                                jcolMILP ( k ) = IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 
                                k = k + 1
!                               lados derechos de las restriciones
                                m = m + 1
                                bMILP ( m )   = -beta
!                               sentidos de las restriciones
                                sMILP ( m ) = 'G'
!                               apuntador al siguiente renglon
                                irowMILP ( m + 1 ) = k
                                write ( leti, 200 ) i
                                write ( 779,* ) m, ',', '"Tiempos min de oper en modos Orig noarrancable URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                            endif
                        endif
                    else
                        bandera = 0
                        do inicio = intervalo , i
!                           para todos los posibles modos de operacion distintos al actual
                            do modo = 1, NumModRD ( u )
                                if ( modo .ne. modoO ) then
!                                   si es una unidad no asignable en un modo distinto
                                    if ( DispoURD ( u, modo, inicio ) .eq. 1 .and. AsignURD ( u, modo, inicio ) .eq. 0 ) then
                                        lbMILP ( IARD + INIURDI ( u, inicio ) + modo - 1 ) = 1.0
                                        ubMILP ( IARD + INIURDI ( u, inicio ) + modo - 1 ) = 1.0
                                        bandera =  bandera  + 1
                                    endif
                                endif
                            enddo
                        enddo
!                       si es posible mantener el modo actual en las horas que faltan
                        if ( bandera .eq. 0 ) then
			                NoRest = 0
	                        do inicio = intervalo , i
!                               si la unidad esta no disponible
                                if (  DispoURD ( u , modoD, inicio ) .eq. 0 ) then
                                    NoRest = 1
                                    exit
                                endif
                            enddo
                            if ( NoRest .eq. 0 ) then
                                do inicio = intervalo , i
!                                   variable de asignacion
                                    aaMILP ( k ) = 1.0
                                    jcolMILP ( k ) = IARD + INIURDI ( u, inicio ) + modoO - 1
                                    k = k + 1
                                enddo
!                               variable de arranque
                                aaMILP ( k ) = -( TminModoURD ( u, modoO ) + 1 )
                                jcolMILP ( k ) = IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 
                                k = k + 1
!                               lados derechos de las restriciones
                                m = m + 1
                                bMILP ( m )   = -beta
!                               sentidos de las restriciones
                                sMILP ( m ) = 'G'
!                               apuntador al siguiente renglon
                                irowMILP ( m + 1 ) = k
                                write ( leti, 200 ) i
                                write ( 779,* ) m, ',', '"Tiempos min de oper en modos Orig noarrancable URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                            endif
                        endif
                    endif
                enddo
            endif
        enddo
    enddo
enddo


write ( 777,* ) 'Inicia restricciones de tiempos min de oper en modos Dest noarrRD:', m + 1

! restricciones de tiempos minimos de operacion
! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos de operacion, excepto el apagado (modo 1)
    do modoO = 2, NumModRD ( u )
!       para todos los modos factibles
        do modoD = 2, NumModRD ( u )
            if ( TransFacti ( u, modoO, modoD ) .gt. 0 .and. modoO .ne. modoD ) then
!               si es unidad en operacion en este modo origen
                if ( EstadoCIURD ( u, modoO ) .eq. 1 ) then
                    TminOper = TminModoURD ( u, modoO )
                    inicio = 1
                    if ( NumHCIURD ( u, modoO ) .lt. TminOper ) then
!                       para las horas que le restan de operacion
                        fin = TminOper - NumHCIURD ( u, modoO )
                        if ( fin .ge. NTINTR ) then
                            fin = NTINTR
                        endif
                        bandera = 0
                        do i = 1 , fin
!                           para todos los posibles modos de operacion distintos al actual
                            do modo = 1, NumModRD ( u )
                                if ( modo .ne. modoO ) then
!                                   si es una unidad no asignable en un modo distinto
                                    if ( DispoURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 0 ) then
                                        lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                                        ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                                        bandera =  bandera  + 1
                                    endif
                                endif
                            enddo
                        enddo
!                       si es posible mantener el modo actual en las horas que faltan
                        if ( bandera .eq. 0 ) then
                            do i = 1 , fin
!                               la unidad permanecer en el mismo modo origen
                                lbMILP ( IARD + INIURDI ( u, i ) + modoO - 1 ) = 1.0
                                ubMILP ( IARD + INIURDI ( u, i ) + modoO - 1 ) = 1.0
                            enddo
                        endif
		                inicio = i
                    endif
                    TminOper = TminModoURD ( u, modoD )
!                   periodos siguientes de operacion
                    do i = inicio , NTINTR
!                       si es una unidad disponible
                        if ( DispoURD ( u , modoD, i ) .gt. 0 ) then
		                    tmin = TiempoTrans ( u, modoO, modoD ) + TminOper
!!!		                    tmin = TminOper
		                    if ( i + tmin -1 .gt. NTINTR ) then
		   	                    tmin = tmin - ( i + TiempoTrans ( u, modoO, modoD ) + TminOper - 1 - NTINTR )
!!!		   	                    tmin = tmin - ( i + TminOper - 1 - NTINTR )
			                endif
			                NoRest = 0
	                        do l = i , i + tmin-1
!                               si la unidad esta no disponible
                                if (  DispoURD ( u , modoD, l ) .eq. 0 ) then
                                    NoRest = 1
                                    exit
                                endif
                            enddo
                            if ( NoRest .eq. 0 ) then
	                            do l = i + TiempoTrans ( u, modoO, modoD ) , i + tmin - 1
!!!	                            do l = i, i + tmin - 1
!                                   variable de asignacion
                                    aaMILP ( k ) = 1.0
                                    jcolMILP ( k ) = IARD + INIURDI ( u, l ) + modoD - 1
                                    k = k + 1
                                enddo
!                               variable de arranque
                                aaMILP ( k ) = TiempoTrans ( u, modoO, modoD ) - tmin
                                jcolMILP ( k ) = IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 
	                            k = k + 1
!                               lados derechos de las restriciones
	                            m = m + 1
                                bMILP ( m )   = 0.0
!                               sentidos de las restriciones
                                sMILP ( m ) = 'G'
!                               apuntador al siguiente renglon
                                irowMILP ( m + 1 ) = k
                                write ( leti, 200 ) i
                                write ( 779,* ) m, ',', '"Tiempos min de oper en modos Dest noarrancable URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                            endif
                        endif
                    enddo
                else
                    TminOper = TminModoURD ( u, modoD )
!                   periodos de operacion
                    do i = 1, NTINTR
!                       si es una unidad disponible
                        if ( DispoURD ( u , modoD, i ) .gt. 0 ) then
		                    tmin = TiempoTrans ( u, modoO, modoD ) + TminOper
!!!		                    tmin = TminOper
		                    if ( i + tmin -1 .gt. NTINTR ) then
		   	                    tmin = tmin - ( i + TiempoTrans ( u, modoO, modoD ) + TminOper - 1 - NTINTR )
!!!		   	                    tmin = tmin - ( i + TminOper - 1 - NTINTR )
			                endif
			                NoRest = 0
	                        do l = i , i + tmin-1
!                               si la unidad esta no disponible
                                if (  DispoURD ( u , modoD, l ) .eq. 0 ) then
                                    NoRest = 1
                                    exit
                                endif
                            enddo
                            if ( NoRest .eq. 0 ) then
	                            do l = i + TiempoTrans ( u, modoO, modoD ) , i + tmin - 1
!!!	                            do l = i, i + tmin - 1
!                                   variable de asignacion
                                    aaMILP ( k ) = 1.0
                                    jcolMILP ( k ) = IARD + INIURDI ( u, l ) + modoD - 1
                                    k = k + 1
                                enddo
!                               variable de arranque
                                aaMILP ( k ) = TiempoTrans ( u, modoO, modoD ) - tmin
!!!                                aaMILP ( k ) = - tmin
                                jcolMILP ( k ) = IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 
	                            k = k + 1
!                               lados derechos de las restriciones
	                            m = m + 1
                                bMILP ( m )   = 0.0
!                               sentidos de las restriciones
                                sMILP ( m ) = 'G'
!                               apuntador al siguiente renglon
                                irowMILP ( m + 1 ) = k
                                write ( leti, 200 ) i
                                write ( 779,* ) m, ',', '"Tiempos min de oper en modos Dest noarrancable URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                            endif
                        endif
                    enddo                  
                endif
            endif
        enddo
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de permanencia en modo origen en la transiRD:', m + 1

! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos de operacion, excepto el apagado (modo 1)
    do modoO = 2, NumModRD ( u )
!       para todos los modos factibles
        do modoD = 2, NumModRD ( u )
            if ( TransFacti ( u, modoO, modoD ) .gt. 0 .and. modoO .ne. modoD ) then
                tmin = TiempoTrans ( u, modoO, modoD )
!               periodos de operacion
                do i = 1, NTINTR
!                   si la posible transicion cabe dentro del periodo de planeacion
                    if ( i + tmin .le. NTINTR ) then
!                       si es una unidad disponible
                        if ( DispoURD ( u , modoO, i ) .gt. 0 .and. DispoURD ( u , modoD, i+tmin ) .gt. 0 ) then
                            NoRest = 0
                            do l = i , i + tmin-1
!                               si la unidad esta no disponible
                                if (  DispoURD ( u , modoO, i ) .eq. 0 ) then
                                    NoRest = 1
                                    exit
                                endif
                            enddo
                            if ( NoRest .eq. 0 ) then
                                do l = i, i + tmin - 1
!                                   variable de asignacion
                                    aaMILP ( k ) = 1.0
                                    jcolMILP ( k ) = IARD + INIURDI ( u, l ) + modoO - 1
                                    k = k + 1
                                enddo
!                               variable de arranque
                                aaMILP ( k ) = -TiempoTrans ( u, modoO, modoD )
                                jcolMILP ( k ) = IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 
                                k = k + 1
!                               lados derechos de las restriciones
                                m = m + 1
                                bMILP ( m )   = 0.0
!                               sentidos de las restriciones
                                sMILP ( m ) = 'G'
!                               apuntador al siguiente renglon
                                irowMILP ( m + 1 ) = k
                                write ( leti, 200 ) i
                                write ( 779,* ) m, ',', '"Permanencia en modo origen en la transicion URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                            endif
                        endif
                    else
!                       variable de arranque
                        ubMILP ( IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 ) = 0.0
                    endif
                enddo
            endif
        enddo
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de tiempos min de paro en unidades de rangoD:', m + 1

! restricciones de tiempos minimos de paro
! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos de operacion, excepto el apagado (modo 1)
    do modo = 2, NumModRD ( u )
!       para todos los modos arrancables
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           periodos de operacion
            do i = 1, NTINTR
                beta = 0
                intervalo = i - TminModoURD ( u, 1 )
!               si es necesario ir al pasado
                if ( intervalo .lt. 1 ) then
                    beta = beta + NumHCIURD ( u, 1 )
!                   variable de arranque
                    aaMILP ( k ) = -( TminModoURD ( u, 1 ) + 1 - i )
                    jcolMILP ( k ) = IARRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
                    m = m + 1
                    bMILP ( m )   = -beta
!                   sentidos de las restriciones
                    sMILP ( m ) = 'G'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Tiempo minimo de paro unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                else
                    bandera = 0
                    do inicio = intervalo , i
!                       para todos los posibles modos de operacion distintos al actual
                        do modoO = 1, NumModRD ( u )
                            if ( modo .ne. modoO ) then
!                               si es una unidad no asignable en un modo distinto
                                if ( DispoURD ( u, modoO, inicio ) .eq. 1 .and. AsignURD ( u, modoO, inicio ) .eq. 0 ) then
                                    lbMILP ( IARD + INIURDI ( u, inicio ) + modo - 1 ) = 0.0
                                    ubMILP ( IARD + INIURDI ( u, inicio ) + modo - 1 ) = 0.0
                                    bandera =  bandera  + 1
                                endif
                            endif
                        enddo
                    enddo
!                   si es posible mantener el modo actual en las horas que faltan
                    if ( bandera .eq. 0 ) then
                        NoRest = 0
                        do inicio = intervalo , i
!                           si la unidad esta no disponible
                            if (  DispoURD ( u , modo, inicio ) .eq. 0 ) then
                                NoRest = 1
                                exit
                            endif
                        enddo
                        if ( NoRest .eq. 0 ) then
                            do inicio = intervalo , i
!                               variable de asignacion
                                aaMILP ( k ) = 1.0
                                jcolMILP ( k ) = IARD + INIURDI ( u, inicio )
                                k = k + 1
                            enddo
!                           variable de arranque
                            aaMILP ( k ) = -( TminModoURD ( u, 1 ) + 1 )
                            jcolMILP ( k ) = IARRD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
!                           lados derechos de las restriciones
                            m = m + 1
                            bMILP ( m )   = -beta
!                           sentidos de las restriciones
                            sMILP ( m ) = 'G'
!                           apuntador al siguiente renglon
                            irowMILP ( m + 1 ) = k
                            write ( leti, 200 ) i
                            write ( 779,* ) m, ',', '"Tiempo minimo de paro unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                        endif
                    endif
                endif
            enddo
        endif
    enddo
enddo

200 format (i3)

return
end

    
Subroutine CondArranRD ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de condiciones durante el arranque          *
! en unidades de rango discontinuo.                                   *
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

integer k, i, l, m, u, fin, modo, ii, ini, entra

real*8  beta
character*3 leti

write ( 777,* ) 'Inicia restricciones de condiciones durante el arranque en uni RD:', m + 1

! para cada unidad
do u = 1, NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           si la unidad tiene tiempo de arranque en ese modo
            if ( TiempoTrans ( u, 1, modo ) .gt. 0 ) then
!               para cada intervalo de planeacion
                do i = 1 , NTINTR
!                   si la unidad es asignable
                    if ( AsignURD ( u, modo, i ) .eq. 1.0 ) then
!                       restriccion de la asignabilidad durante el tiempo de arranque
100                     fin = i + TiempoTrans ( u, 1, modo ) - 1
                        if (  fin .gt. NTINTR ) then
                            fin = NTINTR
                        end if
                        do l = i , fin
!                           variable de asignacion
                            aaMILP ( k ) = 1.0
                            jcolMILP ( k ) = IARD + INIURDI ( u, l ) + modo - 1
                            k = k + 1
                        enddo
!                       variable de arranque
                        aaMILP ( k ) = TiempoTrans ( u, 1, modo )
                        jcolMILP ( k ) = IARRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       lados derechos de las restriciones
	                    m = m + 1
                        bMILP ( m )   = TiempoTrans ( u, 1, modo )
!                       sentidos de las restriciones
                        sMILP ( m ) = 'L'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m, ',', '"Asignabilidad durante el tiempo de arranque unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                        
!                       restriccion de la asignabilidad de sincronizacion durante el tiempo de arranque
                        fin = i + TiempoTrans ( u, 1, modo ) - 1
                        if (  fin .gt. NTINTR-1 ) then
                            fin = NTINTR-1
                        end if
                        do l = i , fin
!                           variable de asignacion durante el arranque
                            aaMILP ( k ) = 1.0
                            jcolMILP ( k ) = IADARD + INIURDI ( u, l ) + modo - 1
                            k = k + 1
                        enddo
!                       variable de arranque
                        aaMILP ( k ) = -TiempoTrans ( u, 1, modo )
                        jcolMILP ( k ) = IARRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       lados derechos de las restriciones
                        m = m + 1
                        bMILP ( m )   = 0.0
!                       sentidos de las restriciones
                        sMILP ( m ) = 'G'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m, ',', '"Asignabilidad durante el tiempo de arranque unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                    else
                        ini = i - TiempoTrans ( u, 1, modo )
                        if ( ini .ge. 1 ) then
                            entra = 0
                            do ii = ini, i - 1
                                if ( DispoURD ( u, modo, ii ) .eq. 0  .or. AsignURD ( u, modo, ii ) .eq. 0 ) then
                                    entra = entra + 1
                                endif
                            enddo
!                            if ( entra .eq. 0 ) goto 100
                        endif
                    endif
                enddo
            else
!               para cada intervalo de planeacion
                do i = 1 , NTINTR
!                   no puede entrar en sincronizacion la unidad
                    ubMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                enddo
            endif
        endif
    enddo
enddo

! para cada unidad
do u = 1, NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           para cada intervalo de planeacion
            do i = 1 , NTINTR
!               restriccion de limite inferior en generacion de sincronizacion
!               coeficiente de generacion durante sincronizacion 
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de asignacion durante sincronizacion 
                aaMILP ( k ) = - PotSincURD ( u, modo )
                jcolMILP ( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Limite inferior en generacion de sincronizacion unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'

!               restriccion de limite superior en generacion de sincronizacion
!               coeficiente de generacion durante sincronizacion 
                aaMILP ( k ) = 1.0
                jcolMILP ( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de asignacion durante sincronizacion 
                aaMILP ( k ) = -PotMinGRD ( u, modo, i )
                jcolMILP ( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Limite superior en generacion de sincronizacion unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            enddo
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de posibilidad a sincronizarse una unidad RD:', m + 1

! para cada unidad
do u = 1, NumUniRD
!   para todos los intervalos
    do i = 1, NTINTR
        beta = 0.0
!       Para el primer intervalo
        if ( i .eq. 1 ) then
!           si la unidad esta en paro
            if ( EstadoCIURD ( u, 1 ) .eq. 1 ) then
                beta = 1.0
            endif
!           para los modos arrancables
            do modo = 2, NumModRD ( u )
                if ( TransFacti ( u, 1, modo ) .gt. 0 .and. lbMILP ( IADARD + INIURDI ( u, 1 ) + modo - 1 ) .ne. 1.0 ) then
!                   coeficiente de asignacion durante sincronizacion 
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
                else
!                   no puede entrar en sincronizacion la unidad
                    ubMILP ( IADARD + INIURDI ( u, 1 ) + modo - 1 ) = 0.0
                endif
            enddo
        else
!           variable de asignacion en el modo apagado e intervalo anterior
            aaMILP ( k ) = -1.0
            jcolMILP ( k ) = IARD + INIURDI ( u, i-1 )
            k = k + 1
!           para los modos arrancables
            do modo = 2, NumModRD ( u )
                if ( TransFacti ( u, 1, modo ) .gt. 0 .and. lbMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) .ne. 1.0 ) then
!                   coeficiente de asignacion durante sincronizacion 
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
                else
!                   no puede entrar en sincronizacion la unidad
                    ubMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                endif
            enddo
        endif
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m )   = beta
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Posibilidad a sincronizarse  unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo


write ( 777,* ) 'Inicia restricciones de rampa durante la sincronizacion unidad RD:', m + 1

! para cada unidad
do u = 1, NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           si la unidad tiene tiempo de arranque en ese modo
            if ( TiempoTrans ( u, 1, modo ) .gt. 0 ) then
!               para todos los intervalos
                do i = 1, NTINTR
!                   restricciones de rampa de subida durante el proceso de arranque
!                   si es el primer intervalo
                    if ( i .eq. 1 ) then
!                       variable de generacion durante tiempo de arranque del intervalo actual
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       variable de asignacion durante tiempo de arranque del intervalo actual
                        aaMILP ( k ) = - PotSincURD ( u, modo )
    !                   si la unidad ya esta en proceso de arranque
                        if ( lbMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) .eq. 1.0 ) then
                            aaMILP ( k ) = - PotMinRRD ( u, modo, 1 )
                        endif
                        jcolMILP ( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    else
!                       siguientes intervalos
!                       variable de generacion durante tiempo de arranque del intervalo actual
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       variable de generacion durante tiempo de arranque del intervalo anterior
                        aaMILP ( k ) = -1.0
                        jcolMILP ( k ) = IGDARD + INIURDI ( u, i-1 ) + modo - 1
                        k = k + 1
!                       variable de asignacion durante tiempo de arranque del intervalo actual
                        aaMILP ( k ) = - PotSincURD ( u, modo )
!                       si la unidad ya esta en proceso de arranque
                        if ( lbMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) .eq. 1.0 ) then
                            aaMILP ( k ) = - PotMinRRD ( u, modo, i )
                        endif
                        jcolMILP ( k ) = IADARD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       variable de asignacion durante tiempo de arranque del intervalo anterior
                        if ( abs(PotSincURD ( u, modo ) - RampArraURD ( u, modo )) .gt. 1e-3 .and. lbMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) .ne. 1.0 ) then
                            aaMILP ( k ) = PotSincURD ( u, modo ) - RampArraURD ( u, modo )
                            jcolMILP ( k ) = IADARD + INIURDI ( u, i-1 ) + modo - 1
                            k = k + 1
                        endif
                    endif
!                   lados derechos de las restriciones
                    m = m + 1
                    bMILP ( m )   = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Rampa durante la sincronizacion unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                enddo
            endif
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de aumento de rampa durante arranque de U RD:', m + 1

! para cada unidad
do u = 1, NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           si la unidad tiene tiempo de arranque en ese modo
            if ( TiempoTrans ( u, 1, modo ) .gt. 0 ) then
!               para cada intervalo de planeacion
                do i = 2 , NTINTR
!                   variable de generacion durante tiempo de arranque del intervalo actual
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   variable de generacion durante tiempo de arranque del intervalo anterior
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k ) = IGDARD + INIURDI ( u, i-1 ) + modo - 1
                    k = k + 1
!                   coeficiente de asignacion durante operacion 
                    aaMILP ( k ) = PotMinGRD ( u, modo, i )
                    jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   variable de asignacion durante tiempo de arranque del intervalo anterior
                    aaMILP ( k ) = -RampArraURD ( u, modo )
                    jcolMILP ( k ) = IADARD + INIURDI ( u, i-1 ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
                    m = m + 1
                    bMILP ( m )   = -1.0e-5
!                   sentidos de las restriciones
                    sMILP ( m ) = 'G'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Aumento de rampa durante el arranque unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                enddo
            endif
        endif
    enddo    
enddo

198 continue

200 format (i3)
    
return
end
 
    
Subroutine RampasOperRD ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de rampas de operacion en unidades de rango *
! discontinuo.                                                        *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, kv, i, m, u, s, modo, modoD, intervalo
real*8  beta
character*3 leti

write ( 777,* ) 'Inicia restricciones de rampas de subida para intervalo inicialRD:', m + 1

kv = IGABRD - 1
! restricciones de limites de rampas de subida para intervalo inicial
do u = 1 , NumUniRD
!   para los modos excepto el paro
    do modo = 2, NumModRD ( u )
!       si la unidad esta disponible y es cordinable
        if ( RampaSubURD ( u, modo ) .gt. 0 .and. DispoURD ( u, modo, 1 ) .ne. 0 .and. CoordURD ( u, modo, 1 ) .ne. 0 ) then
	        beta = 0.0
!           si la unidad esta encendida en condiciones iniciales en ese modo
            if ( EstadoCIURD ( u, modo ) .gt. 0 ) then
!                beta = RampaSubURD ( u, modo ) + GenCIURD ( u, modo ) - PotMinGRD ( u, modo, 1 )
                beta = GenCIURD ( u, modo ) - PotMinGRD ( u, modo, 1 )
                if ( abs(beta) .lt. 1e-3 ) then
                    beta = 0.0
                endif
            endif
!           para todos los segmentos de curva de ofertas de venta
            do s = 1, NumBloVRD( u, modo, 1 )
!               coeficiente de la variable de generacion aceptada en el bloque
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) =  kv + INBURD (u, modo, 1) + s
                k = k + 1
            enddo
            m = m + 1
!           lados derechos de las restriciones
!            bMILP ( m )   = beta 
            bMILP ( m )   = beta + RampaSubURD ( u, modo )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( 779,* ) m, ',', '"Rampa de subida para intervalo inicial unidad URD: '//trim(nombunird(u))//'"'
        endif
    enddo
enddo

write ( 777,* ) 'Inicia siguientes restricciones de limites de rampas de subida URD', m + 1

! siguientes restricciones de limites de rampas de subida
do u = 1 , NumUniRD
!   para los modos excepto el paro
    do modo = 2, NumModRD ( u )
        if ( RampaSubURD ( u, modo ) .gt. 0.0 ) then
            do i = 2 , NTINTR
!               si la unidad esta disponible y es cordinable
                if ( DispoURD ( u, modo, i ) .ne. 0 .and. CoordURD ( u, modo, i ) .ne. 0 ) then
                    if ( abs(PotMinGRD ( u, modo, i ) - PotMinGRD ( u, modo, i-1 ) - RampaSubURD ( u, modo )) .gt. 1e-3 ) then
!                        aaMILP ( k ) = PotMinGRD ( u, modo, i ) - PotMinGRD ( u, modo, i-1 ) - RampaSubURD ( u, modo )
                        aaMILP ( k ) = PotMinGRD ( u, modo, i ) - PotMinGRD ( u, modo, i-1 )
                        jcolMILP ( k ) = IARD  + INIURDI ( u, i-1 ) + modo - 1
	                    k = k + 1
                    endif
!                   para todos los segmentos de curva de ofertas de venta en el intervalo
                    do s = 1, NumBloVRD( u, modo, i )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = 1.0
!                       columna asociada
                        jcolMILP( k ) = kv + INBURD (u, modo, i) + s
                        k = k + 1
                    enddo
!                   para todos los segmentos de curva de ofertas de venta en el intervalo anterior
                    do s = 1, NumBloVRD( u, modo, i-1 )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = -1.0
!                       columna asociada
                        jcolMILP( k ) = kv + INBURD (u, modo, i-1) + s
                        k = k + 1
                    enddo
!                   lados derechos de las restriciones
	                m = m + 1
!                    bMILP ( m )   = 0.0
                    bMILP ( m )   =  RampaSubURD ( u, modo )
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Rampa de subida unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            enddo
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de rampas de bajada intra-modo int inicialRD:', m + 1

! restricciones de limites de rampas de bajada para intervalo inicial
do u = 1 , NumUniRD
!   para los modos excepto el paro
    do modo = 2, NumModRD ( u )
!       si la unidad esta disponible y es cordinable
        if ( RampaBajURD ( u, modo ) .gt. 0 .and. DispoURD ( u, modo, 1 ) .ne. 0 .and. CoordURD ( u, modo, 1 ) .ne. 0 ) then
            if ( AsignURD ( u, modo, 1 ) .gt. 0 ) then
	            beta = 0.0
!               si la unidad esta encendida en condiciones iniciales en ese modo
                if ( EstadoCIURD ( u, modo ) .gt. 0 ) then
                    beta = - GenCIURD ( u, modo )
                endif
!               coeficiente de asignacion
                if ( abs(- RampaBajURD ( u, modo ) - PotMinGRD ( u, modo, 1 )) .gt. 1e-3 ) then
!                    aaMILP ( k ) = - RampaBajURD ( u, modo ) - PotMinGRD ( u, modo, 1 ) 
                    aaMILP ( k ) = - PotMinGRD ( u, modo, 1 ) 
                    jcolMILP ( k ) = IARD  + INIURDI ( u, 1 ) + modo - 1
                    k = k + 1
                endif
!               para todos los segmentos de curva de ofertas de venta
                do s = 1, NumBloVRD( u, modo, 1 )
!                   coeficiente de la variable de generacion aceptada en el bloque
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = kv + INBURD (u, modo, 1) + s
                    k = k + 1
                enddo
!               para todas las posibles transiciones factibles desde el modo
                do modoD = 1, NumModRD(u)
!                   si la transicion es factible y hacia un modo distinto
                    if ( TransFacti ( u, modo, modoD ) .gt. 0 .and. modo .ne. modoD ) then
                        intervalo = 1 - TiempoTrans ( u, modo, modoD )
                        if ( intervalo .ge. 1 ) then
!                           variable de arranque
                            aaMILP ( k ) = -PotMaxGRD ( u, modo, 1 )
                            jcolMILP ( k ) = IOMARD + INALPHAT ( u, intervalo ) + (modo-1)*NumModRD ( u ) + modoD - 1 
                            k = k + 1
                        endif
                    endif
                enddo
                m = m + 1
!               lados derechos de las restriciones
!                bMILP ( m )   = beta
                bMILP ( m )   = beta + RampaBajURD ( u, modo )
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( 779,* ) m, ',', '"rampas de bajada intra-modo intervalo inicial unidad URD: '//trim(nombunird(u))//'"'
            endif
        endif
    enddo
enddo

write ( 777,* ) 'Inicia siguientes restricciones de rampa bajada intramodo siginRD:', m + 1

! siguientes restricciones de limites de rampas de subida
do u = 1 , NumUniRD
!   para los modos excepto el paro
    do modo = 2, NumModRD ( u )
        if ( RampaBajURD ( u, modo ) .gt. 0.0 ) then
            do i = 2 , NTINTR
!               si la unidad esta disponible y es cordinable
                if ( DispoURD ( u, modo, i ) .ne. 0 .and. AsignURD ( u, modo, 1 ) .gt. 0  .and. CoordURD ( u, modo, i ) .ne. 0 ) then
!                   coeficiente de asignacion en el intervalo anterior
                    aaMILP ( k ) = PotMinGRD ( u, modo, i-1 )
                    jcolMILP ( k ) = IARD + INIURDI ( u, i-1 ) + modo - 1
	                k = k + 1
!                   coeficiente de asignacion en el intervalo actual
                    if ( abs(- RampaBajURD ( u, modo ) - PotMinGRD ( u, modo, i )) .gt. 1e-3 ) then
!                        aaMILP ( k ) = - RampaBajURD ( u, modo ) - PotMinGRD ( u, modo, i )
                        aaMILP ( k ) = - PotMinGRD ( u, modo, i )
                        jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modo - 1
	                    k = k + 1
                    endif
!                   para todos los segmentos de curva de ofertas de venta en el intervalo
                    do s = 1, NumBloVRD ( u, modo, i )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = -1.0
!                       columna asociada
                        jcolMILP( k ) = kv + INBURD (u, modo, i) + s
                        k = k + 1
                    enddo
!                   para todos los segmentos de curva de ofertas de venta en el intervalo anterior
                    do s = 1, NumBloVRD ( u, modo, i-1 )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = 1.0
!                       columna asociada
                        jcolMILP( k ) = kv + INBURD (u, modo, i-1) + s
                        k = k + 1
                    enddo
!                   para todas las posibles transiciones factibles desde el modo
                    do modoD = 1, NumModRD(u)
!                       si la transicion es factible y hacia un modo distinto
                        if ( TransFacti ( u, modo, modoD ) .gt. 0 .and. modo .ne. modoD ) then
                            intervalo = i - TiempoTrans ( u, modo, modoD )
                            if ( intervalo .ge. 1 ) then
!                               variable de arranque
                                aaMILP ( k ) = -PotMaxGRD ( u, modo, i-1 )
                                jcolMILP ( k ) = IOMARD + INALPHAT ( u, intervalo ) + (modo-1)*NumModRD ( u ) + modoD - 1 
                                k = k + 1
                            endif
                        endif
                    enddo
!                   lados derechos de las restriciones
	                m = m + 1
!                    bMILP ( m )   = 0.0
                    bMILP ( m )   = RampaBajURD ( u, modo ) 
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Rampa de bajada intrmod unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            enddo
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de rampas de bajada al apagado int inicialRD:', m + 1

! restricciones de limites de rampas de bajada para intervalo inicial
do u = 1 , NumUniRD
!   para los modos excepto el paro
    do modo = 2, NumModRD ( u )
!       si la unidad esta disponible y es cordinable
        if ( RampaBajURD ( u, modo ) .gt. 0 .and. DispoURD ( u, modo, 1 ) .ne. 0 .and. CoordURD ( u, modo, 1 ) .ne. 0 ) then
            if ( AsignURD ( u, modo, 1 ) .gt. 0 ) then
	            beta = 0.0
!               si la unidad esta encendida en condiciones iniciales en ese modo
                if ( EstadoCIURD ( u, modo ) .gt. 0 ) then
                    beta = - GenCIURD ( u, modo )
                endif
!               para todas las transiciones desde el modo
                do modoD = 2, NumModRD(u)
!                   variable de asignacion
                    aaMILP ( k ) = -PotMaxGRD ( u, modo, 1 )
                    jcolMILP ( k ) = IARD  + INIURDI ( u, 1 ) + modoD - 1
                    k = k + 1
                enddo
!               variable de paro
                aaMILP ( k ) = -PotMinGRD ( u, modo, 1 ) -RampaBajURD ( u, modo )
                jcolMILP ( k ) = IPRD + u - 1
                k = k + 1
                m = m + 1
!               lados derechos de las restriciones
                bMILP ( m )   = beta 
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( 779,* ) m, ',', '"rampas de bajada al apagado intervalo inicial unidad URD: '//trim(nombunird(u))//'"'
            endif
        endif
    enddo
enddo

write ( 777,* ) 'Inicia siguientes restricciones de rampa bajada al apagad siginRD:', m + 1

! siguientes restricciones de limites de rampas de subida
do u = 1 , NumUniRD
!   para los modos excepto el paro
    do modo = 2, NumModRD ( u )
        if ( RampaBajURD ( u, modo ) .gt. 0.0 ) then
            do i = 2 , NTINTR
!               si la unidad esta disponible y es cordinable
                if ( DispoURD ( u, modo, i ) .ne. 0 .and. CoordURD ( u, modo, i ) .ne. 0 .and. AsignURD ( u, modo, i ) .gt. 0 ) then
!                   coeficiente de asignacion en el intervalo anterior
                    aaMILP ( k ) = PotMinGRD ( u, modo, i-1 )
                    jcolMILP ( k ) = IARD + INIURDI ( u, i-1 ) + modo - 1
	                k = k + 1
!                   para todos los segmentos de curva de ofertas de venta en el intervalo anterior
                    do s = 1, NumBloVRD ( u, modo, i-1 )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = 1.0
!                       columna asociada
                        jcolMILP( k ) = kv + INBURD (u, modo, i-1) + s
                        k = k + 1
                    enddo
!                   para todas las transiciones desde el modo
                    do modoD = 2, NumModRD(u)
!                       variable de asignacion
                        aaMILP ( k ) = -PotMaxGRD ( u, modo, i-1 )
                        jcolMILP ( k ) = IARD  + INIURDI ( u, i ) + modoD - 1
                        k = k + 1
                    enddo
!                   variable de paro
                    aaMILP ( k ) = -PotMinGRD ( u, modo, i-1 ) -RampaBajURD ( u, modo )
                    jcolMILP ( k ) = IPRD + u + (i-1)*NumUniRD - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m )   = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Rampa de bajada al apagado unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            enddo
        endif
    enddo
enddo

200 format (i3)
    
return
end


Subroutine CostVarArrRD ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de costos variables de arranque en unidades *
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

Integer k, i, s, m, u, tmin, tmax, tau, ir, modo
character*3 leti

! restricciones de tipos de arranque 
write ( 777,* ) 'Inicia restricciones de costos variables de arranque unidades RD :', m + 1

do u = 1 , NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           si existen segmentos de arranque
            if ( NmBloArrURD ( u, modo ) .gt. 0 ) then
                do i = 1 , NTINTR
!                   para todos los segmentos de arranque, excepto el final
                    do s = 1, NmBloArrURD ( u, modo ) - 1
!                       coeficiente de la variable de tipo de arranque
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IBOARD + INBAURD ( u, modo, i ) + s - 1
                        k = k + 1
!                       subconjunto I'
                        tmin = i - TieminicioarrRDS ( u, modo, s+1 ) + 1
                        tmax = i - TieminicioarrRDS ( u, modo, s )
                        if ( tmax .ge. i ) then
                           tmax = i - 1
                        end if
	                    m = m + 1
!                       lados derechos de las restriciones
                        bMILP ( m ) = 0.0
		                if ( ( tmin .gt. 0 .and. tmax .gt. 0 ) .or. ( tmin .le.0 .and. tmax .gt.0 ) ) then
		                    if ( tmin .le.0 .and. tmax .gt.0 ) then
		                        tmin = 1
		                    endif
		                    do tau = tmin, tmax
!                               coeficiente de la variable de paro
!                               si la unidad esta disponible
                                ir = 1
                                if ( tau - 1 .ge. 1 ) then
                                    if ( DispoURD ( u, modo, tau - 1 ) .eq. 0  ) then
                                       ir = 0
                                    endif
                                else
                                    if ( EstadoCIURD ( u, modo ) .eq. 0  ) then
                                       ir = 0
                                    endif
                                endif
!                                if ( ir .eq. 1 .and. AsignURD ( u, modo, tau ) .gt. 0 ) then
                                    aaMILP ( k ) = - 1.0
                                    jcolMILP ( k  ) = IPRD + u + (tau-1)*NumUniRD - 1
                                    k = k + 1
!                                end if
		                    enddo
                        endif
                        tmin = i - TieminicioarrRDS ( u, modo, s+1 ) + 1
                        tmax = i - TieminicioarrRDS ( u, modo, s )
                        if ( tmin .le.0 .or. tmax .le.0 ) then
!                           si la unidad esta en paro en condiciones iniciales
                            if ( EstadoCIURD ( u, 1 ) .eq. 1 ) then
                                tmax = TieminicioarrRDS ( u, modo, s+1 ) - 1
                                tmin = TieminicioarrRDS ( u, modo, s )
                                if ( ( NumHCIURD ( u, 1 ) + i - 1 ) .ge. tmin .and. ( NumHCIURD ( u, 1 ) + i - 1 ) .le. tmax ) then
!                                   lado derecho de la restricion
                                    bMILP ( m ) = 1.0
                                endif
                            endif
                        endif
             
!                       sentidos de la restricion
                        sMILP ( m ) = 'L'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m, ',', '"Costos variables de arranque unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                    enddo
          
!                   coeficiente de la variable de arranque del ultimo segmento
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k  ) = IBOARD + INBAURD ( u, modo, i ) + s - 1
                    k = k + 1
                    tmax = i - TieminicioarrRDS ( u, modo, s )
!                   lado derecho de la restricion
	                m = m + 1
                    bMILP ( m ) = 0.0
	                if ( tmax .gt. 0 ) then
	                    do tau = 1, tmax
!                           coeficiente de la variable de paro
!                           si la unidad esta disponible
                            ir = 1
                            if ( tau - 1 .ge. 1 ) then
                                if ( DispoURD ( u, modo, tau - 1 ) .eq. 0  ) then
                                    ir = 0
                                endif
                            else
                                if ( EstadoCIURD ( u, modo ) .eq. 0  ) then
                                    ir = 0
                                endif
                            endif
                            if ( ir .eq. 1 .and. AsignURD ( u, modo, tau ) .gt. 0 ) then
                                aaMILP ( k ) = - 1.0
                                jcolMILP ( k  ) = IPRD + u + (tau-1)*NumUniRD - 1
                                k = k + 1
                            end if
                        enddo
                    endif
!                   si la unidad esta en paro en condiciones iniciales
                    if ( EstadoCIURD ( u, 1 ) .eq. 1 ) then
                        tmax = TieminicioarrRDS ( u, modo, s )
                        if ( ( NumHCIURD ( u, 1 ) + i - 1 ) .ge. tmax ) then
!                           lado derecho de la restricion
                            bMILP ( m ) = 1.0
                        endif
                    endif

!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Costos variables de arranque unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                enddo
            endif
        endif
    enddo
enddo

! restricciones que asocian la variable de arranque con los segmentos de arranque
write ( 777,* ) 'Inicia restricciones de arranque con segmentos de arranque       :', m + 1

do u = 1 , NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           si existen segmentos de arranque
            if ( NmBloArrURD ( u, modo ) .gt. 0 ) then
                do i = 1 , NTINTR
!                   para todos los segmentos de arranque
                    do s = 1,  NmBloArrURD ( u, modo )
!                       coeficientes de las variables de segmentos de arranque
                        aaMILP ( k ) = -1.0
                        jcolMILP ( k  ) = IBOARD + INBAURD ( u, modo, i ) + s - 1
                        k = k + 1		       
                    enddo
!                   coeficiente de la variable de arranque
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k  ) = IARRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Arranques con segmentos de arranque unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                enddo
            endif
        endif
    enddo
enddo

200 format (i3)
    
return
end
     

Subroutine EstadoUniRD
! ---------------------------------------------------------------------
! Restricciones sobre estados de las unidades de rango discontinuo    *
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

INTEGER   i, u, modo, j, nint

! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion
    do modo = 2, NumModRD ( u )
        do i = 1 , NTINTR
!           si es unidad no disponible en este periodo y modo
            if ( DispoURD ( u , modo, i ) .eq. 0 ) then
!               la unidad no se puede operar
                lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
!           si es unidad disponible y no asignables en este periodo y modo
	        elseif ( DispoURD ( u , modo, i ) .eq. 1 .and.  AsignURD ( u , modo, i ) .eq. 0 ) then
!               la unidad debe operar
                lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
            endif
!           Si es un EXPOST
            if ( TipoEjecu .eq. 2 ) then
                lbMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                ubMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) = 0.0
            endif
        enddo
!       Si la unidad esta arrancando y no es un EXPOST
        if ( EstadoCIURD ( u, modo ) .eq. 11 .and. TipoEjecu .ne. 2 ) then
!           tiempo restante que falta para sincronizarse
            do i = 1 , TiempoTrans ( u, 1, modo ) - NumHCIURD ( u, modo )
                lbMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                ubMILP ( IADARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
            enddo
            nint = min( TminModoURD ( u, modo ), NTINTR - i )
!           tiempo que debe permancer encendida para cumplir su tiempo minimo de operacion dentro del horizonte
            do j = 1 , nint
                lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = 1.0
                i = i + 1
            enddo
        endif            
    enddo
    do i = 1 , NTINTR
!       si es unidad no disponible en este periodo y modo
        if ( DispoURD ( u , 1, i ) .eq. 0 ) then
!           la unidad no se puede operar
            lbMILP ( IARD + INIURDI ( u, i )  ) = 0.0
            ubMILP ( IARD + INIURDI ( u, i )  ) = 0.0
!           la unidad no se puede sincronizar
            lbMILP ( IADARD + INIURDI ( u, i )  ) = 0.0
            ubMILP ( IADARD + INIURDI ( u, i )  ) = 0.0
!       si es unidad disponible y no asignables en este periodo y modo
        elseif ( DispoURD ( u , 1, i ) .eq. 1 .and.  AsignURD ( u , 1, i ) .eq. 0 ) then
!           la unidad debe parar (estar en el modo 1)
            lbMILP ( IARD + INIURDI ( u, i )  ) = 1.0
            ubMILP ( IARD + INIURDI ( u, i )  ) = 1.0
        endif
    enddo
enddo

return
end

    
Subroutine LimResURD ( k, m )
! ---------------------------------------------------------------------
! Restricciones reserva rodante de diez minutos de unidades de rango  *
! discontinuo, en el problema de asignacion (MILP).                   *
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

INTEGER   i, k, m, u, modo
real*8    maximo
character*3 leti

! restricciones de limites de reserva rodante de 10 minutos
write ( 777,* ) 'Inicia restricciones de reserva rodante de 10 minutos U de R Disc:', m + 1
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
!       para todos los intervalos
        do i = 1 , NTINTR
!           si es unidad disponible y cordinable en este periodo
!           if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!           si es unidad disponible en este periodo
            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!               coeficiente de la variable de reserva
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de la variable de asignacion
                aaMILP ( k  ) = - CalOferResR10RD ( u, modo, i )
                jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Reserva rodante de 10 minutos unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            endif
        enddo
    enddo
enddo

! restricciones de limites de reserva rodante suplementaria
write ( 777,* ) 'Inicia restricciones de reserva suplementaria para unidades de RD:', m + 1
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
!       para todos los intervalos
        do i = 1 , NTINTR
!           si es unidad disponible y cordinable en este periodo
!            if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!           si es unidad disponible en este periodo
            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!               coeficiente de la variable de reserva
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRRSRD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de la variable de asignacion
                aaMILP ( k  ) = - CalOferResRxRD ( u, modo, i )
                jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Reserva suplementaria unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            endif
        enddo
    enddo
enddo

! restricciones de limites de reserva de regulacion secundaria
write ( 777,* ) 'Inicia restricciones de limite maximo reserva de regulacion para URD:', m + 1
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
!       para todos los intervalos
        do i = 1 , NTINTR
!           si la unidad oferta reserva de regulacion secundaria en el modo
            if ( CalOferResRegRD ( u, modo, i ) .gt. 0.0 ) then
!               si es unidad disponible y cordinable en este periodo
!                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
                if ( DispoURD ( u, modo, i ) .eq. 1 ) then
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva
                    aaMILP ( k  ) = - CalOferResRegRD ( u, modo, i )
                    jcolMILP( k ) = IVREGRD + INREGRD ( u, modo, i ) - 1
                    k = k + 1
                    ctypeMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 'B'
!                   cota inferior de la variable
                    lbMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 0.0
!                   cota superior de la variable
                    ubMILP ( IVREGRD + INREGRD ( u, modo, i ) - 1 ) = 1.0
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Limite maximo reserva regulacion de unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            else
!               no existe reserva de regulacion para la unidad
                ubMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 ) = 0.0
            endif
        enddo
    enddo
enddo


write ( 777,* ) 'Inicia restricciones de limite minimo reserva de regulacion para URD:', m + 1
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
!       para todos los intervalos
        do i = 1 , NTINTR
!           si la unidad oferta reserva de regulacion secundaria en el modo
            if ( CalOferResRegRD ( u, modo, i ) .gt. 0.0 ) then
!               si es unidad disponible y cordinable en este periodo
!                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
                if ( DispoURD ( u, modo, i ) .eq. 1 ) then
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva
                    aaMILP ( k  ) = - MrreURD ( u, modo, i )
                    jcolMILP( k ) = IVREGRD + INREGRD ( u, modo, i ) - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'G'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Limite minimo reserva regulacion de unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            endif
        enddo
    enddo
enddo

! restricciones de limites de reserva de regulacion y rodante de diez minutos por rampa de 10
write ( 777,* ) 'Inicia restricciones de reserva de regula y rodan 10 por ramp URD:', m + 1
! para las unidades de rango discontinuo
do u = 1 , NumUniRD*SiRegEnRod*0
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
!       para todos los intervalos
        do i = 1 , NTINTR
!           si la unidad oferta reserva de regulacion secundaria y es unidad disponible en este periodo
            if ( CalOferResRegRD ( u, modo, i ) .gt. 0.0 .and. DispoURD ( u , modo, i ) .eq. 1 ) then
!               coeficiente de la variable de reserva de regulacion
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de la variable de reserva rodante de diez
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de la variable de asignacion
!                aaMILP ( k  ) = - RamEmer10RD ( u, modo )
!                jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modo - 1
!                k = k + 1
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = RamEmer10RD ( u, modo )
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Reserva regulacion y rodante de 10 limita por rampa URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            endif
        enddo
    enddo
enddo

! restricciones de limites de reserva rodante de diez minutos y suplementaria
write ( 777,* ) 'Inicia restricciones de reserva rodante de diez y suplementar URD:', m + 1
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
!       para todos los intervalos
        do i = 1 , NTINTR
!           si es unidad disponible y cordinable en este periodo
!            if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!           si es unidad disponible en este periodo
            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
                if ( SiRegEnRod .eq. 5 ) then
!                   coeficiente de la variable de reserva de regulacion
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
                endif
!               coeficiente de la variable de reserva rodante de diez
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de la variable de reserva suplementaria
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = IRRSRD + INIURDI ( u, i ) + modo - 1
                k = k + 1
!               coeficiente de la variable de asignacion
!                aaMILP ( k  ) = - RamEmerxRD ( u, modo )
!                jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modo - 1
!                k = k + 1
!               lados derechos de las restriciones
	            m = m + 1
                bMILP ( m ) = RamEmerxRD ( u, modo )
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Reserva rodante de 10 minutos y suplemetaria unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            endif
        enddo
    enddo
enddo


! restricciones de limites de reserva no rodante de 10 minutos
write ( 777,* ) 'Inicia restricciones de reserva no rodante de 10 minutos para URD:', m + 1

! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           para todos los intervalos
            do i = 1 , NTINTR
!               si es unidad disponible, asignable  y cordinable en este periodo
                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
                
!                   limite inferior
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva no rodante
                    aaMILP ( k  ) = -CalOferResNR10RD ( u, modo, i )
                    jcolMILP ( k ) = IARNRRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'G'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Reserva no rodante de 10 minutos unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
!                   limite superior
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva no rodante
                    aaMILP ( k  ) = -OferResNR10RD ( u, modo, i )
                    jcolMILP ( k ) = IARNRRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Reserva no rodante de 10 minutos unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            enddo
        endif
    enddo

!   restriccion de relacion entre paro y asignacion de reservas no rodantes
!   para todos los intervalos
    do i = 1 , NTINTR
!       si es unidad disponible y asignable en el modo apagado
        if ( DispoURD ( u, 1, i ) .eq. 1 .and. AsignURD ( u, 1, i ) .eq. 1 ) then
!           coeficiente de la variable de asignacion en el modo apagado
            aaMILP ( k  ) = -1.0
            jcolMILP ( k ) = IARD + INIURDI ( u, i )
            k = k + 1
!           para los modos arrancables
            do modo = 2, NumModRD ( u )
                if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!                   si es unidad disponible y asignable en ese modo
                    if ( DispoURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
!                       coeficiente de la variable de asignacion de reserva no rodante
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IARNRRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    endif
                endif
            enddo
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Relacion entre paro y asignacion de reservas no rodantes unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
        endif
    enddo

enddo

! restricciones de limites de reserva no rodante suplementaria
write ( 777,* ) 'Inicia restricciones de reserva no rodante suplementaria para URD:', m + 1

! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           para todos los intervalos
            do i = 1 , NTINTR
!               si es unidad disponible, asignable  y cordinable en este periodo
                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
                
!                   limite inferior
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNRSRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva no rodante
                    aaMILP ( k  ) = -CalOferResNRxRD ( u, modo, i )
                    jcolMILP ( k ) = IARNRRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'G'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Reserva no rodante suplementaria unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                
!                   limite superior
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNRSRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva no rodante
                    aaMILP ( k  ) = -OferResNRxRD ( u, modo, i )
                    jcolMILP ( k ) = IARNRRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Reserva no rodante suplementaria unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            enddo
        endif
    enddo
enddo

! restricciones de limites de reserva no rodante de diez minutos y suplementaria
write ( 777,* ) 'Inicia restricciones de reserva no rodante de 10 min y suplem URD:', m + 1

! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos arrancables
    do modo = 2, NumModRD ( u )
        if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!           para todos los intervalos
            do i = 1 , NTINTR
!               si es unidad disponible, asignable  y cordinable en este periodo
                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
                    maximo =  max ( OferResNR10RD ( u, modo, i ), OferResNRxRD ( u, modo, i ) )
!                   coeficiente de la variable de reserva no rodante de diez
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de reserva no rodante suplementaria
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNRSRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion de reserva no rodante
                    aaMILP ( k  ) = -maximo
                    jcolMILP ( k ) = IARNRRD + INIURDI ( u, i ) + modo - 1
                    k = k + 1
!                   lados derechos de las restriciones
	                m = m + 1
                    bMILP ( m ) = 0.0
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Reserva no rodante de 10 y suplementaria unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                endif
            enddo
        endif
    enddo
enddo

200 format (i3)
    
return
end



subroutine TFactModoOperRD ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de transiciones factibles entre modos de    *
! operacion en unidades de rango discontinuo                          *
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

Integer k, m, modo, modoO, modoD, i, u

real*8  beta
character*3 leti

write ( 777,* ) 'Inicia restricciones de que impiden arranque y paro simultaneo RD:', m + 1

! para cada unidad
do u = 1, NumUniRD
! para todos los periodos
    do i = 1, NTINTR
!       para los modos arrancables
        do modo = 2, NumModRD ( u )
            if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!               coeficiente de la variable de arranque en el periodo
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IARRD + INIURDI ( u, i ) + modo - 1
                k = k + 1
            endif
        enddo
!       coeficiente de la variable de paro
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IPRD + u + (i-1)*NumUniRD - 1
        k = k + 1
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 1.0
!       sentido de la restriccion
        sMILP ( m ) = 'L'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Se impide arranque y paro simultaneo unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

            
write ( 777,* ) 'Inicia restricciones de transiciones factib entre modos de RangoD:', m + 1

! unidades de rango discontinuo
do u = 1 , NumUniRD
!   primer intervalo
!   para todos los modos de operacion
    do modoO = 1, NumModRD ( u )
        beta = 1.0
!       si la unidad esta en el modo origen en condiciones iniciales
        if ( EstadoCIURD ( u, modoO ) .gt. 0 ) then
            beta = -1.0
        endif
!       para todos los modos de operacion a pasar desde el origen
        do modoD = 1, NumModRD(u)
!           si la transicion es factible
            if ( TransFacti ( u, modoO, modoD ) .gt. 0 ) then
!               coeficiente de la variable de asignacion para operacion
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IARD + INIURDI ( u, 1 ) + modoD - 1
                k = k + 1
!           la transicion es infactible
            else
!               coeficiente de la variable de asignacion para operacion
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IARD + INIURDI ( u, 1 ) + modoD - 1
                k = k + 1
            endif
        enddo
        m = m + 1
        if ( m == 128837 ) then
            continue
        endif
!       lado derecho de la restriccion
        bMILP ( m ) = beta
!       sentido de la restriccion
        sMILP ( m ) = 'L'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( 779,* ) m, ',', '"Transiciones factibles entre modos unidad URD: '//trim(nombunird(u))//'"'
        
    enddo
!   Para el resto de intervalos
    do i = 2 , NTINTR
!       para todos los modos de operacion
        do modoO = 1, NumModRD ( u )
!           coeficiente de la variable de asignacion para operacion en el modo
!           en el intervalo anterior
            aaMILP ( k ) = 2.0
!           columna asociada
            jcolMILP( k ) = IARD + INIURDI ( u, i-1 ) + modoO - 1
            k = k + 1
!           para todos los modos de operacion a pasar desde el origen
            do modoD = 1, NumModRD(u)
!               si la transicion es factible
                if ( TransFacti ( u, modoO, modoD ) .gt. 0 ) then
!                   coeficiente de la variable de asignacion para operacion
                    aaMILP ( k ) = -1.0
!                   columna asociada
                    jcolMILP( k ) = IARD + INIURDI ( u, i ) + modoD - 1
                    k = k + 1
!               la transicion es infactible
                else
!                   coeficiente de la variable de asignacion para operacion
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IARD + INIURDI ( u, i ) + modoD - 1
                    k = k + 1
                endif
            enddo
            m = m + 1
            if ( m == 128837 ) then
                continue
            endif
!           lado derecho de la restriccion
            bMILP ( m ) = 1.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Transiciones factibles entre modos unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
        enddo
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de relacion entre asign, arran y paro RangoD:', m + 1

! unidades de rango discontinuo
do u = 1 , NumUniRD
!   primer intervalo
!   coeficiente de la variable de paro
    aaMILP ( k ) = 1.0
!   columna asociada
    jcolMILP( k ) = IPRD + u - 1
    k = k + 1
    beta = 0.0
!   para todos los modos de operacion, excepto el apagado
    do modoO = 2, NumModRD ( u )
!       si la unidad esta en el modo origen en condiciones iniciales
        if ( EstadoCIURD ( u, modoO ) .gt. 0 ) then
            beta = beta + 1.0
        endif
!       coeficiente de la variable de asignacion para operacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IARD + INIURDI ( u, 1 ) + modoO - 1
        k = k + 1
    enddo
!   para todos los modos de operacion arrancables
    do modoD = 2, NumModRD(u)
!       si la transicion es factible
        if ( TransFacti ( u, 1, modoD ) .gt. 0 ) then
!           coeficiente de la variable de asignacion durante el arranque
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IADARD + INIURDI ( u, 1 ) + modoD - 1
            k = k + 1
!           coeficiente de la variable de arranque en el siguiente periodo
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = IARRD + INIURDI ( u, 1 ) + modoD - 1
            k = k + 1
        endif
    enddo
    m = m + 1
!   lado derecho de la restriccion
    bMILP ( m ) = beta
!   sentido de la restriccion
    sMILP ( m ) = 'E'
!   inicio de la siguiente restriccion
    irowMILP ( m + 1 ) = k
    write ( 779,* ) m, ',', '"Relacion entre asign, arran y paro unidad URD: '//trim(nombunird(u))//'"'
    
!   Para el resto de intervalos
    do i = 2 , NTINTR
!       coeficiente de la variable de paro
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IPRD + u + (i-1)*NumUniRD - 1
        k = k + 1
!       para todos los modos de operacion, excepto el apagado
        do modoO = 2, NumModRD ( u )
!           coeficiente de la variable de asignacion para operacion en el intervalo
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IARD + INIURDI ( u, i ) + modoO - 1
            k = k + 1
!           coeficiente de la variable de asignacion para operacion en el intervalo anterior
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = IARD + INIURDI ( u, i-1 ) + modoO - 1
            k = k + 1
        enddo
!       para todos los modos de operacion arrancables
        do modoD = 2, NumModRD(u)
!           si la transicion es factible
            if ( TransFacti ( u, 1, modoD ) .gt. 0 ) then
!               coeficiente de la variable de asignacion durante el arranque en el intervalo
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IADARD + INIURDI ( u, i ) + modoD - 1
                k = k + 1
!               coeficiente de la variable de asignacion durante el arranque en el intervalo anterior
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IADARD + INIURDI ( u, i-1 ) + modoD - 1
                k = k + 1
!               coeficiente de la variable de arranque en el siguiente periodo
                aaMILP ( k ) = -1.0
!               columna asociada
                jcolMILP( k ) = IARRD + INIURDI ( u, i ) + modoD - 1
                k = k + 1
            endif
        enddo
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'E'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Relacion entre asign, arran y paro unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de maximo numero de transici entre modos RD :', m + 1

! restricciones de maximo numero de transiciones entre modos
! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos de operacion
    do modoO = 1, NumModRD ( u )
!       para todos los modos de operacion a pasar desde el origen
        do modoD = 1, NumModRD(u)
!           si la transicion es factible y hacia un modo distinto
            if ( TransFacti ( u, modoO, modoD ) .gt. 0 .and. modoO .ne. modod ) then
                beta = -1.0
!               primer intervalo
!               si la unidad esta en el modo origen en condiciones iniciales
                if ( EstadoCIURD ( u, modoO ) .gt. 0 ) then
                    beta = beta + 1.0
                endif
!               coeficiente de la variable de asignacion para operacion en el modo destino
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = IARD + INIURDI ( u, 1 ) + modoD - 1
                k = k + 1
!               para todos los restantes intervalos
                do i = 2 , NTINTR
!                   coeficiente de la variable de asignacion para operacion en el origen e intervalo anterior
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IARD + INIURDI ( u, i-1 ) + modoO - 1
                    k = k + 1
!                   coeficiente de la variable de asignacion para operacion en el destino e intervalo actual
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IARD + INIURDI ( u, i ) + modoD - 1
                    k = k + 1
                    beta = beta - 1.0
                enddo
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m )   = NumMaxTrans ( u, modoO, modoD  ) - beta
!               sentidos de las restriciones
                sMILP ( m ) = 'L'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Maximo numero de transiciones entre modos unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
            endif
        enddo
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de inicio de operacion para los modos de URD:', m + 1

! para todas las unidades
do u = 1 , NumUniRD
!   para todos los modos excepto el modo en paro
    do modoO = 2, NumModRD ( u )
!       para todos los modos de operacion a pasar desde el origen
        do modoD = 2, NumModRD(u)
!           si la transicion es factible y hacia un modo distinto
            if ( TransFacti ( u, modoO, modoD ) .gt. 0 .and. modoO .ne. modoD ) then
!               periodos de operacion
                do i = 1, NTINTR
                    beta = -1.0
!                   si se puede inidcar una transicion
                    if ( i - TiempoTrans ( u, modoO, modoD ) .gt. 0 .and. AsignURD ( u, modoO, i ) .eq. 1 .and. AsignURD ( u, modoD, i ) .eq. 1 ) then
                        if ( i .eq. 1 ) then
!                           si la unidad esta en el modo origen
                            if ( EstadoCIURD ( u, modoO ) .eq. 1 ) then
                                beta = beta + 1.0
                            endif
                        else
!                           variable de asignacion modo origen en intevalo anterior
                            aaMILP ( k ) = -1.0
                            jcolMILP ( k ) = IARD + INIURDI ( u, i-1 ) + modoO - 1
                            k = k + 1
                        endif
!                       variable de asignacion modo destino en intevalo actual
                        aaMILP ( k ) = -1.0
                        jcolMILP ( k ) = IARD + INIURDI ( u, i ) + modoD - 1
                        k = k + 1
!                       variable de arranque
                        aaMILP ( k ) = 1.0
                        jcolMILP ( k ) = IOMARD + INALPHAT ( u, i-TiempoTrans ( u, modoO, modoD ) ) + (modoO-1)*NumModRD ( u ) + modoD - 1 
                        k = k + 1
!                       lados derechos de las restriciones
                        m = m + 1
                        bMILP ( m )   = beta
!                       sentidos de las restriciones
                        sMILP ( m ) = 'G'
!                       apuntador al siguiente renglon
                        irowMILP ( m + 1 ) = k
                        write ( leti, 200 ) i
                        write ( 779,* ) m, ',', '"Inicio de operacion para los modos unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
                    else
!                       si la unidad es asignable en el modo destino e intervalo actual, diferente modo destino a condiciones iniciales
                        if ( AsignURD ( u, modoD, i ) .eq. 1 .and. modoO .ne. modoD .and. EstadoCIURD ( u, modoD ) .ne. 1 ) then
                            ubMILP ( IARD + INIURDI ( u, i ) + modoD - 1 ) = 0.0
                        endif
                    endif
                enddo
            else
!               periodos de operacion
                do i = 1, NTINTR
!                   variable de arranque
                    ubMILP ( IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 ) = 0.0
                enddo
            endif
        enddo
    enddo
enddo

! se forman restricciones que obligan a estar un solo modo en cada intervalo
call UnModoOperRD ( k, m )

200 format (i3)
    
return
end
    
    
    
subroutine UnModoOperRD ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de estar en un solo modo de operacion en    *
! unidades de rango discontinuo.                                      *
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

Integer k, m, modo, i, u
character*3 leti

write ( 777,* ) 'Inicia restricciones de estar en un solo modo de operacion RangoD:', m + 1

! unidades de rango discontinuo
do u = 1 , NumUniRD
!   Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los modos de operacion
        do modo = 1, NumModRD ( u )
!           coeficiente de la variable de asignacion para operacion del modo
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IARD + INIURDI ( u, i ) + modo - 1
            k = k + 1
        enddo
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 1.0
!       sentido de la restriccion
        sMILP ( m ) = 'E'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Estar en un solo modo de operacion unidad URD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

200 format (i3)
    
return
end
