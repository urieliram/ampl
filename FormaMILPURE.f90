  
! ---------------------------------------------------------------------
! Forman las restricciones del problema de asignacion y despacho de   *
! unidades (MILP), renovables.                                        *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine RestURE ( k, m, sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

Integer k, m,sistema, uunidad, IERROR

character*1 ssistema

! se forma la restriccion de nivel de generacion en unidades renovables
call NivelGenRE ( k, m )

! se forma la restriccion de segmentos de ofertas de venta en unidades renovables
call SegVenRE ( k, m )

! se forman restricciones de limites de generacion en unidades renovables
call LimGenORE ( k, m )

! se cosidera la asignabilidad y disponibilidad del escenario para unidades renovables
call EstadoUniRE

! si existen unidades renovables en el sistema
if ( NumuniRE .gt. 0 ) then

!   Se concatena el numero de subsistema al nombre de los archivos de debugger
    write( ssistema, '(I1)' )  sistema
   
!   se abren archivos para escritura de resultados de renovables
!   Generacion de uniaddes renovables
    uunidad = 501 + sistema
    UnichauRE = uunidad
    OPEN ( UNIT = UnichauRE, FILE = RUT_RES//'r_chauRE'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1250 )
    
endif

return
end

        
subroutine NivelGenRE ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de nivel de generacion en unidades          *
! renovables.                                                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Diciembre de 2015                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, kv, m, i, u, s
character*3 leti

write ( 777,* ) 'Inicia restricciones de nivel de generacion en unidades renovable:', m + 1

! unidades renovables
kv = IGABRE
do u = 1 , NumUniRE
! Para todos los intervalos
    do i = 1 , NTINTR
        if ( m == 3963 ) then
            continue
        endif
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGRE + u + (i-1)*NumUniRE - 1
        k = k + 1
!       para todos los segmentos de curva de ofertas de venta
        do s = 1, NumBloVRE( u, i )
!           coeficiente de la variable de generacion aceptada en el bloque
            aaMILP ( k ) = -1.0
!           columna asociada
            jcolMILP( k ) = kv
            k = k + 1
            kv = kv + 1
        enddo
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'E'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"nivel de generacion en unidad renovable URE: '//trim(nombunire (u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

200 format (i3)
    
return
end
    

subroutine SegVenRE ( k, m )
! ---------------------------------------------------------------------
! Se forma la restriccion de segmentos de ofertas de venta en         *
! unidades renovables.                                                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, k, m, u, s, kv
character*3 leti

! unidades renovables
kv = IGABRE
do u = 1 , NumUniRE
! Para todos los intervalos
    do i = 1 , NTINTR
        do s = 1, NumBloVRE( u, i )
!           limite superior de la variable de generacion en el segmento
            lbMILP ( kv ) = 0.0
!           coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = kv
            k = k + 1
!           coeficiente de variable de asignacion
            aaMILP ( k ) = -OferVenEnerRE ( u, s, i )
!           columna asociada
            jcolMILP( k ) = IARE + u + (i-1)*NumUniRE - 1
            k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'L'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            kv = kv + 1
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Segmentos de venta de energia unidad renovable URE: '//trim(nombunire (u))//' intervalo: '//trim(leti)//'"'
        enddo
    enddo
enddo

200 format (i3)
    
return
end    


subroutine LimGenORE ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de generacion operativo en       *
! unidades renovables.                                                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k, i, m, u
character*3 leti

write ( 777,* ) 'Inicia restricciones de limite maximo de generacion operativo URE:', m + 1

! unidades renovables
do u = 1 , NumUniRE
! Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGRE + u + (i-1)*NumUniRE - 1
        k = k + 1
!       coeficiente de variable de asignacion
        aaMILP ( k ) = -PotMaxGRE ( u, i )
!       columna asociada
        jcolMILP( k ) = IARE + u + (i-1)*NumUniRE - 1
        k = k + 1
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'L'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Limite maximo de generacion operativo URE: '//trim(nombunire (u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de generacion operativo URE:', m + 1

! unidades renovables
do u = 1 , NumUniRE
! Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de la variable de generacion
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IGRE + u + (i-1)*NumUniRE - 1
        k = k + 1
!       coeficiente de variable de asignacion
        aaMILP ( k ) = -PotMinGRE ( u, i )
!       columna asociada
        jcolMILP( k ) = IARE + u + (i-1)*NumUniRE - 1
        k = k + 1
        m = m + 1
!       lado derecho de la restriccion
        bMILP ( m ) = 0.0
!       sentido de la restriccion
        sMILP ( m ) = 'G'
!       inicio de la siguiente restriccion
        irowMILP ( m + 1 ) = k
        write ( leti, 200 ) i
        write ( 779,* ) m, ',', '"Limite minimo de generacion operativo URE: '//trim(nombunire (u))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

200 format (i3)
    
return
end    


Subroutine EstadoUniRE
! ---------------------------------------------------------------------
! Restricciones sobre estados de las unidades renovables en el        *
! problema de asignacion (MILP).                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, u

! para las unidades renovables
do u = 1 , NumUniRE
    do i = 1 , NTINTR
!       si es unidad no disponible en este periodo
        if ( DispoURE ( u , i ) .eq. 0 ) then
!           la unidad no se puede operar
            lbMILP ( IARE + u + (i-1)*NumUniRE - 1 ) = 0.0
            ubMILP ( IARE + u + (i-1)*NumUniRE - 1 ) = 0.0
!           si es unidad disponible y no asignables en este periodo
	    elseif ( DispoURE ( u , i ) .eq. 1 .and.  AsignURE ( u , i ) .eq. 0 ) then
!           la unidad debe operar
            lbMILP ( IARE + u + (i-1)*NumUniRE -1 ) = 1.0
            ubMILP ( IARE + u + (i-1)*NumUniRE -1 ) = 1.0
        endif
   enddo
enddo

return
end

    
