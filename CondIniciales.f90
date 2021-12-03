subroutine CondIniciales  ( k , m )
! ---------------------------------------------------------------------
! Se lee una solucion inicial y se forman las restricciones de limites*
! sobre los flujos de grupos de ramas y estimacion de perdidas.       *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2020                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
Use ParGloRed, only: RamActiva, nomgruram, bangruram

implicit none

Integer i, k, m, u, clave

integer ierror, rama ( 100 ), RamaRes ( 100 )
character*3000 letaux
character*20 nombre

ierror = 0
letaux = 'letaux'
RamaRes = 0
i = 0

! Numero de restricciones adicionales al problema MILP
NumResAdi = 0
! Inicializa informacion sobre restricciones adicionales
InfRestAdi = 0
IntRestAdi = 0
IsenRestAdi = 'E'

! Se lee un conjunto activo existente
OPEN (UNIT = 51, FILE = rut_res//'CONJACTIV.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

if ( ierror .eq. 0 ) then
    write ( 1, 100 ) 'Conjunto activo inicial' 
    write ( 1, 100 ) 'Rama          Nombre' 
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 51, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0  ) then
            i = i + 1
            read ( letaux, * )  rama (i), nombre
!           si la rama esta activa
            if ( bangruram(i) .gt. 0 ) then
                RamaRes ( rama (i) ) = 1
                RamActiva ( rama (i) ) = 1
                write ( 1, 300 ) rama (i), Nombre
            else
                i = i - 1
            endif
        endif
    enddo
endif

close (UNIT = 51, status = 'delete')

! Numero de grupos de ramas en el conjunto activo inicial
RamActIni = i

OPEN ( UNIT = 55, FILE = rut_res//'CONJACTIV.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='unknown', RECORDSIZE = 250 )
do i = 1, RamActIni
    write ( 55, 400 )  rama (i), nomgruram ( rama (i) ) 
enddo

100  format ( a )
300  format ( I3, A25 )
400  format ( I3, ',', '"',A25,'"',',' )

! Se lee un punto base inicial existente
OPEN (UNIT = 51, FILE = rut_res//'RESGEN.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

if ( ierror .eq. 0 ) then
!   para todas las unidades de rango continuo
    do u = 1, NumUniRC
	    read ( 51, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0  ) then
            read ( letaux, * )  nombre, clave, ( GENUNRC(u,i), i=1,NTINTR ) 
        endif
    enddo
!   para todas las unidades de rango discontinuo
    do u = 1, NumUniRD
	    read ( 51, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0  ) then
            read ( letaux, * )  nombre, clave, ( GENUNRD(u,i), i=1,NTINTR ) 
        endif
    enddo
!   para todas las unidades hidro
    do u = 1, NumUniHid
	    read ( 51, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0  ) then
            read ( letaux, * )  nombre, clave, ( GENUNH(u,i), i=1,NTINTR ) 
        endif
    enddo
!   para todas las unidades renovables
    do u = 1, NumUniRE
	    read ( 51, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0  ) then
            read ( letaux, * )  nombre, clave, ( GENUNRE(u,i), i=1,NTINTR ) 
        endif
    enddo
endif

close (51)

xMILP = 0.0

GENUNRC = GENUNRC/Base
GENUNRD = GENUNRD/Base
GENUNH = GENUNH/Base
GENUNRE = GENUNRE/Base

! se calculan perdidas y sensibilidades con la solucion inicial
call CalSnsPerIntervalo ( 1, 1 )

! se forman restricciones para aproximar inicialmente las perdidas
call PerdInic ( k, m )

! si no se desea incluir la estimacion de perdidas
if ( SiPerdidas .eq. 0 ) then
!   Inicio de restricciones de perdidas
    IRPERD = m + 1
endif

! Se forman las restricciones de grupos de ramas del conjunto activo inicial
call LimFlures ( k, m, RamaRes )

! si se desean considerar restricciones de transmision
if ( SiTransmision .eq. 0 ) then
    IRTRANS = m + 1
endif


return
end

    
subroutine LimFluRes  ( k , m, RamaRes )
! ---------------------------------------------------------------------
! Se forma la restriccion de limites sobre los flujos de grupos de    *
! ramas del MILP                                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2019                                                  *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
Use ParGloRed

implicit none

Integer i, d, k, l, m, modo, n, nodo, u, br, r, s
integer componente, componente_1, NoDi
real*8  coeficiente
character*3 leti

integer RamaRes ( 100 )


write ( 777,* ) 'Inicia restricciones de limite superior en grupos de ramas restri:', m + 1

IRTRANS = m + 1
! para todos los subsistemas (islas)
do l = 1, numsis
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
!       para todos los intervalos       
        do i = 1 , NTINTR
!           se calcuan sensibilidades
            call CalculaSensibilidadesFlujos ( l, i, 1, 1 )
!           Para todos los grupos de ramas en el sistema
            do r = 1, NumGruRamSis ( l )
                if ( RamaRes(r) .gt. 0 ) then
                    br = inagruram ( r )
!                   contador de restricciones adicionales
                    NumResAdi = NumResAdi + 1
!                   Informacion de tipo de restriccion adicional
                    InfRestAdi ( NumResAdi ) = r
!                   Informacion de intervalo de restriccion adicional
                    IntRestAdi ( NumResAdi ) = i
!                   Informacion de sentido de restriccion adicional
                    IsenRestAdi ( NumResAdi ) = 'L'
!                   coeficientes de variables de generacion de rango continuo
                    do u = 1 , NumUniRC
                        coeficiente = 0.0
!                       para todos los nodos distribuidos
                        do NoDi = 1, NoNodDisRC ( u, i )
                            nodo = Tempnodorc ( u, NoDi, i )
	                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURC ( u, i ) .ne. 0  ) then
                                coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*facdistgen ( u, NoDi, i )
                            endif
                        enddo
!                       coeficiente de generacion
                        aaMILP ( k ) = coeficiente
                        jcolMILP ( k ) = IGRC + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    enddo
!                   coeficientes de variables de generacion de rango discontinuo
                    do u = 1 , NumUniRD
!                       para todos los modos de operacion
                        do modo = 2, NumModRD(u)
                            coeficiente = 0.0
                            do componente = 1, NumCompXModo ( u, modo )
!                               para todas la componentes de la unida de rango discontinuo
                                do componente_1 = 0, NumCompRD ( u ) - 1
                                    if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                                        nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
	                                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURD ( u, modo, i ) .ne. 0  ) then
                                            coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*GenCompXModo  ( u, modo, componente )
                                            exit
                                        endif
                                    endif
                                enddo
                            enddo
!                           coeficiente de generacion
                            aaMILP ( k ) = coeficiente
                            jcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        enddo
                    enddo
!                   coeficientes de variables de generacion hidro
                    do u = 1 , NumUniHid
                        nodo = nodoh ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoUH ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            aaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
                            k = k + 1
                        endif
                    enddo
!                   coeficientes de variables de generacion renovables
                    do u = 1 , NumUniRE
                        nodo = nodounre ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURE ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            aaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            jcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 1
                            k = k + 1
                        endif
                    enddo
!                   coeficientes de variables de nivel de demanda y corte de carga
                    do d = 1 , NumOferDem
                        coeficiente = 0.0
!                       para todos los nodos distribuidos
                        do NoDi = 1, NoNodDisCar ( d , i )
                            nodo = Tempnodocar ( d, NoDi, i )
	                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
                                coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*facdistcar ( d, NoDi, i )
                            endif
                        enddo
!                       para todos los segmentos de curva de ofertas de compra
                        do s = 1, NumBloDem( d, i )
!                           coeficiente de la variable de demanda aceptada en el segmento
                            aaMILP ( k ) = -coeficiente
!                           columna asociada
                            jcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 1
                            k = k + 1
                        enddo
!                       coeficiente de corte de demanda fija
                        aaMILP ( k ) = coeficiente
!                       columna asociada
                        jcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 1
                        k = k + 1
                    enddo
!                   para todos los nodos
                    do n = 1, NumNodSis ( l )
                        nodo = inaumnodsis ( n )
                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
!                           coeficiente de variable de excedente
                            aaMILP ( k ) = -SnsGruRarInyNod ( br, nodo )
                            jcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 1
                            k = k + 1
                        endif
                    enddo
!                   variable artificial de excedente de flujo en grupos de ramas
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k ) = IAEF + r + (i-1)*NumGruRamSis ( l ) - 1
                    k = k + 1
!                   variable artificial de deficit en grupos de ramas
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IACF + r + (i-1)*NumGruRamSis ( l ) - 1
                    k = k + 1
                    m = m + 1
!                   lados derechos de las restriciones
                    bMILP ( m )   = potmaxgruram ( br, i ) + CoeSnsGruRar ( br, i )
!                   sentidos de las restriciones
                    sMILP ( m ) = 'L'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Limite superior en grupos de ramas restri :'//trim(nomgruram(br))//' intervalo '//trim(leti)//'"'
                endif
            enddo
        enddo
    endif
enddo

write ( 777,* ) 'Inicia restricciones de limite inferior en grupos de ramas restri:', m + 1

! para todos los subsistemas (islas)
do l = 1, numsis
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
!       para todos los intervalos       
        do i = 1 , NTINTR
!           se calcuan sensibilidades
            call CalculaSensibilidadesFlujos (l, i, 1, 1 )
!           Para todos los grupos de ramas en el sistema
            do r = 1, NumGruRamSis (l)
                if ( RamaRes(r) .gt. 0 ) then
                    br = inagruram ( r )
!                   contador de restricciones adicionales
                    NumResAdi = NumResAdi + 1
!                   Informacion de tipo de restriccion adicional
                    InfRestAdi ( NumResAdi ) = r
!                   Informacion de intervalo de restriccion adicional
                    IntRestAdi ( NumResAdi ) = i
!                   Informacion de sentido de restriccion adicional
                    IsenRestAdi ( NumResAdi ) = 'G'
!                   coeficientes de variables de generacion de rango continuo
                    do u = 1 , NumUniRC
                        coeficiente = 0.0
!                       para todos los nodos distribuidos
                        do NoDi = 1, NoNodDisRC ( u, i )
                            nodo = Tempnodorc ( u, NoDi, i )
	                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURC ( u, i ) .ne. 0  ) then
                                coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*facdistgen ( u, NoDi, i )
                            endif
                        enddo
!                       coeficiente de generacion
                        aaMILP ( k ) = coeficiente
                        jcolMILP ( k ) = IGRC + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    enddo
!                   coeficientes de variables de generacion de rango discontinuo
                    do u = 1 , NumUniRD
!                       para todos los modos de operacion
                        do modo = 2, NumModRD(u)
                            coeficiente = 0.0
                            do componente = 1, NumCompXModo ( u, modo )
!                               para todas la componentes de la unida de rango discontinuo
                                do componente_1 = 0, NumCompRD ( u ) - 1
                                    if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                                        nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
	                                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURD ( u, modo, i ) .ne. 0  ) then
                                            coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*GenCompXModo  ( u, modo, componente )
                                            exit
                                        endif
                                    endif
                                enddo
                            enddo
!                           coeficiente de generacion
                            aaMILP ( k ) = coeficiente
                            jcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        enddo
                    enddo
!                   coeficientes de variables de generacion hidro
                    do u = 1 , NumUniHid
                        nodo = nodoh ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoUH ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            aaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
                            k = k + 1
                        endif
                    enddo
!                   coeficientes de variables de generacion renovables
                    do u = 1 , NumUniRE
                        nodo = nodounre ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURE ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            aaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            jcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 1
                            k = k + 1
                        endif
                    enddo
!                   coeficientes de variables de nivel de demanda y corte de carga
                    do d = 1 , NumOferDem
                        coeficiente = 0.0
!                       para todos los nodos distribuidos
                        do NoDi = 1, NoNodDisCar ( d, i )
                            nodo = Tempnodocar ( d, NoDi, i )
	                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
                                coeficiente = coeficiente + SnsGruRarInyNod ( br, nodo )*facdistcar ( d, NoDi, i )
                            endif
                        enddo
!                       para todos los segmentos de curva de ofertas de compra
                        do s = 1, NumBloDem( d, i )
!                           coeficiente de la variable de demanda aceptada en el segmento
                            aaMILP ( k ) = -coeficiente
!                           columna asociada
                            jcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 1
                            k = k + 1
                        enddo
!                       coeficiente de corte de demanda fija
                        aaMILP ( k ) = coeficiente
!                       columna asociada
                        jcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 1
                        k = k + 1
                    enddo
!                   para todos los nodos
                    do n = 1, NumNodSis ( l )
                        nodo = inaumnodsis ( n )
                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
!                           coeficiente de variable de excedente
                            aaMILP ( k ) = -SnsGruRarInyNod ( br, nodo )
                            jcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 1
                            k = k + 1
                        endif
                    enddo
!                   variable artificial de excedente de flujo en grupos de ramas
                    aaMILP ( k ) = -1.0
                    jcolMILP ( k ) = IAEF + r + (i-1)*NumGruRamSis ( l ) - 1
                    k = k + 1
!                   variable artificial de deficit en grupos de ramas
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IACF + r + (i-1)*NumGruRamSis ( l ) - 1
                    k = k + 1
                    m = m + 1
!                   lados derechos de las restriciones
                    bMILP ( m )   = potmingruram ( br, i ) + CoeSnsGruRar ( br, i )
!                   sentidos de las restriciones
                    sMILP ( m ) = 'G'
!                   apuntador al siguiente renglon
                    irowMILP ( m + 1 ) = k
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Limite inferior en grupos de ramas restri::'//trim(nomgruram(br))//' intervalo '//trim(leti)//'"'
                endif
            enddo
        enddo
    endif
enddo

200 format (i3)
    
return
end


subroutine PerdInic ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones para la representacion inicial de las       *
! perdidas.                                                           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2019                                                  *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use symtypes

use ParGloRed, only: NumNodSis, SnsPerNod, InAumNodSis, SnsPerIntIny, PerIntervalo

Integer i, isla, d, k, l, m, modo, n, nodo, u, sistema
integer componente, componente_1, s, sis, NoDi
real*8  coeficiente

character*3 leti

sis = 0

! Inicio de restricciones de perdidas
IRPERD = m + 1

write ( 777,* ) 'Inicia restricciones de estimacion de perdidas en transmision    :', m + 1

! para todos los subsistemas (islas)
do l = 1, numsis
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
        sis = sis + 1
!       para todos los intervalos       
        do i = 1 , NTINTR
!           contador de restricciones adicionales
            NumResAdi = NumResAdi + 1
!           Informacion de tipo de restriccion adicional
            InfRestAdi ( NumResAdi ) = 0
!           Informacion de intervalo de restriccion adicional
            IntRestAdi ( NumResAdi ) = i
!           coeficientes de variables de generacion de rango continuo
            do u = 1 , NumUniRC
                isla = IslaGenRC ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
                    coeficiente = 0.0
!                   para todos los nodos distribuidos
                    do NoDi = 1, NoNodDisRC ( u, i )
                        nodo = Tempnodorc ( u, NoDi, i )
                        coeficiente = coeficiente - SnsPerNod(nodo,i)*facdistgen ( u, NoDi, i )
                    enddo
!                   coeficiente de generacion
                    aaMILP ( k ) = coeficiente
                    jcolMILP ( k ) = IGRC + u + (i-1)*NumUniRC - 1
                    k = k + 1
                endif
            enddo
!           coeficientes de variables de generacion de rango discontinuo
            do u = 1 , NumUniRD
                isla = IslaGenRD ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   para todos los modos de operacion
                    do modo = 2, NumModRD(u)
                        coeficiente = 0.0
                        do componente = 1, NumCompXModo ( u, modo )
!                           para todas la componentes de la unida de rango discontinuo
                            do componente_1 = 0, NumCompRD ( u ) - 1
                                if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                                    nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
                                    coeficiente = coeficiente + SnsPerNod(nodo,i)*GenCompXModo  ( u, modo, componente )
                                    exit
                                endif
                            enddo
                        enddo
!                       coeficiente de generacion
                        aaMILP ( k ) = -coeficiente
                        jcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    enddo
                endif
            enddo
!           coeficientes de variables de generacion hidro
            do u = 1 , NumUniHid
                isla = IslaGenH ( u )
                nodo = nodoh ( u, i )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de generacion
                    aaMILP ( k ) = -SnsPerNod(nodo,i)
                    jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
            enddo
!           coeficientes de variables de generacion renovable
            do u = 1 , NumUniRE
                isla = IslaGenRE ( u )
                nodo = nodounre ( u, i )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de generacion
                    aaMILP ( k ) = -SnsPerNod(nodo,i)
                    jcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 1
                    k = k + 1
                endif
            enddo
!           coeficientes de variables de nivel de demanda y corte de carga
            do d = 1 , NumOferDem
                isla = IslaDem ( d )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
                    coeficiente = 0.0
!                   para todos los nodos distribuidos
                    do NoDi = 1, NoNodDisCar ( d, i )
                        nodo = Tempnodocar ( d, NoDi, i )
                        coeficiente = coeficiente + SnsPerNod ( nodo, i )*facdistcar ( d, NoDi, i )
                    enddo
!                   para todos los segmentos de curva de ofertas de compra
                    do s = 1, NumBloDem( d, i )
!                       coeficiente de demanda en el bloque
                        aaMILP ( k ) = coeficiente
!                       columna asociada
                        jcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 1
                        k = k + 1
                    enddo
!                   coeficiente de corte de demanda fija
                    aaMILP ( k ) = -coeficiente
!                   columna asociada
                    jcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 1
                    k = k + 1
                endif
            enddo
!           para todos los nodos
            do n = 1, NumNodSis ( l )
                nodo = InAumNodSis(n)
!               coeficiente de variable de excedente
                aaMILP ( k ) = SnsPerNod(nodo,i)
                jcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 1
                k = k + 1
            enddo
!           variable de perdidas
!           coeficiente de la variable
            aaMILP ( k ) = 1.0 
            jcolMILP ( k ) = IPERD + i + (sis-1)*numsis_act - 1
            k = k + 1
            m = m + 1
!           lados derechos de las restriciones
            bMILP ( m )   = PerIntervalo(i) - SnsPerIntIny(i)
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write (leti, 200 ) i
            write ( 779,* ) m, ',', '"Estimacion de perdidas en transmision en intervalo: '//trim(leti)//'"'
        enddo
    endif
enddo

200 format (i3)
    
return
end
