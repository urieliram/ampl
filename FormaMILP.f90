! ---------------------------------------------------------------------
! Se definen dimensiones en el problema de asignacion (MILP),         *
! de acuerdo a los parametros de sintonia .                           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine DefineDimensionesMILP

use ParAUHE
use ProblemaAUHE

Implicit none


! No participan unidades de rango continuo
if ( SiUniRC .eq. 0 ) then
   NumUniRC = 0
endif

! No participan unidades de rango discontinuo
if ( SiUniRD .eq. 0 ) then
   NumUniRD = 0
endif

! No participan unidades hidro
if ( SiUniH .eq. 0 ) then
   NumUniHid = 0
endif

! No participan unidades renovables
if ( SiUniRE .eq. 0 ) then
   NumUniRE = 0
endif

! No participan ofertas de demanda
if ( SiOferDem .eq. 0 ) then
   NumBloDem = 0
endif


return

end

    
! ---------------------------------------------------------------------
! Forma el problema de asignacion y despacho de unidades (MILP)       *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2019                                                   *
! ---------------------------------------------------------------------
Subroutine FormaResuelveMILP ( ite, sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

Integer k, m, sistema, ite, bandas, u, ierror

OPEN (UNIT = 779, FILE = RUT_RES//'RestriccionesModelMILP.txt', &
      IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

k = 1
m = 0
irowMILP ( 1 ) = 1

! se forman las variables del problema de asignacion
call VarProAsign ( sistema )

! se forma la primera parte (ofertas de compra-venta) de la funcion objetivo
call FunObj1MILP

! se forma la segunda parte (servicios conexos) de la funcion objetivo
call FunObj2MILP ( sistema )

! se forma la tercera parte (penalizaciones) de la funcion objetivo
call FunObj3MILP ( sistema )

! tipos de variables y cotas simples en unidades de generacion
call TipoLimUni

! si el escenario es de un EXPOST
!if ( TipoEjecu .eq. 2 ) then
!    call LeeGenTR
!   Se calcula el limite inferior de las ofertas de servicios conexos de TODAS las unidades
!    call calc_conexos
!endif

! si el sistema contiene unidades de rango continuo
if ( NumUniRC .gt. 0 ) then
!   se forman restricciones para unidades de rango continuo
    call RestURC ( k, m )
!   si se desea considerar bandas prohibidas y estas existen
    bandas = 0
    do u = 1, NumUniRC
        if ( NoRaOpRC ( u ) .gt. 0 ) then
            bandas = bandas + 1
        endif
    enddo
    if ( SiBandProh .eq. 1 .and. bandas .gt. 0 ) then
!       se forman las restricciones de asociacion entre asignacion de rango y asignacion de operacion
        call RangOperRC ( k, m )
    endif
endif

! si el sistema contiene unidades hidro
if ( NumUniHid .gt. 0 ) then
!   se forman restricciones para unidades hidro
    call RestUH ( k, m, sistema )
!   si se desea considerar bandas prohibidas y estas existen
    bandas = 0
    do u = 1, NumUniHid
        if ( NoRaOpH ( u ) .gt. 0 ) then
            bandas = bandas + 1
        endif
    enddo
    if ( SiBandProh .eq. 1 .and. bandas .gt. 0 ) then
!       se forman las restricciones de asociacion entre asignacion de rango y asignacion de operacion
        call RangOperH ( k, m )
    endif
endif

! si el sistema contiene unidades renovables
if ( NumUniRE .gt. 0 ) then
!   se forman restricciones para unidades renovables
    call RestURE ( k, m, sistema )
endif

! si el sistema contiene unidades de rango discontinuo
if ( NumUniRD .gt. 0 ) then
!   se forman restricciones para unidades de rango discontinuo
    call RestURD ( k, m, sistema )
endif

! restricciones de demanda
call NivDemanda ( k, m )

! se forma la restriccion de balance de potencia horario
call BalPot ( k, m )

! se forma la restriccion de balance de potencia y perdidas
!call GenerPerd ( k, m )

! se forma restriccion de limites en grupos de importacion
!call LimGpoImportacion ( k, m )

! se forma restriccion de limites en grupos de exportacion
!call LimGpoExportacion ( k, m )

! si el problema contiene ofertas de reserva por zona
if ( SiOferComResZona .gt. 0 ) then
!   se forman las restricciones de requerimiento del CENACE de reservas por zona
    call ReqResZona ( k, m )
!   si es el EXPOST
    if ( TipoEjecu .eq. 4 ) then
!       se forman las cotas superiores de la reserva asignada de 10 minutos
        call CotaReqRes10 ( k, m )
    endif
endif

! si el problema contiene ofertas de reserva por sistema
if ( SiOferComResSis .gt. 0 ) then
!   se forman las restricciones de requerimiento del CENACE de reservas por sistema
    call ReqResSis ( k, m )
endif

! si el problema contiene limitaciones de energia termo
if ( SiEnerTer .gt. 0 ) then
!   se forman las restricciones de grupos de energia termo
    call LimEnerTer ( k, m )
!    call LimEnerTer_new ( k, m )
endif

! si el problema contiene limitaciones de combustible
if ( SiGpoGas .gt. 0 ) then
!   se forman las restricciones de grupos de limitacion de combustible
!    call LimConsComb_new ( k, m )
    call LimConsComb ( k, m )
endif

! si el sistema contiene unidades hidro y se desea considerar el modelado hidro
if ( NumUniHid .gt. 0 .and. SiModHid .eq. 1 ) then

!   Se forman restricciones de balance de agua en embalses del problema de asignacion
    call BalanceEmbalRest ( k, m )

!   Se forman restricciones de politicas de operacion en embalses del problema de asignacion
    call PoliticasOperEmbalRest ( k, m )

!   Se forman restricciones de volumen en transito en vias durante el intervalo 
!   final del problema de asignacion
!    call VolTransViasFinRest ( k, m )

!   Se forman restricciones de limites al gasto turbinado en vias en el problema de asignacion
    call LimiteGastoViasRest ( k, m )

!   Se forman restricciones de limites superiores e inferiores de variables de gasto
!   hidro en el problema de asignacion
    call LimGastoUniHidRest ( k, m )

!   Se forman restricciones de potencia de generacion de las unidades 
!   Hidro en el problema de asignacion
!   Se mete restriccion de generacion hidro en funcion de volumen y gasto en el problema de asignacion solo para escenarios de un dia
    call PotenciaGenHidroRest ( k, m, 1 )
endif

! si se desea utilizar una solucion previa y un conjunto activo previo
if ( SiSolucionInicial .eq. 1 ) then
!   se forman las restricciones de limites sobre los flujos de grupos de ramas y estimacion de perdidas
    call CondIniciales ( k, m )
else
    ! Inicio de restricciones de perdidas
    IRPERD = m + 1
endif

! si es un AUGC y se desea mantener la asignacion de reservas del MDA
if ( TipoEjecu .eq. 1 .and. SiMantReservas .eq. 1 ) then
!   se lee y se forza la asignacion de reservas del MDA
!    call LeeAsigReservas
endif

! si es un MDA o AUGC y se desea considerar asignaciones manuales de reserva de regulacion
if ( TipoEjecu .le. 1 ) then
!   se lee y se forza la asignacion manual de reserva de regulacion para el MDA
!    call LeeAsigRegMDA
endif

! Numero de restricciones en el MILP
NumResAsig = m

! numero de elementos distintos de cero en la matriz de restricciones del MILP
nelemMaMILP   = k - 1

! se crea el problema MILP para el optimizador CPLEX
call CreaMILP ( sistema )

! respalda cotas de variables de asignacion
call AsignaOri

write ( 777,* )
write ( 777,* ) ' Numero de restricciones en el problema de asignacion (MILP):', NumResAsig

! se resuelve el problema MILP por el optimizador CPLEX
call ResuelveMILP ( ite, sistema, 0, 0 )

return
end

    
subroutine FunObj1MILP
! ---------------------------------------------------------------------
! Se forma la primera parte (ofertas de compra-venta) de la funcion   *
! objetivo del problema de asignacion (MILP)                          *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Diciembre de 2018                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, ka, kv, km, kd, d, m, s, u, inicio, modoO, modoD
real*8 Factor

! Coeficientes en la funcion objetivo
objMILP = 0.0

! Precio Tope de Energia (Oferta incremental mas cara)
PrecioTopeEner = 0.0

! Precio Tope de Costo Minimo de Energia (Oferta mas cara)
PrecioTopeEM = 0.0

! Precio Tope de Costo de Arranque (Oferta mas cara)
PrecioTopeArr = 0.0
! Ofertas de compra y venta de energia

! unidades de rango continuo
kv = IGABRC
do u = 1 , NumUniRC
! Para todos los intervalos
    do i = 1 , NTINTR
	    if ( CostoMinGRC(u,i) .gt. PrecioTopeEM ) then
	        PrecioTopeEM = CostoMinGRC(u,i)
	    endif
!       si son ofertas de costo a generacion minima
		if ( TipoOferta .eq. 1 ) then
!       	coeficiente de asignacion en operacion
	        objMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = CostoMinGRC(u,i)
	        if ( PotMinGRC ( u, i ) .gt. 0.0 ) then
!           	coeficiente de generacion en sincronizacion
	            objMILP ( IGDARC + u + (i-1)*NumUniRC - 1 ) = CostoMinGRC(u,i)/PotMinGRC ( u, i )
	        endif
!       	para todos los segmentos de curva de ofertas de venta
	        do s = 1, NumBloVRC( u, i )
!           	coeficiente de segmento de venta
	            objMILP ( kv ) = PreVenEnerRC(u,s,i)
	            kv = kv + 1
	            if ( PreVenEnerRC(u,s,i) .gt. PrecioTopeEner ) then
	                PrecioTopeEner = PreVenEnerRC(u,s,i)
	            endif
        	enddo
!       si son ofertas de costo en vacio
		else
!       	coeficiente de asignacion en operacion
        	objMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = CostoMinGRC(u,i) + PotMinGRC ( u, i )*PreVenEnerRC(u,1,i)
!       	coeficiente de asignacion durante arranque
        	objMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = CostoMinGRC(u,i)
!       	costo de primer segmento (generacion de sincronizacion)
	        objMILP ( IGDARC + u + (i-1)*NumUniRC - 1 ) = PreVenEnerRC(u,1,i)
	        if ( NumBloVRC( u, i ) .gt. 0 ) then
	            kv = kv + 1
	            if ( PreVenEnerRC(u,1,i) .gt. PrecioTopeEner ) then
	                PrecioTopeEner = PreVenEnerRC(u,1,i)
                endif	        
            endif
!       	para los siguientes segmentos de curva de ofertas de venta
        	do s = 2, NumBloVRC( u, i )
!           	coeficiente de segmento de venta
	            objMILP ( kv ) = PreVenEnerRC(u,s,i)
	            kv = kv + 1
	            if ( PreVenEnerRC(u,s,i) .gt. PrecioTopeEner ) then
	                PrecioTopeEner = PreVenEnerRC(u,s,i)
	            endif
	        enddo
		endif
    enddo
enddo

! unidades de rango continuo
ka = IBOARC - 1
do u = 1 , NumUniRC
    inicio = NmBloArrURC ( u )*NTINTR
!   Para todos los intervalos
    do i = 1 , NTINTR
        if ( NmBloArrURC ( u ) .gt. 0 ) then 
!           para todos los segmentos de la curva de arranque
            do s = 1, NmBloArrURC( u )
!               coeficiente de segmento de arranque
                objMILP ( ka + s + (i-1)*NmBloArrURC ( u ) ) = CostoArrRCS ( u, s )
                if ( CostoArrRCS ( u, s ) .gt. PrecioTopeArr ) then
                    PrecioTopeArr = CostoArrRCS ( u, s )
                endif
            enddo
        else
!           coeficiente de costo unico de arranque
            objMILP ( IARRC + u + (i-1)*NumUniRC -1 ) = CostArrUniURC ( u )
            if ( CostArrUniURC ( u ) .gt. PrecioTopeArr ) then
                PrecioTopeArr = CostArrUniURC ( u )
            endif
        endif
    enddo
    ka = ka + inicio
enddo
    
    
! unidades de rango discontinuo
ka = IBOARD
kv = IGABRD
km = IARD
kd = IGDARD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
        
        do m = 1, NumModRD ( u )
            
!           si existen segmentos de la curva de arranque
            if ( NmBloARRURD ( u, m ) .gt. 0 ) then
!               para todos los segmentos de la curva de arranque
                do s = 1, NmBloARRURD ( u, m )
!                   coeficiente de segmento de arranque
                    objMILP ( ka ) = CostoArrRDS ( u, m, s )
                    ka = ka + 1
                    if ( CostoArrRDS ( u, m, s ) .gt. PrecioTopeArr ) then
                        PrecioTopeArr = CostoArrRDS ( u, m, s )
                    endif
                enddo
            else
!               coeficiente de variable de arranque (costo unico)
                objMILP ( IARRD + INIURDI ( u, i ) + m - 1 ) = CostoTrans ( u, 1, m )
                if ( CostoTrans ( u, 1, m ) .gt. PrecioTopeArr ) then
                    PrecioTopeArr = CostoTrans ( u, 1, m )
                endif
            endif

            if ( CostoMinGRD ( u, m, i ) .gt. PrecioTopeEM ) then
                PrecioTopeEM = CostoMinGRD ( u, m, i )
            endif

!           si son ofertas de costo a generacion minima
		    if ( TipoOferta .eq. 1 ) then
!               coeficiente de asignacion en operacion
                objMILP ( km ) = CostoMinGRD ( u, m, i )
                km = km + 1
                if ( PotMinGRD ( u, m, i ) .gt. 0.0 ) then
!                   coeficiente de asignacion en sincronizacion
                    objMILP ( kd ) = CostoMinGRD ( u, m, i )/PotMinGRD ( u, m, i )
                endif
                kd = kd + 1
!               para todos los segmentos de curva de ofertas de venta
                do s = 1, NumBloVRD ( u, m, i )
!                   coeficiente de segmento de venta
                    objMILP ( kv ) = PreVenEnerRD (u, m, s, i )
                    kv = kv + 1
                    if ( PreVenEnerRD (u, m, s, i ) .gt. PrecioTopeEner ) then
                        PrecioTopeEner = PreVenEnerRD (u, m, s, i )
                    endif
                enddo
!           si son ofertas de costo en vacio
		    else
!               coeficiente de asignacion en operacion
                objMILP ( km ) = CostoMinGRD ( u, m, i ) + PotMinGRD ( u, m, i )*PreVenEnerRD (u, m, 1, i )
                km = km + 1
!               coeficiente de asignacion durante arranque
                objMILP ( kd ) = CostoMinGRD ( u, m, i )
                kd = kd + 1
!               costo de primer segmento (generacion de sincronizacion)
                objMILP ( IGDARD + INIURDI ( u, i ) + m - 1 ) = PreVenEnerRD (u, m, 1, i )
                if ( NumBloVRD ( u, m, i ) .gt. 0 ) then
                    kv = kv + 1
                    if ( PreVenEnerRD (u, m, 1, i ) .gt. PrecioTopeEner ) then
                        PrecioTopeEner = PreVenEnerRD (u, m, 1, i )
                    endif
                endif
!               para los siguientes segmentos de curva de ofertas de venta
                do s = 2, NumBloVRD ( u, m, i )
!                   coeficiente de segmento de venta
                    objMILP ( kv ) = PreVenEnerRD (u, m, s, i )
                    kv = kv + 1
                    if ( PreVenEnerRD (u, m, s, i ) .gt. PrecioTopeEner ) then
                        PrecioTopeEner = PreVenEnerRD (u, m, s, i )
                    endif
                enddo
            endif
        enddo
        
!       para todos los modos excepto el modo en paro
        do modoO = 2, NumModRD ( u )
!           para todos los modos de operacion a pasar desde el origen
            do modoD = 1, NumModRD(u)
!               si la transicion es factible y hacia un modo distinto
                if ( TransFacti ( u, modoO, modoD ) .gt. 0 .and. modoO .ne. modoD ) then
                    objMILP ( IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 ) = CostoTrans ( u, modoO, modoD )
                endif
            enddo
        enddo
        
!       para todos los modos 
        do modoO = 1, NumModRD ( u )
!           para todos los modos de operacion en la diagonal de la matriz de transicion
            do modoD = 1, NumModRD(u)
!               si modo origen y destino son el mismo
                if ( modoO .eq. modoD ) then
                    ubMILP ( IOMARD + INALPHAT ( u, i ) + (modoO-1)*NumModRD ( u ) + modoD - 1 ) = 0.0
                endif
            enddo
        enddo

    enddo
enddo

! unidades hidro
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades 
    do u = 1 , NumUniHid
!       coeficiente de generacion
        objMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = CostoOporUH(u,i)
!       coeficiente de arranque
!        objMILP ( IARH + u + (i-1)*NumUniHid - 1 ) = 1.25e4
        if ( CostoOporUH(u,i) .gt. PrecioTopeEner ) then
            PrecioTopeEner = CostoOporUH(u,i)
        endif
    enddo
enddo

! unidades renovables
kv = IGABRE
do u = 1 , NumUniRE
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los segmentos de curva de ofertas de venta
        do s = 1, NumBloVRE( u, i )
!           coeficiente de segmento de venta
            objMILP ( kv ) = PreVenEnerRE(u,s,i)
            kv = kv + 1
            if ( PreVenEnerRE(u,s,i) .gt. PrecioTopeEner ) then
                PrecioTopeEner = PreVenEnerRE(u,s,i)
            endif
        enddo
    enddo
enddo

! demandas
kd = IDBC
do d = 1 , NumOferDem
!   Para todos los intervalos
    do i = 1 , NTINTR
!       para todos los segmentos de curva de ofertas de compra
        do s = 1, NumBloDem( d, i )
!           coeficiente de segmento de compra
            objMILP ( kd ) = -PreComEner(d,s,i)
            kd = kd + 1
        enddo
    enddo
enddo

if ( nomsis(1) .eq. 'SIN') then
    Factor = 10.0
else
    Factor = 1.0
endif
if ( SiCalPen .eq. 1 ) then
    ! Precio del corte de carga
    CostoCorte = (PrecioTopeEner+PrecioTopeEM+PrecioTopeArr)*30.0

    ! Costo por no satisfacer una politica de energia Hidro
    PenEnerEmb = CostoCorte*0.9

    ! Costo por no satisfacer una politica de energia Termo
    PenEnerUTerm = CostoCorte*0.9

    ! Costo por violar restricciones de transmision
    PenRamas = CostoCorte*1.03

    ! Precio de infactibilidad en reserva de regulacion
    PreResReg = CostoCorte*(Factor + 0.5)

    ! Precio de infactibilidad en reserva rodante por zona
    PreResRR10 = CostoCorte*(Factor + 0.4)

    ! Precio de infactibilidad en reserva operativa por zona
    PreResR10 = CostoCorte*(Factor + 0.3)

    ! Precio de infactibilidad en reserva suplementaria por zona
    PreResSup = CostoCorte*(Factor + 0.2)
endif

return
end

    
subroutine FunObj2MILP (sistema )
! ---------------------------------------------------------------------
! Se forma la segunda parte (servicios conexos) de la funcion         *
! objetivo del problema de asignacion (MILP)                          *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2017                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, d, k, kr10, k10, ks, kr, m, r, s, u, sistema
real fac1, fac2, fac3, fac4, delta

delta = 0.01
          
if ( TipoEscPre .ne. 0 ) then
!   Precio Tope de Precio de Escasez de Reserva de Regulacion (Oferta mas cara)
    PrecioTopeReg = 0.0
!   Precio Tope de Precio de Escasez de Reserva Rodante (Oferta mas cara)
    PrecioTopeRod = 0.0
!   Precio Tope de Precio de Escasez de Reserva Operativa (Oferta mas cara)
    PrecioTopeOper = 0.0
!   Precio Tope de Precio de Escasez de Reserva Suplementaria (Oferta mas cara)
    PrecioTopeSup = 0.0
endif


! Ofertas de venta de reserva

! unidades de rango continuo
do u = 1 , NumUniRC*SiOferRes
! Para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de reserva rodante de 10 minutos
        objMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) = PreVenResR10RC(u,i)
        if ( PreVenResR10RC(u,i) .gt. PrecioTopeRod .and. TipoEscPre .ne. 0 ) then
            PrecioTopeRod = PreVenResR10RC(u,i)
        endif
!       coeficiente de reserva no rodante de 10 minutos
        objMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) = PreVenResNR10RC(u,i)
        if ( PreVenResNR10RC(u,i) .gt. PrecioTopeOper .and. TipoEscPre .ne. 0 ) then
            PrecioTopeOper = PreVenResNR10RC(u,i)
        endif
!       coeficiente de reserva rodante suplementaria
        objMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) = PreVenResRxRC(u,i)
        if ( PreVenResRxRC(u,i) .gt. PrecioTopeSup .and. TipoEscPre .ne. 0 ) then
            PrecioTopeSup = PreVenResRxRC(u,i)
        endif
!       coeficiente de reserva no rodante suplementaria
        objMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 ) = PreVenResNRxRC(u,i)
        if ( PreVenResNRxRC(u,i) .gt. PrecioTopeSup .and. TipoEscPre .ne. 0 ) then
            PrecioTopeSup = PreVenResNRxRC(u,i)
        endif
!       coeficiente de reserva de regulacion secundaria
        objMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) = PreVenResRegRC(u,i)
        if ( PreVenResRegRC(u,i) .gt. PrecioTopeReg .and. TipoEscPre .ne. 0 ) then
            PrecioTopeReg = PreVenResRegRC(u,i)
        endif
    enddo
enddo

! unidades de rango discontinuo
k = 1
do u = 1 , NumUniRD*SiOferRes
! Para todos los intervalos
    do i = 1 , NTINTR
    !   para todos los modos de la unidad
        do m = 1, NumModRD(u)
    !       coeficiente de reserva rodante de 10 minutos
            objMILP ( IRR10RD + k - 1) = PreVenResR10RD(u,m,i)
            if ( PreVenResR10RD(u,m,i) .gt. PrecioTopeRod .and. TipoEscPre .ne. 0 ) then
                PrecioTopeRod = PreVenResR10RD(u,m,i)
            endif
    !       coeficiente de reserva no rodante de 10 minutos
            objMILP ( IRNR10RD + k - 1 ) = PreVenResNR10RD(u,m,i)
            if ( PreVenResNR10RD(u,m,i) .gt. PrecioTopeOper .and. TipoEscPre .ne. 0 ) then
                PrecioTopeOper = PreVenResNR10RD(u,m,i)
            endif
    !       coeficiente de reserva rodante suplementaria
            objMILP ( IRRSRD + k - 1 ) = PreVenResRxRD(u,m,i)
            if ( PreVenResRxRD(u,m,i) .gt. PrecioTopeSup .and. TipoEscPre .ne. 0 ) then
                PrecioTopeSup = PreVenResRxRD(u,m,i)
            endif
    !       coeficiente de reserva no rodante suplementaria
            objMILP ( IRNRSRD + k - 1 ) = PreVenResNRxRD(u,m,i)
            if ( PreVenResNRxRD(u,m,i) .gt. PrecioTopeSup .and. TipoEscPre .ne. 0 ) then
                PrecioTopeSup = PreVenResNRxRD(u,m,i)
            endif
    !       coeficiente de reserva de regulacion secundaria
            objMILP ( IRRERD + k - 1 ) = PreVenResRegRD(u,m,i)
            if ( PreVenResRegRD(u,m,i) .gt. PrecioTopeReg .and. TipoEscPre .ne. 0 ) then
                PrecioTopeReg = PreVenResRegRD(u,m,i)
            endif
            k = k + 1
        enddo
    enddo
enddo

! unidades hidro
! Para todos los intervalos
do i = 1 , NTINTR*SiOferRes
!   para todas las unidades 
    do u = 1 , NumUniHid
!       coeficiente de reserva rodante de 10 minutos
        objMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) = PreVenResR10H(u,i)
        if ( PreVenResR10H(u,i) .gt. PrecioTopeRod .and. TipoEscPre .ne. 0 ) then
            PrecioTopeRod = PreVenResR10H(u,i)
        endif
!       coeficiente de reserva no rodante de 10 minutos
        objMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) = PreVenResNR10H(u,i)
        if ( PreVenResNR10H(u,i) .gt. PrecioTopeOper .and. TipoEscPre .ne. 0 ) then
            PrecioTopeOper = PreVenResNR10H(u,i)
        endif
!       coeficiente de reserva rodante suplementaria
        objMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) = PreVenResRxH(u,i)
        if ( PreVenResRxH(u,i) .gt. PrecioTopeSup .and. TipoEscPre .ne. 0 ) then
            PrecioTopeSup = PreVenResRxH(u,i)
        endif
!       coeficiente de reserva no rodante suplementaria
        objMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) = PreVenResNRxH(u,i)
        if ( PreVenResNRxH(u,i) .gt. PrecioTopeSup .and. TipoEscPre .ne. 0 ) then
            PrecioTopeSup = PreVenResNRxH(u,i)
        endif
!       coeficiente de reserva de regulacion secundaria
        objMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) = PreVenResRegH(u,i)
        if ( PreVenResRegH(u,i) .gt. PrecioTopeReg .and. TipoEscPre .ne. 0 ) then
            PrecioTopeReg = PreVenResRegH(u,i)
        endif
    enddo
enddo

! si no existen ofertas de demanda
if ( SiOferRes .eq. 0 ) then

!   unidades de rango continuo
    do u = 1 , NumUniRC
!       Para todos los intervalos
        do i = 1 , NTINTR
!           cota superior de reserva rodante de 10 minutos
            ubMILP ( IRR10RC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           cota superior de reserva no rodante de 10 minutos
            ubMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           cota superior de reserva rodante suplementaria
            ubMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           cota superior de reserva no rodante suplementaria
            ubMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           cota superior de reserva de regulacion secundaria
            ubMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) = 0.0
        enddo
    enddo

!   unidades de rango discontinuo
    k = 1
    do u = 1 , NumUniRD
!       Para todos los intervalos
        do i = 1 , NTINTR
!           para todos los modos de la unidad
            do m = 1, NumModRD(u)
!               cota superior de reserva rodante de 10 minutos
                ubMILP ( IRR10RD + k - 1) = 0.0
!               cota superior de reserva no rodante de 10 minutos
                ubMILP ( IRNR10RD + k - 1 ) = 0.0
!               cota superior de reserva rodante suplementaria
                ubMILP ( IRRSRD + k - 1 ) = 0.0
!               cota superior de reserva no rodante suplementaria
                ubMILP ( IRNRSRD + k - 1 ) = 0.0
!               cota superior de reserva de regulacion secundaria
                ubMILP ( IRRERD + k - 1 ) = 0.0
                k = k + 1
            enddo
        enddo
    enddo

! unidades hidro
! Para todos los intervalos
    do i = 1 , NTINTR
!       para todas las unidades 
        do u = 1 , NumUniHid
!           cota superior de reserva rodante de 10 minutos
            ubMILP ( IRR10H + u + (i-1)*NumUniHid - 1 ) = 0.0
!           cota superior de reserva no rodante de 10 minutos
            ubMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) = 0.0
!           cota superior de reserva rodante suplementaria
            ubMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) = 0.0
!           cota superior de reserva no rodante suplementaria
            ubMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) = 0.0
!           cota superior de reserva de regulacion secundaria
            ubMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) = 0.0
        enddo
    enddo

endif

! demandas controlables

! para todas las demandas
do d = 1 , NumOferDem*SiOferDemCon*0
!   para todos los intervalos
    do i = 1 , NTINTR
!       coeficiente de oferta de reserva de 10 minutos
        objMILP ( ICC10 + d + (i-1)*NumOferDem - 1 ) = PreOferRes10(d,i)
!       coeficiente de oferta de reserva suplementaria
        objMILP ( ICCS + d + (i-1)*NumOferDem - 1 ) = PreOferResS(d,i)
    enddo
enddo

! Ofertas de compra de reserva del CENACE por zona

! para los grupos de reserva
kr10 = ICARR10G
k10 = ICAR10G
ks = ICARSG
kr = ICARRG
do r = 1, NumGruRes*SiOferComResZona
!   para todos los intervalos
    do i = 1, NTINTR
!       para los requerimientos de reserva rodante de 10 minutos
        fac1 = 1.0
        do s = 1, NumBloRR10( i )
!           coeficiente en la funcion objetivo de un segmento de la curva
            if ( TipoEscPre .eq. 0 ) then
            	objMILP ( kr10 ) = -PrecioTopeRod*fac1
			else
            	objMILP ( kr10 ) = -PreResRR10 (r, s, i)*fac1
			endif
            ubMILP ( kr10 ) = ReqResR10 (r, s, i)
            kr10 = kr10 + 1
            fac1 = fac1 -delta
        enddo
!       para los requerimientos de reserva de 10 minutos
        fac2 = 1.0
        do s = 1, NumBloR10( i )
!           coeficiente en la funcion objetivo de un segmento de la curva
            if ( TipoEscPre .eq. 0 ) then
            	objMILP ( k10 ) = -PrecioTopeOper*fac2
			else
            	objMILP ( k10 ) = -PreResR10 (r, s, i)*fac2
            endif
!           si no es el EXPOST
            if ( TipoEjecu .le. 3 ) then
                ubMILP ( k10 ) = ReqRes10 (r, s, i)
            endif
            k10 = k10 + 1
            fac2 = fac2 -delta
        enddo
!       para los requerimientos de reserva suplementaria
        fac3 = 1.0
        do s = 1, NumBloRSu( i )
!           coeficiente en la funcion objetivo de un segmento de la curva
            if ( TipoEscPre .eq. 0 ) then
            	objMILP ( ks ) = -PrecioTopeSup*fac3
			else
            	objMILP ( ks ) = -PreResSup (r, s, i)*fac3
			endif
            ubMILP ( ks ) = ReqResSup (r, s, i)
            ks = ks + 1
            fac3 = fac3 -delta
        enddo
!       para los requerimientos de reserva de regulacion secundaria
        fac4 = 1.0
        do s = 1, NumBloRReg( i )
!           coeficiente en la funcion objetivo de un segmento de la curva
            if ( TipoEscPre .eq. 0 ) then
            	objMILP ( kr ) = -PrecioTopeReg*fac4
			else
            	objMILP ( kr ) = -PreResReg (r, s, i)*fac4
			endif
            ubMILP ( kr ) = ReqResReg (r, s, i)
            kr = kr + 1
            fac4 = fac4 -delta
        enddo
    enddo
enddo

! para el sistema
kr10 = ICARR10S
k10 = ICAR10S
ks = ICARSS
kr = ICARRS
! para todos los intervalos
do i = 1, NTINTR*SiOferComResSis
    
!   para los requerimientos de reserva rodante de 10 minutos
    fac1 = 1.0
    do s = 1, NumBloRR10( i )
!       coeficiente en la funcion objetivo de un segmento de la curva
        if ( TipoEscPre .eq. 0 ) then
            objMILP ( kr10 ) = -PrecioTopeRod*fac1
		else
            objMILP ( kr10 ) = -PreResRR10S (sistema, s, i)*fac1
		endif
        kr10 = kr10 + 1
        fac1 = fac1 -delta
    enddo
!   para los requerimientos de reserva de 10 minutos
    fac2 = 1.0
    do s = 1, NumBloR10( i )
!       coeficiente en la funcion objetivo de un segmento de la curva
        if ( TipoEscPre .eq. 0 ) then
            objMILP ( k10 ) = -PrecioTopeOper*fac2
		else
            objMILP ( k10 ) = -PreResR10S (sistema, s, i)*fac2
		endif
        k10 = k10 + 1
        fac2 = fac2 -delta
    enddo
!   para los requerimientos de reserva suplementaria
    fac3 = 1.0
    do s = 1, NumBloRSu( i )
!       coeficiente en la funcion objetivo de un segmento de la curva
        if ( TipoEscPre .eq. 0 ) then
            objMILP ( ks ) = -PrecioTopeSup*fac3
		else
            objMILP ( ks ) = -PreResSupS (sistema, s, i)*fac3
		endif
        ks = ks + 1
        fac3 = fac3 -delta
    enddo
!   para los requerimientos de reserva de regulacion secundaria
    fac4 = 1.0
    do s = 1, NumBloRReg( i )
!       coeficiente en la funcion objetivo de un segmento de la curva
        if ( TipoEscPre .eq. 0 ) then
            objMILP ( kr ) = -PrecioTopeReg*fac4
		else
            objMILP ( kr ) = -PreResRegS (sistema, s, i)*fac4
		endif
        kr = kr + 1
        fac4 = fac4 -delta
    enddo
enddo

return
end

    
    
subroutine FunObj3MILP (sistema )
! ---------------------------------------------------------------------
! Se forma la tercera parte (penalizaciones) de la funcion            *
! objetivo del problema de asignacion (MILP)                          *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2019                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only: VolMxEmb
use ParGloRed, only: NumGruRamSis, NumNodSis, tipnod

implicit none

Integer i, n, br, e, o, u, d, sistema
real*8 limitesup

limitesup = 10000.0/Base


! Para todos los intervalos
do i = 1 , NTINTR
!   para todos los nodos
    do n = 1, NumNodSis ( sistema )
!       cota inferior de la variable artificial de excedente
        lbMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) = 0.0
!       coeficiente de variable artificial de excedente
        objMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) = CostoExced*SiExced
!       cota superior de la variable artificial de excedente
        ubMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) = 0.0
!       solo para nodos con generacion
        if ( ( tipnod (n) .eq. 1 .or. tipnod (n) .eq. 3 .or. tipnod (n) .eq. 4 ) .and. kvbase (n) .gt. 1.0 ) then
            ubMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) = 10.0*SiExced/Base
        endif
    enddo
!   para todas las ramas restringidas
    do br = 1, NumGruRamSis (sistema)
!       coeficiente de variable artificial de excedente de flujo
        objMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 ) = PenRamas*SiArtTrans
!       cota superior de la variable artificial de excedente
        ubMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 ) = limitesup*SiArtTrans
!       coeficiente de variable artificial de excedente de contraflujo
        objMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 ) = PenRamas*SiArtTrans
!       cota superior de la variable artificial de excedente de contraflujo
        ubMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 ) = limitesup*SiArtTrans
    enddo
enddo

! Penalizacion por corte de carga
do i = 1, NTINTR
    do d = 1, NumOferDem
!       coeficiente de variable artificial de corte
        objMILP ( IDF + d + (i-1)*NumOferDem - 1 ) = CostoCorte*SiCorte
!       cota superior de la variable artificial de corte
        ubMILP (  IDF + d + (i-1)*NumOferDem - 1 ) = PorcenCorte*DemFija(d,i)*SiCorte
    enddo
enddo

! produccion de energia para grupos de energia termica
do  o = 1 , NumGruUTer*SiEnerTer
!   para todos los dias
    do d = 1, DURDIA
!       coeficiente de variable artificial para cumpir la energia a producir
        objMILP ( IARGT + NumGruUTer*(d-1) + o - 1) = PenEnerUTerm
    enddo
enddo

! produccion de energia para grupos de energia termica
do  o = 1 , NResEner*SiEnerTer*0
!   coeficiente de variable artificial para cumpir la energia a producir
    objMILP ( IARGT + o - 1) = PenEnerUTerm
enddo

! consumo de combustible
do  o = 1 , NumGruGas*SiGpoGas
!   para todos los dias
    do d = 1, DURDIA
!       coeficiente de variable artificial para cumpir el consumo de combustible
        objMILP ( IARCG + NumGruGas*(d-1) + o - 1 ) = PenEnerUTerm
    enddo
enddo

! consumo de combustible
do  o = 1 , NResComb*SiGpoGas*0
!   coeficiente de variable artificial para cumpir el consumo de combustible
    objMILP ( IARCG + o - 1 ) = PenLimComb
enddo

! produccion de energia por embalse
do  e = 1 , NumEmbalses*SiEnerHid
!    coeficiente de variable artificial para cumpir la produccion de energia
     objMILP ( IAREE + e - 1) = PenEnerEmb
enddo

! para todos los embalses
do e = 1, NumEmbalses*SiModHid
!  para todos los intervalos
   do i = 1 , NTINTR
!     cotas variable artificial de deficit en volumen
      ubMILP ( IDBAL + e + (i-1)*NumEmbalses - 1 ) = VolMxEmb ( e )
      lbMILP ( IDBAL + e + (i-1)*NumEmbalses - 1 ) = 0.0
!     penalizacion en la funcion objetivo
      objMILP ( IDBAL + e + (i-1)*NumEmbalses - 1 ) = 1.2*CostoCorte
!     cotas variable artificial de excedente en volumen
      ubMILP ( IEBAL + e + (i-1)*NumEmbalses - 1 ) = VolMxEmb ( e )
      lbMILP ( IEBAL + e + (i-1)*NumEmbalses - 1 ) = 0.0
!     penalizacion en la funcion objetivo
      objMILP ( IEBAL + e + (i-1)*NumEmbalses - 1 ) = 1.2*CostoCorte
   enddo
end do

! si se permite hacer uso de reserva
if ( TipoEjecu .le. 1 ) then
!   unidades de rango continuo
    do u = 1 , NumUniRC
!       Para todos los intervalos
        do i = 1 , NTINTR
!           penalizacion por uso de reserva rodante de 10 minutos
            objMILP ( IREUSO10 + u + (i-1)*NumUniRC - 1 ) = PenRelRod
!           penalizacion por uso de reserva de regulacion
            objMILP ( IREUSORE + u + (i-1)*NumUniRC - 1 ) = PenRelReg
        enddo
    enddo
!   unidades hidro
    do u = 1 , NumUniHid
!       Para todos los intervalos
        do i = 1 , NTINTR
!           penalizacion por uso de reserva de regulacion
            objMILP ( IREUSOREH + u + (i-1)*NumUniHid - 1 ) = PenRelReg
        enddo
    enddo
endif


return
end
    

subroutine TipoLimUni
! ---------------------------------------------------------------------
! Se definen tipos de variables y cotas smples en unidades de         *
! generacion.                                                         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo de 2018                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, kao, kaa, kar, ksm, m, u, m1, ro

! unidades de rango continuo
do u = 1 , NumUniRC
! Para todos los intervalos
    do i = 1 , NTINTR
!       variable de asignacion para operacion
!       cota inferior de la variable
        lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 1
!       variable de asignacion para proceso de arranque
!       cota inferior de la variable
        lbMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 1
!       variable de arranque
!       cota inferior de la variable
        lbMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) = 1
!       variable de paro
!       cota inferior de la variable
        lbMILP ( IPRC + u + (i-1)*NumUniRC - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IPRC + u + (i-1)*NumUniRC - 1 ) = 1
!       si la solucion es en variables enteras
		if ( TipoProblema .eq. 1 ) then
!       	variable de asignacion para operacion
        	ctypeMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = 'B'
!       	variable de asignacion para proceso de arranque
        	ctypeMILP ( IADARC + u + (i-1)*NumUniRC - 1 ) = 'B'
!       	variable de arranque
        	ctypeMILP ( IARRC + u + (i-1)*NumUniRC - 1 ) = 'B'
!       	variable de paro
        	ctypeMILP ( IPRC + u + (i-1)*NumUniRC - 1 ) = 'B'
		endif
!       si es una unidad de importacion
        if ( ImpEnt ( U ) .eq. 1 ) then
!           variable de generacion para operacion
            ctypeMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = 'I'
!           cota inferior de la variable
            lbMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = 0.0
!           cota superior de la variable
            ubMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = PotMaxGRC ( u, i )
        endif
!       si se desea considerar bandas prohibidas
        if ( SiBandProh .eq. 1 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpRC ( u )
!               variable de asignacion de rango operativo
!       		si la solucion es en variables enteras
				if ( TipoProblema .eq. 1 ) then
                	ctypeMILP ( IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2 ) = 'B'
				endif
!               cota inferior de la variable
                lbMILP ( IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2 ) = 0
!               cota superior de la variable
                ubMILP ( IABPRC + INVBPRC ( u ) + (i-1)*NoRaOpRC ( u ) + ro - 2 ) = 1
            enddo
        endif
    enddo
enddo

do i = IVREGRC, IGRD - 1
!   variable de asignacion de reserva de regulacion
!   si la solucion es en variables enteras
	if ( TipoProblema .eq. 1 ) then
    	ctypeMILP ( i ) = 'B'
	endif
!   cota inferior de la variable de asignacion de reserva de regulacion
    lbMILP ( i ) = 0
!   cota superior de la variable de asignacion de reserva de regulacion
    ubMILP ( i ) = 1
enddo
! unidades de rango discontinuo
kao = IARD
kaa = IADARD
kar = IARRD
ksm = IOMARD
do u = 1 , NumUniRD
! Para todos los intervalos
    do i = 1 , NTINTR
        do m = 1, NumModRD(u)
!           variable de asignacion para operacion
!           cota inferior de la variable
            lbMILP ( kao ) = 0
!           cota superior de la variable
            ubMILP ( kao ) = 1
!           variable de asignacion para proceso de arranque
!           cota inferior de la variable
            lbMILP ( kaa ) = 0
!           cota superior de la variable
            ubMILP ( kaa ) = 1
!           variable de arranque
!           cota inferior de la variable
            lbMILP ( kar ) = 0
!           cota superior de la variable
            ubMILP ( kar ) = 1
!       	si la solucion es en variables enteras
			if ( TipoProblema .eq. 1 ) then
!           	variable de asignacion para operacion
            	ctypeMILP ( kao ) = 'B'            
!           	variable de asignacion para proceso de arranque
            	ctypeMILP ( kaa ) = 'B'
!           	variable de arranque
            	ctypeMILP ( kar ) = 'B'
			endif
			do m1 = 1, NumModRD(u)
    !           variable de inicio de operacion de modo
!       		si la solucion es en variables enteras
				if ( TipoProblema .eq. 1 ) then
                	ctypeMILP ( ksm + m1 ) = 'B'
				endif
    !           cota inferior de la variable
                lbMILP ( ksm + m1 ) = 0
    !           cota superior de la variable
                ubMILP ( ksm + m1 ) = 1
            enddo
            kao = kao + 1
            kaa = kaa + 1
            kar = kar + 1
            ksm = ksm + NumModRD(u)
        enddo     
!       variable de paro
!       si la solucion es en variables enteras
		if ( TipoProblema .eq. 1 ) then
        	ctypeMILP ( IPRD + u + (i-1)*NumUniRD - 1) = 'B'
		endif
!       cota inferior de la variable
        lbMILP ( IPRD + u + (i-1)*NumUniRD - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IPRD + u + (i-1)*NumUniRD - 1 ) = 1
    enddo
enddo

do i = IVREGRD, IGH - 1
!   variable de asignacion de reserva de regulacion
!   si la solucion es en variables enteras
	if ( TipoProblema .eq. 1 ) then
    	ctypeMILP ( i ) = 'B'
	endif
!   cota inferior de la variable de asignacion de reserva de regulacion
    lbMILP ( i ) = 0
!   cota superior de la variable de asignacion de reserva de regulacion
    ubMILP ( i ) = 1
enddo
! unidades hidro
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades 
    do u = 1 , NumUniHid
!       variable de asignacion
!       cota inferior de la variable
        lbMILP ( IAH + u + (i-1)*NumUniHid - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IAH + u + (i-1)*NumUniHid - 1 ) = 1
!       variable de arranque
!       cota inferior de la variable
        lbMILP ( IARH + u + (i-1)*NumUniHid - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IARH + u + (i-1)*NumUniHid - 1 ) = 1
!       variable de paro
!       cota inferior de la variable
        lbMILP ( IPH + u + (i-1)*NumUniHid - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IPH + u + (i-1)*NumUniHid - 1 ) = 1
!       si la solucion es en variables enteras
		if ( TipoProblema .eq. 1 ) then
!       	variable de asignacion
        	ctypeMILP ( IAH + u + (i-1)*NumUniHid - 1 ) = 'B'
!       	variable de arranque
        	ctypeMILP ( IARH + u + (i-1)*NumUniHid - 1 ) = 'B'
!       	variable de paro
        	ctypeMILP ( IPH + u + (i-1)*NumUniHid - 1 ) = 'B'
		endif
!       si se desea considerar bandas prohibidas
        if ( SiBandProh .eq. 1 ) then
!           para los rangos de la unidad
            do ro = 1, NoRaOpH ( u )
!               variable de asignacion de rango operativo
!       		si la solucion es en variables enteras
				if ( TipoProblema .eq. 1 ) then
                	ctypeMILP ( IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2 ) = 'B'
				endif
!               cota inferior de la variable
                lbMILP ( IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2 ) = 0
!               cota superior de la variable
                ubMILP ( IABPH + INVBPH ( u ) + (i-1)*NoRaOpH ( u ) + ro - 2 ) = 1
            enddo
        endif
    enddo
enddo

do i = IVREGH, IGRE - 1
!   variable de asignacion de reserva de regulacion
!   si la solucion es en variables enteras
	if ( TipoProblema .eq. 1 ) then
    	ctypeMILP ( i ) = 'B'
	endif
!   cota inferior de la variable de asignacion de reserva de regulacion
    lbMILP ( i ) = 0
!   cota superior de la variable de asignacion de reserva de regulacion
    ubMILP ( i ) = 1
enddo
! unidades renovables
do u = 1 , NumUniRE
! Para todos los intervalos
    do i = 1 , NTINTR
!       variable de asignacion
!       si la solucion es en variables enteras
		if ( TipoProblema .eq. 1 ) then
        	ctypeMILP ( IARE + u + (i-1)*NumUniRE - 1 ) = 'B'
		endif
!       cota inferior de la variable
        lbMILP ( IARE + u + (i-1)*NumUniRE - 1 ) = 0
!       cota superior de la variable
        ubMILP ( IARE + u + (i-1)*NumUniRE - 1 ) = 1
    enddo
enddo

return
end    
     
    
subroutine NivDemanda ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de nivel de demanda                         *
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

Integer k, kd, m, i, d, s
character*3 leti

write ( 777,* ) 'Inicia restricciones sobre las reservas de 10 minutos y suplement:', m + 1

kd = IDBC
do d = 1 , NumOferDem
! Para todos los intervalos
    do i = 1 , NTINTR
        if ( NumBloDem( d, i ) .gt. 0 ) then
!           para todos los segmentos de curva de ofertas de compra
            do s = 1, NumBloDem( d, i )
!               coeficiente de demanda en el bloque
                aaMILP ( k ) = 1.0
!               columna asociada
                jcolMILP( k ) = kd
                k = k + 1
                kd = kd + 1
            enddo
!           coeficiente de oferta de reserva de 10 minutos
!           aaMILP ( k ) = -1.0
!           columna asociada
!           jcolMILP( k ) = ICC10 + d + (i-1)*NumOferDem - 1
!           k = k + 1
!           coeficiente de oferta de reserva suplementaria
!           aaMILP ( k ) = -1.0
!           columna asociada
!           jcolMILP( k ) = ICCS + d + (i-1)*NumOferDem - 1
!           k = k + 1
            m = m + 1
!           lado derecho de la restriccion
            bMILP ( m ) = 0.0
!           sentido de la restriccion
            sMILP ( m ) = 'G'
!           inicio de la siguiente restriccion
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779, * ) m, ',', '"Reservas de 10 minutos de carga '//trim(nombcar(d))//' intervalo: '//trim(leti)//'"'
        endif
    enddo
enddo

! cotas simples de ofertas de demandas
kd = IDBC
do d = 1 , NumOferDem
!   Para todos los intervalos
    do i = 1 , NTINTR
        if ( NumBloDem( d, i ) .gt. 0 ) then
!           para todos los segmentos de curva de ofertas de compra
            do s = 1, NumBloDem( d, i )
!               cota inferior del segmento de oferta de demanda
                lbMILP ( kd ) = 0.0
!               cota superior del segmento de oferta de demanda
                ubMILP ( kd ) = OferComDem ( d, s, i )
!               si se trata de una exportacion
                if ( ExpEnt ( d ) .eq. 1 ) then
                    ctypeMILP ( kd ) = 'I'
                endif
                kd = kd + 1
            enddo
!           cota inferior de oferta de reserva de 10 minutos
!           lbMILP ( ICC10 + d + (i-1)*NumOferDem - 1 ) = 0.0
!           cota inferior de oferta de reserva suplementaria
!           lbMILP ( ICCS + d + (i-1)*NumOferDem - 1 ) = 0.0
!           cota superior de oferta de reserva de 10 minutos
!           ubMILP ( ICC10 + d + (i-1)*NumOferDem - 1 ) = OferRes10(d,i)*SiOferDemCon
!           cota superior de oferta de reserva suplementaria
!           ubMILP ( ICCS + d + (i-1)*NumOferDem - 1 ) = OferResS(d,i)*SiOferDemCon
        endif
    enddo
enddo

200 format (i3)
    
return
end

    
subroutine BalPot ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de balance de potencia horario del MILP.    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2018                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumNodSis

implicit none

Integer i, isla, d, k, l, m, modo, n, u, s, sistema
real*8  TotNPR, TotInter, demanda
character*3 leti, lets

sistema = 0
write ( 777,* ) 'Inicia restricciones de balance de potencia horario              :', m + 1

IRBAL = m + 1
! para todos los subsistemas (islas)
do l = 1, numsis
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
        sistema = sistema + 1
!       para todos los intervalos       
        do i = 1 , NTINTR
!           total de intercambios
            TotInter = 0.0
!           total de generacion no programable
            TotNPR = 0.0
!           total de demanda fija
            demanda = 0.0
!           coeficientes de variables de generacion de rango continuo
            do u = 1 , NumUniRC
                isla = IslaGenRC ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de generacion
                    aaMILP ( k ) = 1.0
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
!                       coeficiente de generacion
                        aaMILP ( k ) = 1.0 
                        jcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    enddo
                endif
            enddo
!           coeficientes de variables de generacion hidro
            do u = 1 , NumUniHid
                isla = IslaGenH ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de generacion
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
            enddo
!           coeficientes de variables de generacion renovable
            do u = 1 , NumUniRE
                isla = IslaGenRE ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de generacion
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 1
                    k = k + 1
                endif
            enddo
            m = m + 1
!           coeficientes de variables de nivel de demanda y corte de carga
            do d = 1 , NumOferDem
                isla = IslaDem ( d )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   para todos los segmentos de curva de ofertas de compra
                    do s = 1, NumBloDem( d, i )
!                       coeficiente de demanda en el bloque
                        aaMILP ( k ) = -1.0
!                       columna asociada
                        jcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 1
                        k = k + 1
                    enddo
!                   coeficiente de corte de demanda fija
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 1
                    k = k + 1
                    demanda = demanda + DemFija ( d, i )
                endif
            enddo
!           para todos los nodos
            do n = 1, NumNodSis ( l )
!               coeficiente de variable de excedente
                aaMILP ( k ) = -1.0 
                jcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 1
                k = k + 1
            enddo
!           Total de generacion no programable
!           para todas las unidades no programables
            do u = 1 , NumUniNPR
	            isla = IslaGenNPR ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
                    TotNPR = TotNPR + PotNPR ( u, i )
                endif
            enddo
!           Total de intercambios
!           para todos los intercambios
            do u = 1 , NumNodInt
	            isla = IslaNodInt ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
                    TotInter = TotInter + PotNodInt ( u, i )
                endif
            enddo
!           variable de perdidas
!           coeficiente de la variable
            aaMILP ( k ) = -1.0 
            jcolMILP ( k ) = IPERD + i + (sistema-1)*numsis_act - 1
            k = k + 1
!           En AUGC no hay estimacion de perdidas
!            if ( TipoEjecu .eq. 1 ) then
!                ubMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = 0.0
!            endif
            objMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = CostoExced + 1.0
            if ( nomsis(sistema) .eq. 'BCA' ) then
                objMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = 0.0
                ubMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = 0.0
            endif
!           lados derechos de las restriciones
            bMILP ( m )   = TotInter - TotNPR + demanda
!           sentidos de las restriciones
            sMILP ( m ) = 'E'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( lets, 200 ) l
            write ( 779,* ) m, ',', '"Balance de energia de subsistema electrico '//trim(lets)//' intervalo: '//trim(leti)//'"'
        enddo        
    endif
enddo

200 format (i3)
    
return
end
 
    
subroutine CreaMILP ( sistema )
! ---------------------------------------------------------------------
! Se crea el problema MILP para el optimizador CPLEX                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2015                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use symtypes

!use cplex_ifaces, only: CPXcreateprob, CPXwriteprob  ,     & !Windows (comentarizar para Linux)
!                        CPXchgobjsen , CPXsetintparam        !Windows (comentarizar para Linux)
!use cplex_forman, only: addrowsCPX, newcolsCPX               !Windows (comentarizar para Linux)

use cplex_cons, only:   CPX_INFBOUND , CPX_MIN

implicit none
 
integer                 CPXcreateprob, CPXwriteprob  ,  & !Linux (comentarizar para Windows)
                        CPXchgobjsen , CPXsetintparam,  & !Linux (comentarizar para Windows)
                        CPXnewcols   , CPXaddrows         !Linux (comentarizar para Windows)

! Define integer constant for NULL
integer(IL), parameter :: NULL = 0

! environment and LP variable pointers
integer status
data status  / 0 /
                   
CHARACTER*9  probname 

CHARACTER fecha_Ej*19

character*1 name_array( maxresMILP )   !Linux (comentarizar para Windows)
integer     name( maxresMILP )         !Linux (comentarizar para Windows)

integer     i, ibanbit, ierror, j, sistema
character*1 ssistema

ibanbit = 1
ierror = 0

! SE RECORREN APUNTADORES AL ESTILO DE LENGUAJE EN C 
do j = 1, irowMILP ( NumResAsig + 1 ) - 1
    jcolMILP ( j ) = jcolMILP ( j ) - 1
enddo
do j = 1, NumResAsig
    irowMILP ( j ) = irowMILP ( j ) - 1
enddo

!  Create the problem
probname = 'MILP'

lpMILP = CPXcreateprob (enb, status, probname)

if ( lpMILP .eq. NULL ) then
    write (*,*) ' Error al crear el MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! Minimization problem
status =  CPXchgobjsen (enb, lpMILP, CPX_MIN)

do i = 1, NumVarAsig                         !Linux (comentarizar para Windows)
   name_array(i) = name_array(i)//char(0)    !Linux (comentarizar para Windows)
   name(i) = loc( name_array(i) )            !Linux (comentarizar para Windows)
end do                                       !Linux (comentarizar para Windows)


! Se agrega informacion de variables
status = CPXnewcols (enb, lpMILP, NumVarAsig, objMILP, lbMILP, ubMILP , ctypeMILP, name)  !Linux (comentarizar para Windows)
! si la solucion es en variables enteras
!if ( TipoProblema .eq. 1 ) then			!Windows (comentarizar para Linux)
!	status = newcolsCPX (enb, lpMILP, NumVarAsig, objMILP, lbMILP, ubMILP , ctypeMILP)       !Windows (comentarizar para Linux) 
!else
!	status = newcolsCPX (enb, lpMILP, NumVarAsig, objMILP, lbMILP, ubMILP )                  !Windows (comentarizar para Linux) 
!endif								!Windows (comentarizar para Linux)
if ( status .ne. 0 ) then
    write (*,*) ' Error al agregar las variables al MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

do i = 1, NumResAsig                        !Linux (comentarizar para Windows)
   name_array(i) = name_array(i)//char(0)   !Linux (comentarizar para Windows)
   name(i) = loc( name_array(i) )           !Linux (comentarizar para Windows)
end do                                      !Linux (comentarizar para Windows)

! Se agrega informacion de restricciones (por renglon)
status = CPXaddrows (enb, lpMILP, 0, NumResAsig, nelemMaMILP, bMILP, sMILP,  &   !Linux (comentarizar para Windows)
                     irowMILP, jcolMILP, aaMILP, name, name )                  !Linux (comentarizar para Windows)
!status = addrowsCPX (enb, lpMILP, 0, NumResAsig, nelemMaMILP, bMILP, sMILP, &  !Windows (comentarizar para Linux)
!                     irowMILP, jcolMILP, aaMILP )                             !Windows (comentarizar para Linux)
if ( status .ne. 0 ) then
    write (*,*) ' Error al agregar las restricciones al MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

Write( ssistema, '(I1)' )  sistema
!Se escribe el modelo MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'mp'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'mp'//ssistema//'.sav', 'SAV')


return
end
    

subroutine ResuelveMILP ( ite, sistema, imprime, itipo )
! ---------------------------------------------------------------------
! Se resuelve el problema MILP por el optimizador CPLEX               *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril de 2019                                                       *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use symtypes
use ParGloRed, only: NumGruRamSis

!use cplex_ifaces, only: CPXgetx      , CPXgetstat    , &       !Windows (comentarizar para Linux)
!                        CPXmipopt    , CPXprimopt    , &       !Windows (comentarizar para Linux)
!                        CPXgetobjval , CPXchgprobtype, &       !Windows (comentarizar para Linux)
!                        CPXsolution  , CPXsetintparam, &       !Windows (comentarizar para Linux)
!                        CPXsetdblparam, CPXfreeprob  , &       !Windows (comentarizar para Linux)
!                        CPXchgobj     , CPXwriteprob , &       !Windows (comentarizar para Linux)
!                        CPXwritemipstarts ,            &       !Windows (comentarizar para Linux)
!                        CPXsetlogfilename, CPXreadcopymipstarts, & !Windows (comentarizar para Linux)
!                        CPXsolwrite   , CPXclpwrite  , &       !Windows (comentarizar para Linux)
!                        CPXrefineconflict, CPXdualopt, &       !Windows (comentarizar para Linux)
!                        CPXgetmiprelgap, CPXgetbestobjval      !Windows (comentarizar para Linux)

use cplex_cons, only:   CPXPROB_FIXEDMILP, CPX_ON, CPX_PARAM_EPRHS, &
                        CPX_PARAM_EPINT, CPX_PARAM_PROBE, CPXPROB_MILP, &
                        CPX_PARAM_SCRIND, CPXPROB_LP

implicit none

integer                 CPXgetx      , CPXgetstat    , &          !Linux (comentarizar para Windows)
                        CPXmipopt    , CPXprimopt    , &          !Linux (comentarizar para Windows)
                        CPXgetobjval , CPXchgprobtype, &          !Linux (comentarizar para Windows)
                        CPXsolution  , CPXsetintparam, &          !Linux (comentarizar para Windows)
                        CPXsetdblparam, CPXfreeprob  , &          !Linux (comentarizar para Windows)
                        CPXchgobj    ,  CPXwriteprob , &          !Linux (comentarizar para Windows)
                        CPXwritemipstarts ,            &          !Linux (comentarizar para Windows)
                        CPXsetlogfilename,   CPXreadcopymipstarts, &  !Linux (comentarizar para Windows)
                        CPXsolwrite     , CPXclpwrite, &          !Linux (comentarizar para Windows)
                        CPXrefineconflict, CPXdualopt, &          !Linux (comentarizar para Windows)
                        CPXgetmiprelgap, CPXgetbestobjval         !Linux (comentarizar para Windows)

integer status, bandera, ibanbit, solstat, sistema, ite, i, logfile
integer  infconst, infvar

data status  / 0 /

integer infactible, ierror, imprime, itipo

real*8     x, slack, y, yac, Costo_Gen, fobj, fobjMILP, gap, agap, objval

real*8    volumhR ( maxuh, MAXINT ), cargahR ( maxuh, MAXINT ), a_tra_res ( maxint + 25, nmxvia ), &
          gplah ( nmxpla, maxint ), qplah ( nmxpla, maxint ), aa4 ( nmxemb, maxdia, 7 )
integer*4 UniOnHid ( nmxpla, maxint )

CHARACTER fecha_Ej*19, ssistema*1
character*15 aaux1

DIMENSION   slack      ( maxresMILP )
DIMENSION   y          ( maxresMILP )
DIMENSION   yac        ( maxvarMILP )
DIMENSION   x          ( maxvarMILP )
DIMENSION   Costo_Gen  ( maxsis )

ierror = 0
bandera = 0
ibanbit = 1
infconst = 0
infvar = 0

! Se inicializa la solucion del problema
!xMILP = 0.0
x = 0.0
y = 0.0
slack = 0.0
yac = 0.0
Costo_Gen = 0.0



! Mensajes de CPLEX a pantalla
status = CPXsetintparam (enb, CPX_PARAM_SCRIND, CPX_ON)

!status = CPXfopen ("mylog.log", "w")
!status = CPXsetlogfilename (enb, logfile)

! si existen ramas restringidas y ya no existen violaciones de transmision
if ( SiViolacion .eq. 0 .and. NumGruRamSis (sistema ) .gt. 0 .and. ite .ge. 1 &
    .and. SiTransmision .gt. 0 .and. nomsis(sistema) .eq. 'SIN' ) then
! 	si la solucion es en variables enteras
	if ( TipoProblema .eq. 1 ) then
     	call FijaAsignacion
	endif
endif

! si la solucion es en variables continuas
if ( TipoProblema .eq. 0 ) then
	goto 2345
endif

! si se desea utilizar una solucion previa y un conjunto activo previo
if ( SiSolucionInicial .eq. 1 ) then
!   Se lee una solucion previa
    status = CPXreadcopymipstarts(enb, lpMILP, 'SolMip.mst')
endif

! Se resuelve el problema entero mixto linealizado
status = CPXmipopt (enb, lpMILP);
if ( status .ne. 0) then
    if ( imprime .ne. 1 ) then
        write (*,*) ' Error al resolver el MILP'
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write(*,*) '1'
!       Se ecribe resultado de semaforos
        call EscSemaforosError
!       algoritmo no termina bien
        call SalidaError
        stop
    else
        imprime = 3
        return
    endif
end if

! optimum objective value
status = CPXgetobjval (enb, lpMILP, fobjMILP)
status = CPXgetbestobjval (enb, lpMILP, objval)

! diferencia (GAP) absoluta
agap = abs( fobjMILP - objval )

! diferencia (GAP) realativa
status = CPXgetmiprelgap (enb, lpMILP, gap)

if ( status .eq. 0 ) then
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux1, 5100 ) gap*100.0
    Call FechaEjecucion (fecha_Ej)
    BMensaje = fecha_Ej//' '//NomEjecu//'100 GAP (%) OBTENIDO POR CPLEX  :'//aaux1
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux1, 5100 ) GAPCPLEX*100
    Call FechaEjecucion (fecha_Ej)
    BMensaje = fecha_Ej//' '//NomEjecu//'100 GAP (%) DESEADO POR USUARIO :'//aaux1
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    if ( gap*100.0 .gt. GAPCPLEX*100 ) then
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 CUIDADO! SOLUC PARO POR TIEMPO'
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif
    write ( aaux1, 5200 ) agap/Base
    Call FechaEjecucion (fecha_Ej)
    BMensaje = fecha_Ej//' '//NomEjecu//'100 GAP ABS OBTENIDO POR CPLEX  :'//aaux1
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
endif
! se escribe la solucion
status = CPXwritemipstarts (enb, lpMILP,'SolMip.mst', 0, 0)

! se escribe la solucion
!status = CPXsolwrite (enb, lpMILP, 'myfile.txt');


if ( status .ne. 0 ) then
    if ( imprime .ne. 1 ) then
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'100 PROBLEMA INFACTIBLE '
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        if ( itipo .eq. 0 ) then
            bmensaje = fecha_Ej//' '//NomEjecu//'100 REVISE LOS MENSAJES EN VENTANA'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!           status = CPXsetintparam (enb, CPX_PARAM_SCRIND, CPX_OFF)
!           status = CPXrefineconflict (enb, lpMILP, infconst, infvar)
!           status = CPXclpwrite (enb, lpMILP, 'Infactibilidad.txt')
            infactible = 1
!           se libera de la memoria el modelo infactible
            status = CPXfreeprob (enb, lpMILP)
            SemBandera ( 7 ) = 1
            write(*,*) '1'
            write(998,*) '1'
!           terminacion anormal del algoritmo
            call SalidaError
!           Se ecribe resultado de semaforos
            write ( UniSemaf, * ) SemBandera ( 1 ),',',' Corte de energia'
            write ( UniSemaf, * ) SemBandera ( 2 ),',',' Excedente de energia'
            write ( UniSemaf, * ) SemBandera ( 3 ),',',' Escasez de reservas'
            write ( UniSemaf, * ) SemBandera ( 4 ),',',' Infactibilidad en transmision'
            write ( UniSemaf, * ) SemBandera ( 5 ),',',' Violacion de limites de energia termo'
            write ( UniSemaf, * ) SemBandera ( 6 ),',',' Violacion de limites de energia hidro'
            write ( UniSemaf, * ) SemBandera ( 7 ),',',' Problema de optimizacion infactible'
            write ( UniSemaf, * ) SemBandera ( 8 ),',',' Violacion de limites unidades hidro'
!            write ( UniSemaf, * ) SemBandera ( 9 ),',',' Violacion de limites de combustible'
            stop
        else
!           Inicializa duales escalados
            dualresr10zesc = dualresr10z
            dualres10zesc = dualres10z
            dualresszesc = dualressz
            dualresrezesc = dualresrez
!           si se requiere escalar los precios (solo AUHE puede no solicitarlo)
            if ( SiEscalaConex .eq. 1 ) then
!               se escalan precios de servicios conexos
                call EscalaPreciosConexos
            endif
!           se escriben resultados de reservas por zona aceptadas por CENACE
            call ResZonCENACE ( ite, sistema, imprime  )
!           se escriben resultados de reservas por sistema aceptadas por CENACE
            call ResSisCENACE ( ite, sistema, imprime  )
            return 
        endif
    else
        imprime = 3
        return
    endif
end if

! optimum solution
status = CPXgetx (enb, lpMILP, xMILP, 0, NumVarAsig-1);
if ( status .ne. 0 ) then
    if ( imprime .ne. 1 ) then
        write (*,*) ' Error al obtener solucion del MILP'
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'102 TERMINACION ERROR FATAL CPLEX'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write(*,*) '1'
!       Se ecribe resultado de semaforos
        call EscSemaforosError
!       algoritmo no termina bien
        call SalidaError
        stop
    else
        imprime = 3
        return
    endif
end if

! si se requiere modelado hidro y refinamiento
if ( SiModHid .eq. 1 .and. NumUniHid .gt. 0 .and. SiRefHid .eq. 1 ) then
!    if ( NTINTR .le. 25 .or. (NTINTR .gt. 25 .and.  ite .ge. IterPerdidas ) ) then
    !   se actualiza la funcion de generacion hidro (LP)
	    call ActPotenciaGenHidro ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4, 2 )
    !   status = CPXwriteprob (enb, lpMILP, 'RHf.sav', 'SAV')
!       si se desea utilizar una solucion previa y un conjunto activo previo
        if ( SiSolucionInicial .eq. 1 ) then
!           Se lee una solucion previa
            status = CPXreadcopymipstarts(enb, lpMILP, 'SolMip.mst')
        endif
    !   Se resuelve el problema entero mixto linealizado, con la actualizacion de coeficientes
        status = CPXmipopt (enb, lpMILP)
        if ( status .ne. 0) then
            if ( imprime .ne. 1 ) then
                write (*,*) ' Error al resolver el MILP'
                Call FechaEjecucion (fecha_Ej)
                bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                write(*,*) '1'
        !       Se ecribe resultado de semaforos
                call EscSemaforosError
        !       algoritmo no termina bien
                call SalidaError
!                status = CPXrefineconflict (enb, lpMILP, infconst, infvar)
!                status = CPXclpwrite (enb, lpMILP, 'Infactibilidad.txt')
                stop
            else
                imprime = 3
                return
            endif
        end if

!    endif
endif

! se escribe la solucion a un archivo para despues utilizarla como solucion inicial
status = CPXwritemipstarts (enb, lpMILP,'SolMip.mst', 0, 0)

! se cambia el problema a uno de programacion lineal
! fijando la solucion de las variables enteras
status = CPXchgprobtype (enb, lpMILP, CPXPROB_FIXEDMILP)

Write( ssistema, '(I1)' )  sistema
! se escribe el modelo LP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'LP'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'LP'//ssistema//'.sav', 'SAV')

2345 continue

! si la solucion es en variables continuas
if ( TipoProblema .eq. 0 ) then                              !Linux (comentarizar para Windows)
! 	se cambia el problema a uno de programacion lineal
	status = CPXchgprobtype (enb, lpMILP, CPXPROB_LP)   !Linux (comentarizar para Windows)
endif                              						!Linux (comentarizar para Windows)

status = CPXdualopt (enb, lpMILP)
if ( status .ne. 0) then
    if ( imprime .ne. 1 ) then
        write (*,*) ' Error al resolver el problema de LP'
        write (*,*) ' '
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'103 TERMINACION ERROR FATAL CPLEX'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write(*,*) '1'
!       Se ecribe resultado de semaforos
        call EscSemaforosError
!       algoritmo no termina bien
        call SalidaError
        infactible = 1
        SemBandera ( 7 ) = 1
        write(998,*) '1'
!       Se ecribe resultado de semaforos
        write ( UniSemaf, * ) SemBandera ( 1 ),',',' Corte de energia'
        write ( UniSemaf, * ) SemBandera ( 2 ),',',' Excedente de energia'
        write ( UniSemaf, * ) SemBandera ( 3 ),',',' Escasez de reservas'
        write ( UniSemaf, * ) SemBandera ( 4 ),',',' Infactibilidad en transmision'
        write ( UniSemaf, * ) SemBandera ( 5 ),',',' Violacion de limites de energia termo'
        write ( UniSemaf, * ) SemBandera ( 6 ),',',' Violacion de limites de energia hidro'
        write ( UniSemaf, * ) SemBandera ( 7 ),',',' Problema de optimizacion infactible'
        write ( UniSemaf, * ) SemBandera ( 8 ),',',' Violacion de limites unidades hidro'
!        write ( UniSemaf, * ) SemBandera ( 9 ),',',' Violacion de limites de combustible'
!           status = CPXrefineconflict (enb, lpMILP, infconst, infvar)
!           status = CPXclpwrite (enb, lpMILP, 'Infactibilidad.txt')
        stop
    else
        imprime = 3
        return
    endif
end if

! se obtienen los duales del problema LP
status = CPXsolution ( enb, lpMILP, solstat, fobj, x, y, slack, yac )

!if ( status .ne. 0 .or. ( solstat .gt. 1 .and. solstat .lt. 5 ) ) then
if ( status .ne. 0 ) then
    if ( imprime .ne. 1 ) then
        write (*,*) ' Error al obtener la solucion del LP'
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'104 TERMINACION ERROR FATAL CPLEX'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write(*,*) '1'
        write(333,*) '1, solstat:', solstat
!       Se ecribe resultado de semaforos
        call EscSemaforosError
!       algoritmo no termina bien
        call SalidaError
        stop
    else
        imprime = 3
        return
    endif
endif

! se escribe la solucion
!status = CPXsolwrite (enb, lpMILP, 'continuas.txt');
do i = 1, NumVarAsig
    xMILP ( i ) = x ( i )
enddo

! se almacena solucion del MILP y LP
call AlmacenaSol ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4, y )

! se calculan perdidas y sensibilidades
call CalSnsPerIntervalo ( sistema, ite )

! se escriben resultados del MILP y LP
call ResulMILP ( ite, sistema, imprime, y )

! si la solucion es en variables enteras
if ( TipoProblema .eq. 1 ) then
!   se regresa el problema a uno de programacion entera mixta
	status = CPXchgprobtype (enb, lpMILP, CPXPROB_MILP)
endif

! si se requiere modelado hidro se actualiza la funcion de generacion hidro (MILP)
if ( SiModHid .eq. 1 .and. NumUniHid .gt. 0 .and. SiRefHid .eq. 1 ) then
!    if ( NTINTR .le. 25 .or. (NTINTR .gt. 25 .and.  ite .gt. IterPerdidas ) )then
        call ActPotenciaGenHidroL
!    endif
endif

5100 FORMAT (F9.6)
5200 FORMAT (F15.2)

return
end
    
    
! ---------------------------------------------------------------------
! Almacena resultados de asignaciones, despachos y variables duales   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2019                                                        *
! ---------------------------------------------------------------------
Subroutine AlmacenaSol ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4, y )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, dualgrurams, inagruram, &
                     dualgrurami, bangruram, MulPerdidas, ApuMulPerInt, LisMulPerInt, IteMulPerInt 

Implicit none

INTEGER   i, k, l, m, u, r, s, br, grupo, dia, kuenta, icuenta, j, iter

Real*8    y, sum

DIMENSION   y          ( maxresMILP )

real*8    volumhR ( maxuh, MAXINT ), cargahR ( maxuh, MAXINT ), a_tra_res ( maxint + 25, nmxvia ), &
          gplah ( nmxpla, maxint ), qplah ( nmxpla, maxint ), aa4 ( nmxemb, maxdia, 7 )
integer*4 UniOnHid ( nmxpla, maxint )

integer*4 IntMulPer ( 3*maxint ), SisMulPer ( 3*maxint )

! Inicializa arreglos asociados a restricciones activas de perdidas
kuenta = 0
MulPerdidas = 0
LisMulPerInt = 0
ApuMulPerInt = 0
IntMulPer = 0
SisMulPer = 0
IteMulPerInt = 0

! guarda solucion de generacion de unidades de rango continuo
do u = 1, NumUniRC
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        GENUNRC ( u, i ) = xMILP ( IGRC + u + (i-1)*NumUniRC - 1 )
        if ( GENUNRC ( u, i ) .lt. 0.0001 ) then
            GENUNRC ( u, i ) = 0.0
        endif
    enddo
enddo

! guarda solucion de generacion de unidades de rango discontinuo
do u = 1, NumUniRD
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        GENUNRD ( u, i ) = 0.0
        do m = 1, NumModRD(u)
            GENUNRD ( u, i ) = GENUNRD ( u, i ) + xMILP ( IGRD + INIURDI ( u, i ) + m - 1 )
        enddo
        if ( GENUNRD ( u, i ) .lt. 0.0001 ) then
            GENUNRD ( u, i ) = 0.0
        endif
    enddo
enddo

! guarda solucion de generacion de unidades hidro
do u = 1, NumUniHid
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        GENUNH ( u, i ) = xMILP ( IGH + u + (i-1)*NumUniHid - 1 )
        if ( GENUNH ( u, i ) .lt. 0.0001 ) then
            GENUNH ( u, i ) = 0.0
        endif
    enddo
enddo

! guarda solucion de generacion de unidades renovables
do u = 1, NumUniRE
!   para todos los intervalos de planeacion
    do i = 1, NTINTR
        GENUNRE ( u, i ) = xMILP ( IGRE + u + (i-1)*NumUniRE - 1 )
        if ( GENUNRE ( u, i ) .lt. 0.0001 ) then
            GENUNRE ( u, i ) = 0.0
        endif
    enddo
enddo

dualbalance = 0.0
dualresr10z = 0.0
dualres10z = 0.0
dualressz = 0.0
dualresr10s = 0.0
dualres10s = 0.0
dualresss = 0.0
dualresres = 0.0
dualgrurams = 0.0
dualgrurami = 0.0
dualperdidas = 0.0
dualenerbal = 0.0
dualsgpogas = 0.0
dualigpogas = 0.0

! guarda solucion dual de restricciones de balance de potencia
!   para cada subsistema
    k = IRBAL
    do l = 1, numsis
!       si la isla esta activa
        if ( EstadoIsla(l) .eq. 1 ) then
!           para todos los intervalos       
            do i = 1 , NTINTR
                dualbalance ( l, i ) = y ( k )
                k = k + 1
            enddo
        endif
    enddo

!   guarda solucion dual de restricciones de reserva rodante de 10 minutos por zona
!   para cada subsistema
    k = IRR10Z
    do l = 1, NumGruRes*SiOferComResZona
!       para todos los intervalos       
        do i = 1 , NTINTR
            if ( ReqResR10 (l, 1, i) .gt. 0.0 ) then
                dualresr10z ( l, i ) = y ( k )
                k = k + 1
            endif
        enddo
    enddo

!   guarda solucion dual de restricciones de reserva de 10 minutos por zona
!   para cada subsistema
    k = IR10Z
    do l = 1, NumGruRes*SiOferComResZona
!       para todos los intervalos       
        do i = 1 , NTINTR
            if ( ReqRes10 (l, 1, i) .gt. 0.0 ) then
                dualres10z ( l, i ) = y ( k )
                k = k + 1
            endif
        enddo
    enddo

!   guarda solucion dual de restricciones de reserva suplementaria por zona
!   para cada subsistema
    k = IRSZ
    do l = 1, NumGruRes*SiOferComResZona
!       para todos los intervalos       
        do i = 1 , NTINTR
            if ( ReqResSup (l, 1, i) .gt. 0.0 ) then
                dualressz ( l, i ) = y ( k )
                k = k + 1
            endif
        enddo
    enddo

    ! guarda solucion dual de restricciones de reserva de regulacion por zona
    ! para cada subsistema
    k = IREZ
    dualresrez = 0.0
    do l = 1, NumGruRes*SiOferComResZona
    !   para todos los intervalos       
        do i = 1 , NTINTR
            if ( ReqResReg (l, 1, i) .gt. 0.0 ) then
                dualresrez ( l, i ) = y ( k )
                k = k + 1
            endif
        enddo
    enddo

!   guarda solucion dual de restricciones de reserva rodante de 10 minutos por sistema
!   para cada subsistema
    s = 0
    k = IRR10S
    do l = 1, numsis*SiOferComResSis
!       si la isla esta activa
        if ( EstadoIsla(l) .eq. 1 ) then
            s = s + 1
!           para todos los intervalos       
            do i = 1 , NTINTR
                if ( ReqResR10S (s, 1, i) .gt. 0.0 ) then
                    dualresr10s ( l, i ) = y ( k )
                    k = k + 1
                endif
            enddo
        endif
    enddo

!   guarda solucion dual de restricciones de reserva de 10 minutos por sistema
!   para cada subsistema
    s = 0
    k = IR10S
    do l = 1, numsis*SiOferComResSis
!       si la isla esta activa
        if ( EstadoIsla(l) .eq. 1 ) then
            s = s + 1
!           para todos los intervalos       
            do i = 1 , NTINTR
                if ( ReqRes10S (s, 1, i) .gt. 0.0 ) then
                    dualres10s ( l, i ) = y ( k )
                    k = k + 1
                endif
            enddo
        endif
    enddo

!   guarda solucion dual de restricciones de reserva suplementaria por sistema
!   para cada subsistema
    s = 0
    k = IRSS
    do l = 1, numsis*SiOferComResSis
!       si la isla esta activa
        if ( EstadoIsla(l) .eq. 1 ) then
            s = s + 1
!           para todos los intervalos       
            do i = 1 , NTINTR
                if ( ReqResSupS (s, 1, i) .gt. 0.0 ) then
                    dualresss ( l, i ) = y ( k )
                    k = k + 1
                endif
            enddo
        endif
    enddo

!   guarda solucion dual de restricciones de reserva de regulacion por sistema
!   para cada subsistema
    s = 0
    k = IRES
    do l = 1, numsis*SiOferComResSis
!       si la isla esta activa
        if ( EstadoIsla(l) .eq. 1 ) then
            s = s + 1
!           para todos los intervalos       
            do i = 1 , NTINTR
                if ( ReqResRegS (s, 1, i) .gt. 0.0 ) then
                    dualresres ( l, i ) = y ( k )
                    k = k + 1
                endif
            enddo
        endif
    enddo

! guarda solucion dual de restricciones de combustible
!   para cada subsistema
    k = IRGPOGAS
    
!   para todos los grupos con limitacion superior de combustible
    do grupo = 1, NumGruGas*SiGpoGas
!       si el grupo esta activo
        if ( GpoAct ( grupo ) .eq. 1 ) then
!           para todos los dias del horizonte
            do dia = 1, DURDIA
                dualsgpogas ( grupo, dia ) = y ( k )
                k = k + 1
            enddo
        endif
    enddo
!   para todos los grupos con limitacion inferior de combustible
    do grupo = 1, NumGruGas*SiGpoGas
!       si el grupo esta activo
        if ( GpoAct ( grupo ) .eq. 1 ) then
!           para todos los dias del horizonte
            do dia = 1, DURDIA
                dualigpogas ( grupo, dia ) = y ( k )
                k = k + 1
            enddo
        endif
    enddo

!   para todos los grupos con limitacion superior de combustible
    do grupo = 1, NResComb*SiGpoGas*0
!       si el grupo esta activo
        if ( ActResComb ( grupo ) .eq. 1 ) then
            dualsupgpogas ( grupo ) = y ( k )
            k = k + 1
        endif
    enddo
!   para todos los grupos con limitacion inferior de combustible
    do grupo = 1, NResComb*SiGpoGas*0
!       si el grupo esta activo
        if ( ActResComb ( grupo ) .eq. 1 ) then
            dualinfgpogas ( grupo ) = y ( k )
            k = k + 1
        endif
    enddo

    
!   guarda solucion dual de restricciones de limite de energia termo
    k = IRGPTER
!   para todos los grupos con limitacion superior de energia termo
    do grupo = 1, NumGruUTer*SiEnerTer
!       para todos los dias del horizonte
        do dia = 1, DURDIA
            dualsgpter ( grupo, dia ) = y ( k )
            k = k + 1
        enddo
    enddo
!   para todos los grupos con limitacion inferior de energia termo
    do grupo = 1, NumGruUTer*SiEnerTer
!       para todos los dias del horizonte
        do dia = 1, DURDIA
            dualigpter ( grupo, dia ) = y ( k )
            k = k + 1
        enddo
    enddo

!   para todos los grupos con limitacion superior de energia termo
    do grupo = 1, NResEner*SiEnerTer*0
!       si el grupo esta activo
        if ( ActResEner ( grupo ) .eq. 1 ) then
            dualsupener ( grupo ) = y ( k )
            k = k + 1
        endif
    enddo
!   para todos los grupos con limitacion inferior de energia termo
    do grupo = 1, NResEner*SiEnerTer*0
!       si el grupo esta activo
        if ( ActResEner ( grupo ) .eq. 1 ) then
            dualinfener ( grupo ) = y ( k )
            k = k + 1
        endif
    enddo

    ! si se uso una soluion inicial
    if ( SiSolucionInicial .eq. 1 ) then
        k = IRPERD - 1
    else
        k = NumresAsig
    endif
   
    ! guarda solucion dual de restricciones de transmision y perdidas en transmision
    do l = 1, numsis
    !   si la isla esta activa
        if ( EstadoIsla(l) .eq. 1 ) then
            iter = 0
    !       para todas las restricciones adicionales
            do i = 1 , NumResAdi        
    !           si se trata de una restriccion de transmision
                if ( InfRestAdi ( i ) .gt. 0 .and. SiTransmision .eq. 1 ) then
    !               si se trata de una restriccion de menor o igual (limite superior)
                    if ( IsenRestAdi ( i ) .eq. 'L' ) then
                        dualgrurams ( InfRestAdi ( i ), IntRestAdi ( i ) ) = y ( k + i )
                    else if ( IsenRestAdi ( i ) .eq. 'G' ) then
                        dualgrurami ( InfRestAdi ( i ), IntRestAdi ( i ) ) = y ( k + i )
                    endif
                else
    !               se trata de una restriccion de perdidas
                    if ( InfRestAdi ( i ) .eq. 0 .and. SiPerdidas .eq. 1 ) then
                        dualperdidas ( l, IntRestAdi(i) ) = dualperdidas ( l, IntRestAdi(i) ) + y ( k + i )
                        if ( IntRestAdi(i) .eq. 1 ) iter = iter + 1
                            if (  abs ( y ( k + i )  ) .gt. 0.0 ) then
                                kuenta = kuenta + 1
                                IntMulPer ( kuenta ) = IntRestAdi(i)
                                IteMulPerInt ( kuenta ) = iter
                                SisMulper ( kuenta ) = l
                                MulPerdidas ( kuenta ) = y ( k + i )
                            endif
                    endif
                endif   
            enddo    
        endif
    enddo

! almacena resultados hidraulicos
!if ( NumUniHid .gt. 0 .and. SiModHid .eq. 1 ) then
!   variables duales de restricciones de seguridad
!    do l = 1, Numembalses
!        dualenerbal ( l ) = y ( ApResEner(l) )
!    enddo
!    call Almacena_Hidro ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4 )
!endif

k = 0
! almacena resultados de limitaciones hidro
if ( NumUniHid .gt. 0 ) then
!   variables duales de restricciones de limites de energia
    do l = 1, Numembalses
!       si el embalse tiene restriccion de energia
        if ( RestEnergia ( l ) .eq. 1 .and. SiEnerHid .eq. 1 ) then
            k = k + 1
            dualenerbal ( l ) = ( y ( APResLimSH + k ) + y ( APResLimIH + k ) ) / Base
        endif
    enddo
    if ( SiModHid .eq. 1 ) then
        call Almacena_Hidro ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4 )
    endif
endif
    

! Forma lista de apuntadores de multiplicadores de perdidas por intervalo
do l = 1, numsis
   ApuMulPerInt ( l, 1 ) = 1; icuenta = 0
   do i = 1, NTINTR
       do k = 1, kuenta
          if ( SisMulPer(k) .eq. l .and. IntMulPer ( k ) .eq. i ) then
             icuenta = icuenta + 1
             LisMulPerInt ( icuenta ) = k
          endif
       enddo
       ApuMulPerInt ( l, i + 1 ) = icuenta + 1
   enddo
enddo

! Verifica que la solucion no sea degenerada en perdidas
do l = 1, numsis
    do i = 1,  NTINTR
       sum = 0.0
       do j =  ApuMulPerInt ( l, i  ),  ApuMulPerInt ( l, i + 1 ) - 1
          icuenta = LisMulPerInt ( j )
          sum = sum + MulPerdidas ( icuenta )
       enddo 
       ! Si la solucion es degenerada escala multiplicadores
       if ( abs ( sum ) .gt. abs ( 1.20*dualbalance ( l, i ) ) ) then
           do j =  ApuMulPerInt ( l, i  ),  ApuMulPerInt ( l, i + 1 ) - 1
              icuenta = LisMulPerInt ( j )
              MulPerdidas ( icuenta ) = MulPerdidas ( icuenta )*(dualbalance ( l, i ) / sum )
           enddo 
       endif
    enddo
enddo

! Inicializa duales escalados
dualresr10zesc = dualresr10z
dualres10zesc = dualres10z
dualresszesc = dualressz
dualresrezesc = dualresrez
! si se requiere escalar los precios (solo AUHE puede no solicitarlo)
if ( SiEscalaConex .eq. 1 ) then
!   se escalan precios de servicios conexos
    call EscalaPreciosConexos
endif


return
end
    
    
Subroutine ReqResZona ( k, m )
! ---------------------------------------------------------------------
! Restricciones de requerimiento del CENACE de reserva por zona, en   *
! el problema de asignacion (MILP).                                   *
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

INTEGER   i, k, kr10, k10, ks, kr, m, r, ro, s, u, iniciou, unidad, modo, d
character*3 leti

kr10 = ICARR10G
k10 = ICAR10G
ks = ICARSG
kr = ICARRG
! restricciones de requerimiento del CENACE de reserva por zona
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE R 10 min porzona:', m + 1
IRR10Z = m + 1
! para todo los grupos de reserva
do r = 1, NumGruRes
!   para todos los intervalos
    do i = 1, NTINTR
!       si existe requerimiento de reserva rodante de 10 min
        if ( ReqResR10 (r, 1, i) .gt. 0.0 ) then
!           para las unidades de rango continuo que estan en ese grupo (zona)
            iniciou = ApunURCxZona ( r )
            do unidad = 1 , NumURCxZona ( r )*SiUniRC
                u = UniRCxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono en este periodo
                if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                   si la reserva de regulacion contribuye a la reserva rodante
                    if ( SiRegEnRod .eq. 1 ) then
!                       coeficiente de la variable de reserva de regulacion
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                        k = k + 1
    !                   si se permite hacer uso de reserva
                        if ( TipoEjecu .le. 1 ) then
    !                       coeficiente de variable de uso
                            aaMILP ( k ) = - 1.0
    !                       columna asociada
                            jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
                    endif
!                   coeficiente de la variable de reserva rodante de 10 min
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   si se permite hacer uso de reserva en AUGC
                    if ( SiRelRod .eq. 1 ) then
!                       coeficiente de variable de uso
                        aaMILP ( k ) = - 1.0
!                       columna asociada
                        jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo (zona)
            iniciou = ApunURDxZona ( r )
            do unidad = 1 , NumURDxZona ( r )*SiUniRD
                u = UniRDxZona ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible y cordinable en este periodo y modo
                    if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                       si es unidad disponible en este periodo
                        if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                           si la reserva de regulacion contribuye a la reserva rodante
                            if ( SiRegEnRod .eq. 1 ) then
!                               coeficiente de la variable de regulacion
                                aaMILP ( k  ) = 1.0
                                jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                                k = k + 1
                            endif
!                           coeficiente de la variable de reserva rodante de 10 min
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        endif
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
!           para las unidades hidro que estan en ese grupo (zona)
            iniciou = ApunUHxZona ( r )
            do unidad = 1 , NumUHxZona ( r )*SiUniH
                u = UniHxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono con bandera 1 en este periodo
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .ne. 1 ) then
!                   coeficiente de la variable de reserva rodante de 10 min
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 .and. SiRegEnRod .eq. 1 ) then
!                   coeficiente de la variable de reserva de regulacion
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
                iniciou = iniciou + 1
            enddo

!           si la reserva de regulacion contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 1 ) then
!               para los requerimientos de reserva de regulacion secundaria
                do s = 1, NumBloRReg( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr
                    k = k + 1
                    kr = kr + 1
                enddo
            endif

!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRR10( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = kr10
                k = k + 1
                kr10 = kr10 + 1
            enddo
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva rodante 10 min porzona '//trim(NomZonaRes(r))//' intervalo '//trim(leti)//'"'
        else
!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRR10( i )
!               cota superior de reserva aceptada por CENACE
                ubMILP ( kr10 ) = 0.0
                kr10 = kr10 + 1
            enddo
        endif
    enddo
enddo

kr10 = ICARR10G
kr = ICARRG
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE 10 min por zona :', m + 1
IR10Z = m + 1
! para todo los grupos de reserva
do r = 1, NumGruRes
!   para todos los intervalos
    do i = 1, NTINTR
!       si existe requerimiento de reserva de 10 min
        if ( ReqRes10 (r, 1, i) .gt. 0.0 ) then
!           para las unidades de rango continuo que estan en ese grupo (zona)
            iniciou = ApunURCxZona ( r )
            do unidad = 1 , NumURCxZona ( r )*SiUniRC
                u = UniRCxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono en este periodo
                if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                   si la reserva de regulacion contribuye a la reserva rodante
                    if ( SiRegEnRod .eq. 1 ) then
!                       coeficiente de la variable de reserva de regulacion
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                        k = k + 1
    !                   si se permite hacer uso de reserva
                        if ( TipoEjecu .le. 1 ) then
    !                       coeficiente de variable de uso
                            aaMILP ( k ) = - 1.0
    !                       columna asociada
                            jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
                    endif
!                   coeficiente de la variable de reserva rodante de 10 min
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   si se permite hacer uso de reserva en AUGC
                    if ( SiRelRod .eq. 1 ) then
!                       coeficiente de variable de uso
                        aaMILP ( k ) = - 1.0
!                       columna asociada
                        jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
                endif
!               si es unidad disponible, coordinable y asignable en este periodo
                if ( DispoURC ( u , i ) .and. CoordURC ( u , i ) .eq. 1 .and. AsignURC ( u, i ) .eq. 1 ) then
!                   coeficiente de la variable de reserva no rodante
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo (zona)
            iniciou = ApunURDxZona ( r )
            do unidad = 1 , NumURDxZona ( r )*SiUniRD
                u = UniRDxZona ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible en este periodo
                    if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                       si la reserva de regulacion contribuye a la reserva rodante
                        if ( SiRegEnRod .eq. 1 ) then
!                           coeficiente de la variable de regulacion
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        endif
!                       coeficiente de la variable de reserva rodante de 10 min
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    endif
                enddo
!               para los modos arrancables
                do modo = 2, NumModRD ( u )
                    if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!                       si es unidad disponible, asignable  y cordinable en este periodo
                        if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
!                           coeficiente de la variable de reserva no rodante
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        endif
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
!           para las unidades hidro que estan en ese grupo (zona)
            iniciou = ApunUHxZona ( r )
            do unidad = 1 , NumUHxZona ( r )*SiUniH
                u = UniHxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono con bandera 1 en este periodo
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .ne. 1 ) then
!                   coeficiente de la variable de reserva rodante de 10 min
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 .and. SiregEnrod .eq. 1 ) then
!                   coeficiente de la variable de reserva de regulacion
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
!               si es unidad disponible, coordinable y asignable en este periodo
                if ( DispoUH ( u , i ) .and. CoordUH ( u , i ) .eq. 1 .and. AsignUH ( u, i ) .eq. 1 ) then
!                   coeficiente de la variable de reserva no rodante
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las demandas controlables de 10 minutos que estan en ese grupo (zona)
            iniciou = ApunCarxZona ( r )
            do unidad = 1 , NumCarxZona ( r )*0
                d = CarxZona ( iniciou )
!               coeficiente de la variable de demanda controlable de 10
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = ICC10 + d + (i-1)*NumOferDem - 1
                k = k + 1
                iniciou = iniciou + 1
            enddo

!           si la reserva de regulacion contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 1 ) then
!               para los requerimientos de reserva de regulacion secundaria
                do s = 1, NumBloRReg( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr
                    k = k + 1
                    kr = kr + 1
                enddo
            endif
            
!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRR10( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = kr10
                k = k + 1
                kr10 = kr10 + 1
            enddo
!           para los requerimientos de reserva de 10 minutos
            do s = 1, NumBloR10( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = k10
                k = k + 1
                k10 = k10 + 1
            enddo
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva 10 min porzona '//trim(NomZonaRes(r))//' intervalo '//trim(leti)//'"'
        else
!           para los requerimientos de reserva de 10 minutos
            do s = 1, NumBloR10( i )
!               cota superior de reserva aceptada por CENACE
                ubMILP ( k10 ) = 0.0
                k10 = k10 + 1
                kr10 = kr10 + 1
            enddo
        endif
    enddo
enddo

kr10 = ICARR10G
k10 = ICAR10G
kr = ICARRG
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE res sup por zona:', m + 1
IRSZ = m + 1
! para todo los grupos de reserva
do r = 1, NumGruRes
!   para todos los intervalos
    do i = 1, NTINTR
!       si existe requerimiento de reserva suplementaria
        if ( ReqResSup (r, 1, i) .gt. 0.0 ) then
!           para las unidades de rango continuo que estan en ese grupo (zona)
            iniciou = ApunURCxZona ( r )
            do unidad = 1 , NumURCxZona ( r )*SiUniRC
                u = UniRCxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono en este periodo
                if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                   si la reserva de regulacion contribuye a la reserva rodante
                    if ( SiRegEnRod .eq. 1 ) then
!                       coeficiente de la variable de reserva de regulacion
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                        k = k + 1
    !                   si se permite hacer uso de reserva
                        if ( TipoEjecu .le. 1 ) then
    !                       coeficiente de variable de uso
                            aaMILP ( k ) = - 1.0
    !                       columna asociada
                            jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
                    endif
!                   coeficiente de la variable de reserva rodante de 10
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   coeficiente de la variable de reserva rodante suplementaria
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRSRC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   si se permite hacer uso de reserva en AUGC
                    if ( SiRelRod .eq. 1 ) then
!                       coeficiente de variable de uso
                        aaMILP ( k ) = - 1.0
!                       columna asociada
                        jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
                endif
!               si es unidad disponible, coordinable y asignable en este periodo
                if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u, i ) .eq. 1 .and. AsignURC ( u, i ) .eq. 1 ) then
!                   coeficiente de la variable de reserva no rodante de 10
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   coeficiente de la variable de reserva no rodante suplementaria
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNRSRC + u + (i-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo (zona)
            iniciou = ApunURDxZona ( r )
            do unidad = 1 , NumURDxZona ( r )*SiUniRD
                u = UniRDxZona ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible en este periodo
                    if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                       si la reserva de regulacion contribuye a la reserva rodante
                        if ( SiRegEnRod .eq. 1 ) then
!                           coeficiente de la variable de regulacion
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        endif
!                       coeficiente de la variable de reserva rodante de 10
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       coeficiente de la variable de reserva rodante suplementaria
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRRSRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    endif
                enddo
!               para los modos arrancables
                do modo = 2, NumModRD ( u )
                    if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!                       si es unidad disponible, asignable  y cordinable en este periodo
                        if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
!                           coeficiente de la variable de reserva no rodante de 10
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
!                           coeficiente de la variable de reserva no rodante suplementaria
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNRSRD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        endif
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
!           para las unidades hidro que estan en ese grupo (zona)
            iniciou = ApunUHxZona ( r )
            do unidad = 1 , NumUHxZona ( r )*SiUniH
                u = UniHxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono con bandera 1 en este periodo
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .ne. 1 ) then
!                   coeficiente de la variable de reserva rodante de 10
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                    k = k + 1
!                   coeficiente de la variable de reserva rodante suplementaria
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRSH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 .and. SiRegEnRod .eq. 1 ) then
!                   coeficiente de la variable de reserva de regulacion
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
!               si es unidad disponible, coordinable y asignable en este periodo
                if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u, i ) .eq. 1 .and. AsignUH ( u, i ) .eq. 1 ) then
!                   coeficiente de la variable de reserva no rodante de 10
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
                    k = k + 1
!                   coeficiente de la variable de reserva no rodante suplementaria
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRNRSH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las demandas controlables de 10 minutos y suplementaria que estan en ese grupo (zona)
            iniciou = ApunCarxZona ( r )
            do unidad = 1 , NumCarxZona ( r )*0
                d = CarxZona ( iniciou )
!               coeficiente de la variable de demanda controlable de 10
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = ICC10 + d + (i-1)*NumOferDem - 1
                k = k + 1
!               coeficiente de la variable de demanda controlable suplementaria
                aaMILP ( k  ) = 1.0
                jcolMILP ( k ) = ICCS + d + (i-1)*NumOferDem - 1
                k = k + 1
                iniciou = iniciou + 1
            enddo

!           si la reserva de regulacion contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 1 ) then
!               para los requerimientos de reserva de regulacion secundaria
                do s = 1, NumBloRReg( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr
                    k = k + 1
                    kr = kr + 1
                enddo
            endif
            
!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRR10( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = kr10
                k = k + 1
                kr10 = kr10 + 1
            enddo
!           para los requerimientos de reserva de 10 minutos
            do s = 1, NumBloR10( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = k10
                k = k + 1
                k10 = k10 + 1
            enddo
!           para los requerimientos de reserva suplementaria
            do s = 1, NumBloRSu( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = ks
                k = k + 1
                ks = ks + 1
            enddo
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva suplementaria por zona '//trim(NomZonaRes(r))//' intervalo '//trim(leti)//'"'
        else
!           para los requerimientos de reserva rodante de 10 minutos
            do s = 1, NumBloRSu( i )
!               cota superior de reserva aceptada por CENACE
                ubMILP ( ks ) = 0.0
                ks = ks + 1
                kr10 = kr10 + 1
                k10 = k10 + 1
            enddo
        endif
    enddo
enddo

kr = ICARRG
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE regul secun zona:', m + 1
IREZ = m + 1
! para todo los grupos de reserva
do r = 1, NumGruRes
!   para todos los intervalos
    do i = 1, NTINTR
!       si existe requerimiento de reserva de regulacion
        if ( ReqResReg (r, 1, i) .gt. 0.0 ) then
!           para las unidades de rango continuo que estan en ese grupo (zona)
            iniciou = ApunURCxZona ( r )
            do unidad = 1 , NumURCxZona ( r )*SiUniRC
                u = UniRCxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono en este periodo
                if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                    k = k + 1
    !               si se permite hacer uso de reserva
                    if ( TipoEjecu .le. 1 ) then
!                       coeficiente de variable de uso
                        aaMILP ( k ) = - 1.0
!                       columna asociada
                        jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo (zona)
            iniciou = ApunURDxZona ( r )
            do unidad = 1 , NumURDxZona ( r )*SiUniRD
                u = UniRDxZona ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible y cordinable en este periodo y modo
!                    if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                   si es unidad disponible en este periodo
                    if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                       coeficiente de la variable de regulacion
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
!           para las unidades hidro que estan en ese grupo (zona)
            iniciou = ApunUHxZona ( r )
            do unidad = 1 , NumUHxZona ( r )*SiUniH
                u = UniHxZona ( iniciou )
!               si es unidad disponible y no condensador sincrono en este periodo
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 ) then
!                   coeficiente de la variable de reserva
                    aaMILP ( k  ) = 1.0
                    jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
                    k = k + 1
    !               si se permite hacer uso de reserva
                    if ( TipoEjecu .le. 1 ) then
!                       coeficiente de variable de uso
                        aaMILP ( k ) = - 1.0
!                       columna asociada
                        jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
                        k = k + 1
                    endif
                endif
                iniciou = iniciou + 1
            enddo
!           para los requerimientos de reserva de regulacion secundaria
            do s = 1, NumBloRReg( i )
!               coeficiente de la variable de reserva aceptada por CENACE
                aaMILP ( k  ) = -1.0
                jcolMILP ( k ) = kr
                k = k + 1
                kr = kr + 1
            enddo
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva regulacion secundaria por zona '//trim(NomZonaRes(r))//' intervalo '//trim(leti)//'"'
        else
!           para los requerimientos de reserva de regulacion
            do s = 1, NumBloRReg( i )
!               cota superior de reserva aceptada por CENACE
                ubMILP ( kr ) = 0.0
                kr = kr + 1
            enddo
        endif
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de minimo de unidades para  regul secun zona:', m + 1
! para todo los grupos de reserva
do r = 1, NumGruRes
!   para todos los intervalos
    do i = 1, NTINTR
!       si existe requerimiento de reserva de regulacion
        if ( ReqResReg (r, 1, i) .gt. 0.0 ) then
!           para las unidades de rango continuo que estan en ese grupo (zona)
            iniciou = ApunURCxZona ( r )
            do unidad = 1 , NumURCxZona ( r )*SiUniRC
                u = UniRCxZona ( iniciou )
!               si es unidad disponible en este periodo
                if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                if ( DispoURC ( u , i ) .eq. 1 ) then
!                   si se desea considerar bandas prohibidas y la unidad las tiene
                    if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
!                       si la unidad oferta reserva de regulacion secundaria
                        if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!                           para los rangos de la unidad
                            do ro = 1, NoRaOpRC ( u )
!                               coeficiente de variable de asignacion de reserva en rango operativo
                                aaMILP ( k ) = 1.0
!                               columna asociada
                                jcolMILP( k ) = INVRERORC + IRERORC ( u, i ) + ro - 1
                                k = k + 1
                            enddo
                        endif
                    else
!                       si la unidad oferta reserva de regulacion secundaria
                        if ( CalOferResRegRC ( u, i ) .gt. 0.0 ) then
!                           coeficiente de variable de asignacion de reserva
                            aaMILP ( k ) = 1.0
!                           columna asociada
                            jcolMILP( k ) = IVREGRC + INREGRC ( u, i ) - 1
                            k = k + 1
                        endif
                    endif
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo (zona)
            iniciou = ApunURDxZona ( r )
            do unidad = 1 , NumURDxZona ( r )*SiUniRD
                u = UniRDxZona ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible y cordinable en este periodo y modo
!                    if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                   si es unidad disponible en este periodo
                    if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                       coeficiente de variable de asignacion de reserva
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IVREGRD + INREGRD ( u, modo, i ) - 1
                        k = k + 1
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
!           para las unidades hidro que estan en ese grupo (zona)
            iniciou = ApunUHxZona ( r )
            do unidad = 1 , NumUHxZona ( r )*SiUniH
                u = UniHxZona ( iniciou )
                if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 ) then
!                   si se desea considerar bandas prohibidas y la unidad las tiene
                    if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
!                       si la unidad oferta reserva de regulacion secundaria
                        if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!                           para los rangos de la unidad
                            do ro = 1, NoRaOpH ( u )
!                               coeficiente de variable de asignacion de reserva en rango operativo
                                aaMILP ( k ) = 1.0
!                               columna asociada
                                jcolMILP( k ) = INVREROH + IREROH ( u, i ) + ro - 1
                                k = k + 1
                            enddo
                        endif
                    else
!                       si la unidad oferta reserva de regulacion secundaria
                        if ( CalOferResRegH ( u, i ) .gt. 0.0 ) then
!                           coeficiente de variable de asignacion de reserva
                            aaMILP ( k ) = 1.0
!                           columna asociada
                            jcolMILP( k ) = IVREGH + INREGH ( u, i ) - 1
                            k = k + 1
                        endif
                    endif
                endif
                iniciou = iniciou + 1
            enddo
!           lados derechos de las restriciones
            m = m + 1
!            bMILP ( m ) = NminRegZ ( r )
            bMILP ( m ) = NminRegZ ( r, i )
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Numero minimo de unidades para dar reservan de reg sec por zona '//trim(NomZonaRes(r))//' intervalo '//trim(leti)//'"'
        endif
    enddo
enddo

! si se permite hacer uso de reserva rodante
if ( TipoEjecu .le. 1 ) then
    
    write ( 777,* ) 'Inicia restricciones de maximo uso de la reserva rodante por U RC:', m + 1
!   unidades de rango continuo
    do u = 1 , NumUniRC
!       Para todos los intervalos
        do i = 1 , NTINTR
!           coeficiente de variable de uso
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IREUSO10 + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de reserva rodante de 10 min
            aaMILP ( k  ) = -1.0*SiRelRod
            jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Limite maximo uso de la reserva rodante por U RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        enddo
    enddo

endif

! si se permite hacer uso de reserva de regulacion
if ( TipoEjecu .le. 1 ) then

    write ( 777,* ) 'Inicia restricciones de maximo uso de la reserva de regu por U RC:', m + 1
!   unidades de rango continuo
    do u = 1 , NumUniRC
!       Para todos los intervalos
        do i = 1 , NTINTR
!           coeficiente de variable de uso
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IREUSORE + u + (i-1)*NumUniRC - 1
            k = k + 1
!           coeficiente de la variable de reserva de regulacion
            aaMILP ( k  ) = -1.0*SiRelReg
            jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
            k = k + 1
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Limite maximo uso de la reserva de regu por U RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
        enddo
    enddo

    write ( 777,* ) 'Inicia restricciones de maximo uso de la reserva de regu por U HI:', m + 1
!   unidades hidro
    do u = 1 , NumUniHid
!       Para todos los intervalos
        do i = 1 , NTINTR
!           coeficiente de variable de uso
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IREUSOREH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           coeficiente de la variable de reserva de regulacion
            aaMILP ( k  ) = -1.0*SiRelReg
            jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
            k = k + 1
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m,',', '"Limite maximo uso de la reserva de regu por U HI: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
        enddo
    enddo

endif


200 format (i3)
    
return
end
    
    
Subroutine ReqResSis ( k, m )
! ---------------------------------------------------------------------
! Restricciones de requerimiento del CENACE de reserva por sistema,   *
! en el problema de asignacion (MILP).                                *
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

INTEGER   i, k, kr10, k10, ks, kr, m, r, s, u, modo, d, l
character*3 leti

kr10 = ICARR10S
k10 = ICAR10S
ks = ICARSS
kr = ICARRS
l = 0
! restricciones de requerimiento del CENACE de reserva por sistema
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE R 10 min por sis:', m + 1
IRR10S = m + 1
! para todos los sistemas (islas)
do r = 1, numsis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        l = l + 1
!       para todos los intervalos
        do i = 1, NTINTR
!           si existe requerimiento de reserva rodante de 10 min
            if ( ReqResR10S (l, 1, i) .gt. 0.0 ) then
!               para las unidades de rango continuo que estan en el sistema 
                do u = 1 , NumUniRC
!                   si es unidad disponible y no condensador sincrono en este periodo
                    if ( IslaGenRC ( u ) .eq. r .and. DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                       coeficiente de la variable de reserva de regulacion
!                        aaMILP ( k  ) = 1.0
!                        jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
!                        k = k + 1
!                       coeficiente de la variable de reserva rodante de 10 min
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
                enddo
!               para las unidades de rango discontinuo que estan en el sistema 
                do u = 1 , NumUniRD
!                   si la unidad esta en el sistema
                    if ( IslaGenRD ( u ) .eq. r ) then
!                       para los modos excepto el apagado
                        do modo = 2, NumModRD ( u )
!                           si es unidad disponible y cordinable en este periodo y modo
!                            if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                           si es unidad disponible en este periodo
                            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                               coeficiente de la variable de regulacion
!                                aaMILP ( k  ) = 1.0
!                                jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
!                                k = k + 1
!                               coeficiente de la variable de reserva rodante de 10 min
                                aaMILP ( k  ) = 1.0
                                jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                                k = k + 1
                            endif
                        enddo
                    endif
                enddo
!               para las unidades hidro que estan en el sistema 
                do u = 1 , NumUniHid
!                   si es unidad disponible y no condensador sincrono con bandera 1 en este periodo
                    if ( IslaGenH ( u ) .eq. r .and. DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .ne. 1 ) then
!                       coeficiente de la variable de reserva
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                        k = k + 1
                    endif
!                    if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 ) then
!                       coeficiente de la variable de reserva de regulacion
!                        aaMILP ( k  ) = 1.0
!                        jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
!                        k = k + 1
!                    endif
                enddo
!               para los requerimientos de reserva rodante de 10 minutos
                do s = 1, NumBloRR10( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr10
                    k = k + 1
                    ubMILP ( kr10 ) = ReqResR10S (l, s, i)
                    kr10 = kr10 + 1
                enddo
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva rodante 10 min por sistema'//' intervalo '//trim(leti)//'"'
            else
!               para los requerimientos de reserva rodante de 10 minutos
                do s = 1, NumBloRR10( i )
!                   cota superior de reserva aceptada por CENACE
                    ubMILP ( kr10 ) = 0.0
                    kr10 = kr10 + 1
                enddo                
            endif
        enddo
    endif
enddo

kr10 = ICARR10S
l = 0
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE 10 min por siste:', m + 1
IR10S= m + 1
! para todos los sistemas (islas)
do r = 1, numsis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        l = l + 1
!       para todos los intervalos
        do i = 1, NTINTR
!           si existe requerimiento de reserva de 10 min
            if ( ReqRes10S (l, 1, i) .gt. 0.0 ) then
!               para las unidades de rango continuo que estan en el sistema 
                do u = 1 , NumUniRC
!                   si la unidad pertenece al sistema
                    if ( IslaGenRC ( u ) .eq. r ) then
!                       si es unidad disponible y no condensador sincrono en este periodo
                        if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                           coeficiente de la variable de reserva de regulacion
!                            aaMILP ( k  ) = 1.0
!                            jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
!                            k = k + 1
!                           coeficiente de la variable de reserva rodante de 10 min
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
!                       si es unidad disponible, coordinable y asignable en este periodo
                        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u, i ) .eq. 1 .and. AsignURC ( u, i ) .eq. 1 ) then
!                           coeficiente de la variable de reserva no rodante
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
                    endif
                enddo
!               para las unidades de rango discontinuo que estan en el sistema 
                do u = 1 , NumUniRD
!                   si la unidad pertenece al sistema
                    if ( IslaGenRD ( u ) .eq. r ) then
!                       para los modos excepto el apagado
                        do modo = 2, NumModRD ( u )
!                           si es unidad disponible y cordinable en este periodo y modo
!                            if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                           si es unidad disponible en este periodo
                            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                               coeficiente de la variable de regulacion
!                                aaMILP ( k  ) = 1.0
!                                jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
!                                k = k + 1
!                               coeficiente de la variable de reserva rodante de 10 min
                                aaMILP ( k  ) = 1.0
                                jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                                k = k + 1
                            endif
                        enddo
!                       para los modos arrancables
                        do modo = 2, NumModRD ( u )
                            if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!                               si es unidad disponible, asignable  y cordinable en este periodo
                                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
!                                   coeficiente de la variable de reserva no rodante
                                    aaMILP ( k  ) = 1.0
                                    jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                                    k = k + 1
                                endif
                            endif
                        enddo
                    endif
                enddo
!               para las unidades hidro que estan en el sistema 
                do u = 1 , NumUniHid
!                   si la unidad pertenece al sistema
                    if ( IslaGenH ( u ) .eq. r ) then
!                       si es unidad disponible y no condensador sincrono con bandera 1 en este periodo
                        if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .ne. 1 ) then
!                           coeficiente de la variable de reserva rodante
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                            k = k + 1
                        endif
!                        if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 ) then
!                           coeficiente de la variable de reserva de regulacion
!                            aaMILP ( k  ) = 1.0
!                            jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
!                            k = k + 1
!                        endif
!                       si es unidad disponible, coordinable y asignable en este periodo
                        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u, i ) .eq. 1 .and. AsignUH ( u, i ) .eq. 1 ) then
!                           coeficiente de la variable de reserva no rodante
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
                            k = k + 1
                        endif
                    endif
                enddo
!               para las demandas controlables de 10 minutos que estan en el sistema 
                do d = 1 , NumOferDem*0
!                   si la demanda pertenece al sistema
                    if ( IslaDem ( d ) .eq. r ) then
!                       coeficiente de la variable de demanda controlable de 10
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = ICC10 + d + (i-1)*NumOferDem - 1
                        k = k + 1
                    endif
                enddo

!               para los requerimientos de reserva rodante de 10 minutos
                do s = 1, NumBloRR10( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr10
                    k = k + 1
                    kr10 = kr10 + 1
                enddo
!               para los requerimientos de reserva de 10 minutos
                do s = 1, NumBloR10( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = k10
                    k = k + 1
                    ubMILP ( k10 ) = ReqRes10S (l, s, i)           
                    k10 = k10 + 1
                enddo
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva 10 min por sistema'//' intervalo '//trim(leti)//'"'
            else
!               para los requerimientos de reserva de 10 minutos
                do s = 1, NumBloR10( i )
!                   cota superior de reserva aceptada por CENACE
                    ubMILP ( k10 ) = 0.0
                    k10 = k10 + 1
                    kr10 = kr10 + 1
                enddo
            endif
        enddo
    endif
enddo

kr10 = ICARR10S
k10 = ICAR10S
l = 0
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE res sup por sist:', m + 1
IRSS = m + 1
! para todos los sistemas (islas)
do r = 1, numsis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        l = l + 1
!       para todos los intervalos
        do i = 1, NTINTR
!           si existe requerimiento de reserva suplementaria
            if ( ReqResSupS (l, 1, i) .gt. 0.0 ) then
!               para las unidades de rango continuo que estan en el sistema 
                do u = 1 , NumUniRC
!                   si la unidad pertenece al sistema
                    if ( IslaGenRC ( u ) .eq. r ) then
!                       si es unidad disponible y no condensador sincrono en este periodo
                        if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 ) then
!                           coeficiente de la variable de reserva de regulacion
!                            aaMILP ( k  ) = 1.0
!                            jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
!                            k = k + 1
!                           coeficiente de la variable de reserva rodante de 10
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRR10RC + u + (i-1)*NumUniRC - 1
                            k = k + 1
!                           coeficiente de la variable de reserva rodante suplementaria
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRRSRC + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
!                       si es unidad disponible, coordinable y asignable en este periodo
                        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u, i ) .eq. 1 .and. AsignURC ( u, i ) .eq. 1 ) then
!                           coeficiente de la variable de reserva no rodante de 10
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNR10RC + u + (i-1)*NumUniRC - 1
                            k = k + 1
!                           coeficiente de la variable de reserva no rodante suplementaria
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNRSRC + u + (i-1)*NumUniRC - 1
                            k = k + 1
                        endif
                    endif
                enddo
!               para las unidades de rango discontinuo que estan en el sistema 
                do u = 1 , NumUniRD
!                   si la unidad pertenece al sistema
                    if ( IslaGenRD ( u ) .eq. r ) then
!                       para los modos excepto el apagado
                        do modo = 2, NumModRD ( u )
!                           si es unidad disponible y cordinable en este periodo y modo
!                            if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                           si es unidad disponible en este periodo
                            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                               coeficiente de la variable de regulacion
!                                aaMILP ( k  ) = 1.0
!                                jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
!                                k = k + 1
!                               coeficiente de la variable de reserva rodante de 10
                                aaMILP ( k  ) = 1.0
                                jcolMILP ( k ) = IRR10RD + INIURDI ( u, i ) + modo - 1
                                k = k + 1
!                               coeficiente de la variable de reserva rodante suplementaria
                                aaMILP ( k  ) = 1.0
                                jcolMILP ( k ) = IRRSRD + INIURDI ( u, i ) + modo - 1
                                k = k + 1
                            endif
                        enddo
!                       para los modos arrancables
                        do modo = 2, NumModRD ( u )
                            if ( TransFacti ( u, 1, modo ) .gt. 0 ) then
!                               si es unidad disponible, asignable  y cordinable en este periodo
                                if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 .and. AsignURD ( u, modo, i ) .eq. 1 ) then
!                                   coeficiente de la variable de reserva no rodante de 10
                                    aaMILP ( k  ) = 1.0
                                    jcolMILP ( k ) = IRNR10RD + INIURDI ( u, i ) + modo - 1
                                    k = k + 1
!                                   coeficiente de la variable de reserva no rodante suplementaria
                                    aaMILP ( k  ) = 1.0
                                    jcolMILP ( k ) = IRNRSRD + INIURDI ( u, i ) + modo - 1
                                    k = k + 1
                                endif
                            endif
                        enddo
                    endif
                enddo
!               para las unidades hidro que estan en el sistema 
                do u = 1 , NumUniHid
!                   si la unidad pertenece al sistema
                    if ( IslaGenH ( u ) .eq. r ) then
!                       si es unidad disponible y no condensador sincrono con bandera 1 en este periodo
                        if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .ne. 1 ) then
!                           coeficiente de la variable de reserva rodante de 10
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRR10H + u + (i-1)*NumUniHid - 1
                            k = k + 1
!                           coeficiente de la variable de reserva rodante suplementaria
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRRSH + u + (i-1)*NumUniHid - 1
                            k = k + 1
                        endif
!                        if ( DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 ) then
!                           coeficiente de la variable de reserva de regulacion
!                            aaMILP ( k  ) = 1.0
!                            jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
!                            k = k + 1
!                        endif
!                       si es unidad disponible, coordinable y asignable en este periodo
                        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u, i ) .eq. 1 .and. AsignUH ( u, i ) .eq. 1 ) then
!                           coeficiente de la variable de reserva no rodante de 10
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNR10H + u + (i-1)*NumUniHid - 1
                            k = k + 1
!                           coeficiente de la variable de reserva no rodante suplementaria
                            aaMILP ( k  ) = 1.0
                            jcolMILP ( k ) = IRNRSH + u + (i-1)*NumUniHid - 1
                            k = k + 1
                        endif
                    endif
                enddo
!               para las demandas controlables de 10 minutos y suplementaria que estan en el sistema 
                do d = 1 , NumOferDem*0
!                   si la demanda pertenece al sistema
                    if ( IslaDem ( d ) .eq. r ) then
!                       coeficiente de la variable de demanda controlable de 10
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = ICC10 + d + (i-1)*NumOferDem - 1
                        k = k + 1
!                       coeficiente de la variable de demanda controlable suplementaria
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = ICCS + d + (i-1)*NumOferDem - 1
                        k = k + 1
                    endif
                enddo

!               para los requerimientos de reserva rodante de 10 minutos
                do s = 1, NumBloRR10( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr10
                    k = k + 1
                    kr10 = kr10 + 1
                enddo
!               para los requerimientos de reserva de 10 minutos
                do s = 1, NumBloR10( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = k10
                    k = k + 1
                    k10 = k10 + 1
                enddo
!               para los requerimientos de reserva suplementaria
                do s = 1, NumBloRSu( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = ks
                    k = k + 1
                    ubMILP ( ks ) = ReqResSupS (l, s, i)
                    ks = ks + 1
                enddo
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva suplementaria por sistema'//' intervalo '//trim(leti)//'"'
            else
!               para los requerimientos de reserva suplementaria
                do s = 1, NumBloRSu( i )
!                   cota superior de reserva aceptada por CENACE
                    ubMILP ( ks ) = 0.0
                    ks = ks + 1
                    k10 = k10 + 1
                    kr10 = kr10 + 1
                enddo
            endif
        enddo
    endif
enddo

l = 0
write ( 777,* ) 'Inicia restricciones de requerimiento del CENACE regul secun sist:', m + 1
IRES = m + 1
! para todos los sistemas (islas)
do r = 1, numsis
!   si el sistema esta activo
    if ( EstadoIsla ( r ) .eq. 1 ) then
        l = l + 1
!       para todos los intervalos
        do i = 1, NTINTR
!           si existe requerimiento de reserva de regulacion
            if ( ReqResRegS (l, 1, i) .gt. 0.0 ) then
!               para las unidades de rango continuo que estan en el sistema 
                do u = 1 , NumUniRC
!                   si es unidad disponible y no condensador sincrono en este periodo
                    if ( IslaGenRC ( u ) .eq. r .and. DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0  ) then
!                       coeficiente de la variable de reserva
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRRERC + u + (i-1)*NumUniRC - 1
                        k = k + 1
                    endif
                enddo
!               para las unidades de rango discontinuo que estan en el sistema
                do u = 1 , NumUniRD
!                   si la unidad esta en el sistema
                    if ( IslaGenRD ( u ) .eq. r ) then
!                       para los modos excepto el apagado
                        do modo = 2, NumModRD ( u )
!                           si es unidad disponible y cordinable en este periodo y modo
!                            if ( DispoURD ( u, modo, i ) .eq. 1 .and. CoordURD ( u, modo, i ) .eq. 1 ) then
!                           si es unidad disponible en este periodo
                            if ( DispoURD ( u , modo, i ) .eq. 1 ) then
!                               coeficiente de la variable de reserva de regulacion
                                aaMILP ( k  ) = 1.0
                                jcolMILP ( k ) = IRRERD + INIURDI ( u, i ) + modo - 1
                                k = k + 1
                            endif
                        enddo
                    endif
                enddo
!               para las unidades hidro que estan en el sistema 
                do u = 1 , NumUniHid
!                   si es unidad disponible y no condensador sincrono en este periodo
                    if ( IslaGenH ( u ) .eq. r .and. DispoUH ( u , i ) .eq. 1 .and. CompSincH ( u, i ) .eq. 0 ) then
!                       coeficiente de la variable de reserva
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IRREH + u + (i-1)*NumUniHid - 1
                        k = k + 1
                    endif
                enddo
!               para los requerimientos de reserva de regulacion secundaria
                do s = 1, NumBloRReg( i )
!                   coeficiente de la variable de reserva aceptada por CENACE
                    aaMILP ( k  ) = -1.0
                    jcolMILP ( k ) = kr
                    k = k + 1
                    ubMILP ( kr ) = ReqResRegS (l, s, i)
                    kr = kr + 1
                enddo
!               lados derechos de las restriciones
                m = m + 1
                bMILP ( m ) = 0.0
!               sentidos de las restriciones
                sMILP ( m ) = 'G'
!               apuntador al siguiente renglon
                irowMILP ( m + 1 ) = k
                write ( leti, 200 ) i
                write ( 779,* ) m, ',', '"Requerimiento del CENACE reserva regulaci�n secundaria por sistema'//' intervalo '//trim(leti)//'"'
            else
!               para los requerimientos de reserva de regulacion
                do s = 1, NumBloRReg( i )
!                   cota superior de reserva aceptada por CENACE
                    ubMILP ( kr ) = 0.0
                    kr = kr + 1
                enddo
            endif
        enddo
    endif
enddo

100 continue

200 format (i3)
    
return
end
    

subroutine LimEnerTer ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de energia a unidades termo      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2018                                                        *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer dia, k,  intervalo, IntIni, m, u, grupo, iniciou, modo, unidad
character*3 letr

IRGPTER = m + 1

write ( 777,* ) 'Inicia restricciones de limite maximo de energia en unidades term:', m + 1

! para todos los grupos con limitacion de energia
do grupo = 1, NumGruUTer
    IntIni = 1
!   para todos los dias del horizonte
    do dia = 1, DURDIA
!       para todos los intervalos del dia
        do intervalo = IntIni, IntIni + intdia (dia) - 1
!           para las unidades de rango continuo que estan en ese grupo
            iniciou = ApunURCxGrupo ( grupo )
            do unidad = 1 , NumURCxGrupo ( grupo )
                u = UniRCxGrupo ( iniciou )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniciou = ApunURDxGrupo ( grupo )
            do unidad = 1 , NumURDxGrupo ( grupo )
                u = UniRDxGrupo ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible 
                    if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
        enddo
!       coeficiente de la variable artificial de energia
        aaMILP ( k ) = - 1.0
!       columna asociada
        jcolMILP( k ) = IARGT + NumGruUTer*(dia-1) + grupo - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LimEnerSUTermo ( grupo, dia )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( letr, 200 ) dia
        write ( 779,* ) m, ',', '"Limite maximo de energia en unidades term, grupo '//trim(NomGpoGas(grupo))//' dia: '//trim(letr)//'"'
        IntIni = IntIni + intdia ( dia )
    enddo
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de energia en unidades term:', m + 1

! para todos los grupos con limitacion de energia
do grupo = 1, NumGruUTer
    IntIni = 1
!   para todos los dias del horizonte
    do dia = 1, DURDIA
!       para todos los intervalos del dia
        do intervalo = IntIni, IntIni + intdia (dia) - 1
!           para las unidades de rango continuo que estan en ese grupo
            iniciou = ApunURCxGrupo ( grupo )
            do unidad = 1 , NumURCxGrupo ( grupo )
                u = UniRCxGrupo ( iniciou )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniciou = ApunURDxGrupo ( grupo )
            do unidad = 1 , NumURDxGrupo ( grupo )
                u = UniRDxGrupo ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible 
                    if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
        enddo
!       coeficiente de la variable artificial de energia
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IARGT + NumGruUTer*(dia-1) + grupo - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LimEnerIUTermo ( grupo, dia )
!       sentidos de las restriciones
        sMILP ( m ) = 'G'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( letr, 200 ) dia
        write ( 779,* ) m, ',', '"Limite minimo de energia en unidades term, grupo '//trim(NomGpoGas(grupo))//' dia: '//trim(letr)//'"'    
        IntIni = IntIni + intdia ( dia )
    enddo
enddo

200 format (i3)
    
return
end    


subroutine LimEnerTer_new ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de energia a unidades termo      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2018                                                        *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer k,  intervalo, m, u, grupo, iniciou, modo, unidad
character*3 letr

IRGPTER = m + 1

write ( 777,* ) 'Inicia restricciones de limite maximo de energia en unidades term:', m + 1

! para todos los grupos con limitacion de energia
do grupo = 1, NResEner
!   si el grupo esta activo
    if ( ActResEner ( grupo ) .eq. 1 ) then
!       para todos los intervalos del grupo
        do intervalo = HIResEner(grupo), HFResEner(grupo)
!           para las unidades de rango continuo que estan en ese grupo
            iniciou = ApunURCxGrupo ( grupo )
            do unidad = 1 , NumURCxGrupo ( grupo )
                u = UniRCxGrupo ( iniciou )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniciou = ApunURDxGrupo ( grupo )
            do unidad = 1 , NumURDxGrupo ( grupo )
                u = UniRDxGrupo ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible 
                    if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
        enddo
!       coeficiente de la variable artificial de energia
        aaMILP ( k ) = - 1.0
!       columna asociada
        jcolMILP( k ) = IARGT + grupo - 1
        k = k + 1
!       cota superior
        ubMILP( IARGT + grupo - 1 ) = LSupResEner ( grupo )
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LSupResEner ( grupo )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( letr, 200 ) grupo
        write ( 779,* ) m, ',', '"Limite maximo de energia en unidades term, grupo '//trim(NomGpoResEner(grupo))//' rest: '//trim(letr)//'"'
    endif
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de energia en unidades term:', m + 1

! para todos los grupos con limitacion de energia
do grupo = 1, NResEner
!   si el grupo esta activo
    if ( ActResEner ( grupo ) .eq. 1 ) then
!       para todos los intervalos del grupo
        do intervalo = HIResEner(grupo), HFResEner(grupo)
!           para las unidades de rango continuo que estan en ese grupo
            iniciou = ApunURCxGrupo ( grupo )
            do unidad = 1 , NumURCxGrupo ( grupo )
                u = UniRCxGrupo ( iniciou )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniciou = ApunURDxGrupo ( grupo )
            do unidad = 1 , NumURDxGrupo ( grupo )
                u = UniRDxGrupo ( iniciou )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible 
                    if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k  ) = 1.0
                        jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
        enddo
!       coeficiente de la variable artificial de energia
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IARGT + grupo - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LInfResEner ( grupo )
!       sentidos de las restriciones
        sMILP ( m ) = 'G'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( letr, 200 ) grupo
        write ( 779,* ) m, ',', '"Limite minimo de energia en unidades term, grupo '//trim(NomGpoResEner(grupo))//' rest: '//trim(letr)//'"'
    endif
enddo

200 format (i3)
    
return
end subroutine LimEnerTer_new

    
subroutine RestPerdidas ( sistema, m )
! ---------------------------------------------------------------------
! Se forman nuevas restricciones para la representacion de perdidas   *
! en el problema  MILP.                                               *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2015                                                       *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use symtypes

use ParGloRed, only: NumNodSis, SnsPerNod, InAumNodSis, SnsPerIntIny, PerIntervalo

!use cplex_ifaces, only: CPXwriteprob    !Windows (comentarizar para Linux)
!use cplex_forman, only: addrowsCPX      !Windows (comentarizar para Linux)


implicit none
integer                 CPXwriteprob, CPXaddrows    !Linux (comentarizar para Windows)

Integer i, isla, d, k, l, m, modo, n, nodo, u, sistema
integer componente, componente_1, s, status, sis, NoDi
real*8  coeficiente

character*1 ssistema, AsMILP
CHARACTER fecha_Ej*19
integer   ibanbit, ierror

integer cnt, AjcolMILP, AirowMILP
real*8  AaaMILP, AbMILP
character*3 leti

character*1 name_array( maxresMILP )   !Linux (comentarizar para Windows)
integer     name( maxresMILP )         !Linux (comentarizar para Windows)

! coeficientes de las restricciones de perdidas en el problema de asignacion (MILP)
dimension  AaaMILP    ( maxresMILP*20 )
! indicador de la variable asociada a cada coeficiente de las restricciones de perdidas
dimension  AjcolMILP  ( maxresMILP*20 )
! lados derechos de las restricciones de perdidas
dimension  AbMILP     ( maxresMILP )
! sentidos de las restricciones de perdidas
dimension  AsMILP     ( maxresMILP )
! indicador en el vector AaaMILP de la restriccion de balance en el problema de asignacion (MILP)
dimension  AirowMILP  ( maxresMILP )


write ( 777,* ) 'Inicia restricciones de estimacion de perdidas en transmision    :', m + 1

ibanbit = 1
ierror = 0
AirowMILP ( 1 ) = 0
k = 1
cnt = 1
sis = 0
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
                    AaaMILP ( k ) = coeficiente
                    AjcolMILP ( k ) = IGRC + u + (i-1)*NumUniRC - 2
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
                        AaaMILP ( k ) = -coeficiente
                        AjcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 2
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
                    AaaMILP ( k ) = -SnsPerNod(nodo,i)
                    AjcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 2
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
                    AaaMILP ( k ) = -SnsPerNod(nodo,i)
                    AjcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 2
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
                        AaaMILP ( k ) = coeficiente
!                       columna asociada
                        AjcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 2
                        k = k + 1
                    enddo
!                   coeficiente de corte de demanda fija
                    AaaMILP ( k ) = -coeficiente
!                   columna asociada
                    AjcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 2
                    k = k + 1
                endif
            enddo
!           para todos los nodos
            do n = 1, NumNodSis ( l )
                nodo = InAumNodSis(n)
!               coeficiente de variable de excedente
                AaaMILP ( k ) = SnsPerNod(nodo,i)
                AjcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 2
                k = k + 1
            enddo
!           variable de perdidas
!           coeficiente de la variable
            AaaMILP ( k ) = 1.0 
            AjcolMILP ( k ) = IPERD + i + (sis-1)*numsis_act - 2
            k = k + 1
!           lados derechos de las restriciones
            AbMILP ( cnt )   = PerIntervalo(i) - SnsPerIntIny(i)
!           sentidos de las restriciones
            AsMILP ( cnt ) = 'G'
!           apuntador al siguiente renglon
            AirowMILP ( cnt + 1 ) = k - 1
            cnt = cnt + 1
            m = m + 1
            write (leti, 200 ) i
            write ( 779,* ) m, ',', '"Estimacion de perdidas en transmision en intervalo: '//trim(leti)//'"'
        enddo
    endif
enddo

do i = 1, NumResAsig                        !Linux (comentarizar para Windows)
   name_array(i) = name_array(i)//char(0)   !Linux (comentarizar para Windows)
   name(i) = loc( name_array(i) )           !Linux (comentarizar para Windows)
end do                                      !Linux (comentarizar para Windows)

! Se agrega informacion de restricciones (por renglon)
status = CPXaddrows (enb, lpMILP, 0, cnt-1, k-1, AbMILP, AsMILP,  &   !Linux (comentarizar para Windows)
                     AirowMILP, AjcolMILP, AaaMILP, name, name )      !Linux (comentarizar para Windows)
!status = addrowsCPX (enb, lpMILP, 0, cnt-1,  k-1, AbMILP, AsMILP, &  !Windows (comentarizar para Linux)
!                     AirowMILP, AjcolMILP, AaaMILP )                 !Windows (comentarizar para Linux)
if ( status .ne. 0 ) then
    write (*,*) ' Error al agregar las restricciones al MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

write( ssistema, '(I1)' )  sistema
!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'mp'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'mP'//ssistema//'.sav', 'SAV')

200 format (i3)
    
return
end


    
subroutine AddLimFluRes ( sistema, m )
! ---------------------------------------------------------------------
! Se forman nuevas restricciones de grupos de ramas violadas en el    *
! problema  MILP.                                                     *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2018                                                     *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use symtypes

use ParGloRed, only: NumNodSis, SnsGruRarInyNod, InAumNodSis, bangruram, &
                     CoeSnsGruRar, NumGruRamSis, potmaxgruram, potmingruram, &
                     inagruram, bangruramcopy, RamActiva, nomgruram

!use cplex_ifaces, only: CPXwriteprob    !Windows (comentarizar para Linux)
!use cplex_forman, only: addrowsCPX      !Windows (comentarizar para Linux)

implicit none

integer                 CPXwriteprob, CPXaddrows    !Linux (comentarizar para Windows)

Integer i, d, k, l, m, modo, n, nodo, u, br, r, s
integer componente, componente_1, status, sistema, NoDi
real*8  coeficiente

character*1 ssistema, AsMILP
CHARACTER fecha_Ej*19
integer   ibanbit, ierror

integer cnt, AjcolMILP, AirowMILP
real*8  AaaMILP, AbMILP

character*3 leti

character*1 name_array( maxresMILP )   !Linux (comentarizar para Windows)
integer     name( maxresMILP )         !Linux (comentarizar para Windows)

! coeficientes de las restricciones de perdidas en el problema de asignacion (MILP)
dimension  AaaMILP    ( maxresMILP*30 )
! indicador de la variable asociada a cada coeficiente de las restricciones de perdidas
dimension  AjcolMILP  ( maxresMILP*30 )
! lados derechos de las restricciones de perdidas
dimension  AbMILP     ( maxresMILP )
! sentidos de las restricciones de perdidas
dimension  AsMILP     ( maxresMILP )
! indicador en el vector AaaMILP de la restriccion de balance en el problema de asignacion (MILP)
dimension  AirowMILP  ( maxresMILP )

ibanbit = 1
ierror = 0
AirowMILP ( 1 ) = 0
k = 1
cnt = 1

! para todos los subsistemas (islas)
do l = 1, numsis
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
!       Para todos los grupos de ramas en el sistema
        do r = 1, NumGruRamSis ( l )
            if ( bangruram(r) .eq. 1 .and. bangruramcopy(r) .eq. 1 ) then
                RamActiva ( r ) = bangruram ( r )
            endif
        enddo
    endif
enddo

write ( 777,* ) 'Inicia restricciones de limite superior en grupos de ramas restri:', m + 1

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
                if ( bangruram(r) .eq. 1 .and. bangruramcopy(r) .eq. 1 ) then
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
                        AaaMILP ( k ) = coeficiente
                        AjcolMILP ( k ) = IGRC + u + (i-1)*NumUniRC - 2
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
                            AaaMILP ( k ) = coeficiente
                            AjcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 2
                            k = k + 1
                        enddo
                    enddo
!                   coeficientes de variables de generacion hidro
                    do u = 1 , NumUniHid
                        nodo = nodoh ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoUH ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            AaaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            AjcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 2
                            k = k + 1
                        endif
                    enddo
!                   coeficientes de variables de generacion renovables
                    do u = 1 , NumUniRE
                        nodo = nodounre ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURE ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            AaaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            AjcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 2
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
                            AaaMILP ( k ) = -coeficiente
!                           columna asociada
                            AjcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 2
                            k = k + 1
                        enddo
!                       coeficiente de corte de demanda fija
                        AaaMILP ( k ) = coeficiente
!                       columna asociada
                        AjcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 2
                        k = k + 1
                    enddo
!                   para todos los nodos
                    do n = 1, NumNodSis ( l )
                        nodo = inaumnodsis ( n )
                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
!                           coeficiente de variable de excedente
                            AaaMILP ( k ) = -SnsGruRarInyNod ( br, nodo )
                            AjcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 2
                            k = k + 1
                        endif
                    enddo
!                   variable artificial de excedente de flujo en grupos de ramas
                    AaaMILP ( k ) = -1.0
                    AjcolMILP ( k ) = IAEF + r + (i-1)*NumGruRamSis ( l ) - 2
                    k = k + 1
!                   variable artificial de deficit en grupos de ramas
                    AaaMILP ( k ) = 1.0
                    AjcolMILP ( k ) = IACF + r + (i-1)*NumGruRamSis ( l ) - 2
                    k = k + 1
!                   lados derechos de las restriciones
                    AbMILP ( cnt )   = potmaxgruram ( br, i ) + CoeSnsGruRar ( br, i )
!                   sentidos de las restriciones
                    AsMILP ( cnt ) = 'L'
!                   apuntador al siguiente renglon
                    AirowMILP ( cnt + 1 ) = k - 1
                    cnt = cnt + 1
                    m = m + 1
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
            call CalculaSensibilidadesFlujos ( l, i, 1, 1 )
!           Para todos los grupos de ramas en el sistema
            do r = 1, NumGruRamSis ( l )
                if ( bangruram(r) .eq. 1 .and. bangruramcopy(r) .eq. 1 ) then
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
                        AaaMILP ( k ) = coeficiente
                        AjcolMILP ( k ) = IGRC + u + (i-1)*NumUniRC - 2
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
                            AaaMILP ( k ) = coeficiente
                            AjcolMILP ( k ) = IGRD + INIURDI ( u, i ) + modo - 2
                            k = k + 1
                        enddo
                    enddo
!                   coeficientes de variables de generacion hidro
                    do u = 1 , NumUniHid
                        nodo = nodoh ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoUH ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            AaaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            AjcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 2
                            k = k + 1
                        endif
                    enddo
!                   coeficientes de variables de generacion renovables
                    do u = 1 , NumUniRE
                        nodo = nodounre ( u, i )
	                    if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 .and. DispoURE ( u, i ) .ne. 0  ) then
!                           coeficiente de generacion
                            AaaMILP ( k ) = SnsGruRarInyNod ( br, nodo )
                            AjcolMILP ( k ) = IGRE + u + (i-1)*NumUniRE - 2
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
                            AaaMILP ( k ) = -coeficiente
!                           columna asociada
                            AjcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 2
                            k = k + 1
                        enddo
!                       coeficiente de corte de demanda fija
                        AaaMILP ( k ) = coeficiente
!                       columna asociada
                        AjcolMILP( k ) = IDF + d + (i-1)*NumOferDem - 2
                        k = k + 1
                    enddo
!                   para todos los nodos
                    do n = 1, NumNodSis ( l )
                        nodo = inaumnodsis ( n )
                        if ( abs(SnsGruRarInyNod ( br, nodo )) .gt. 1e-10 ) then
!                           coeficiente de variable de excedente
                            AaaMILP ( k ) = -SnsGruRarInyNod ( br, nodo )
                            AjcolMILP ( k ) = IEXC + n + (i-1)*NumNodSis ( l ) - 2
                            k = k + 1
                        endif
                    enddo
!                   variable artificial de excedente de flujo en grupos de ramas
                    AaaMILP ( k ) = -1.0
                    AjcolMILP ( k ) = IAEF + r + (i-1)*NumGruRamSis ( l ) - 2
                    k = k + 1
!                   variable artificial de deficit en grupos de ramas
                    AaaMILP ( k ) = 1.0
                    AjcolMILP ( k ) = IACF + r + (i-1)*NumGruRamSis ( l ) - 2
                    k = k + 1
!                   lados derechos de las restriciones
                    AbMILP ( cnt )   = potmingruram ( br, i ) + CoeSnsGruRar ( br, i )
!                   sentidos de las restriciones
                    AsMILP ( cnt ) = 'G'
!                   apuntador al siguiente renglon
                    AirowMILP ( cnt + 1 ) = k - 1
                    cnt = cnt + 1
                    m = m + 1
                    write ( leti, 200 ) i
                    write ( 779,* ) m, ',', '"Limite inferior en grupos de ramas restri::'//trim(nomgruram(br))//' intervalo '//trim(leti)//'"'
                endif
            enddo
        enddo
    endif
enddo

do i = 1, NumResAsig                        !Linux (comentarizar para Windows)
   name_array(i) = name_array(i)//char(0)   !Linux (comentarizar para Windows)
   name(i) = loc( name_array(i) )           !Linux (comentarizar para Windows)
end do                                      !Linux (comentarizar para Windows)

! Se agrega informacion de restricciones (por renglon)
status = CPXaddrows (enb, lpMILP, 0, cnt-1, k-1, AbMILP, AsMILP,  &   !Linux (comentarizar para Windows)
                     AirowMILP, AjcolMILP, AaaMILP, name, name )      !Linux (comentarizar para Windows)
!status = addrowsCPX (enb, lpMILP, 0, cnt-1,  k-1, AbMILP, AsMILP, &  !Windows (comentarizar para Linux)
!                     AirowMILP, AjcolMILP, AaaMILP )                 !Windows (comentarizar para Linux)
if ( status .ne. 0 ) then
    write (*,*) ' Error al agregar las restricciones al MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError    
!   algoritmo no termina bien
    call SalidaError
    stop
end if

write( ssistema, '(I1)' )  sistema
!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'mT'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'mT'//ssistema//'.sav', 'SAV')
!Se escribe el modelo actuaizado MILP a un archivo
if ( SiEscLP .eq. 1 .and. TipoEjecu .ne. 3 ) then
!   Se escribe el modelo actuaizado MILP a un archivo
    status = CPXwriteprob (enb, lpMILP, 'dirres/MDA.lp', 'LP')
!    status = CPXwriteprob (enb, lpMILP, 'MDA.sav', 'SAV')
endif

! se reinicializan las ramas violadas
bangruram = 0

200 format (i3)
    
return

end
    

subroutine FijaAsignacion
! ---------------------------------------------------------------------
! Se fija la signacion de las unidades en el modelo MILP              *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2015                                                       *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use symtypes

!use cplex_ifaces, only: CPXchgbds       !Windows (comentarizar para Linux)

implicit none

integer                 CPXchgbds       !Linux (comentarizar para Windows)
 
integer status, ibanbit, i, modo, u, cnt, indices


integer ierror

REAL*8     bd 
character*1 lu

CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )

data status  / 0 /

ibanbit = 1
cnt = 0    
! para todas las unidades de rango continuo
do u = 1, NumUniRC
!   para cada intervalo
    do i = 1, NTINTR
        cnt = cnt + 1
        indices ( cnt ) = IARC + u + (i-1)*NumUniRC - 2
!       si la unidad esta asignada
        if ( xMILP ( IARC + u + (i-1)*NumUniRC - 1 ) .gt. 0.9 ) then
            bd ( cnt ) = 1.0
            lu ( cnt ) = 'B'
        else
            bd ( cnt ) = 0.0
            lu ( cnt ) = 'U'
        endif
    enddo
enddo
! para todas las unidades de rango discontinuo
do u = 1, NumUniRD
!   para cada intervalo
    do i = 1, NTINTR
!       para todos los modos
        do modo = 1, NumModRD ( u )
            cnt = cnt + 1
            indices ( cnt ) = IARD + INIURDI ( u, i ) + modo - 2
!           si la unidad esta asignada
            if ( xMILP ( IARD + INIURDI ( u, i ) + modo - 1  ) .gt. 0.9 ) then
                bd ( cnt ) = 1.0
                lu ( cnt ) = 'B'
            else
                bd ( cnt ) = 0.0
                lu ( cnt ) = 'U'
            endif
        enddo
    enddo
enddo
! para todas las unidades hidro
do u = 1 , NumUniHid
!   Para todos los intervalos
    do i = 1 , NTINTR
        cnt = cnt + 1
        indices ( cnt ) = IAH + u + (i-1)*NumUniHid - 2 
!       si la unidad esta asignada
        if ( xMILP ( IAH + u + (i-1)*NumUniHid - 1  ) .gt. 0.9 ) then
            bd ( cnt ) = 1.0
            lu ( cnt ) = 'B'
        else
            bd ( cnt ) = 0.0
            lu ( cnt ) = 'U'
        endif
    enddo
enddo
! para todas las unidades renovables
do u = 1 , NumUniRE
!   Para todos los intervalos
    do i = 1 , NTINTR
        cnt = cnt + 1
        indices ( cnt ) = IARE + u + (i-1)*NumUniRE - 2 
!       si la unidad esta asignada
        if ( xMILP ( IARE + u + (i-1)*NumUniRE - 1  ) .gt. 0.9 ) then
            bd ( cnt ) = 1.0
            lu ( cnt ) = 'B'
        else
            bd ( cnt ) = 0.0
            lu ( cnt ) = 'U'
        endif
    enddo
enddo
! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, cnt, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

return
end
    
    
subroutine GenerPerd ( k , m )
! ---------------------------------------------------------------------
! Se forma la restriccion de balance de potencia y perdida.           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2015                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, isla, k, l, m, modo, u, s, sistema
integer kv, kvd, kvr
character*3 leti, lets

sistema = 0
write ( 777,* ) 'Inicia restricciones de balance de potencia y perdidas           :', m + 1

! para todos los subsistemas (islas)
do l = 1, numsis
!   si la isla esta activa
    if ( EstadoIsla(l) .eq. 1 ) then
        kv = IGABRC
        kvd = IGABRD
        kvr = IGABRE
        sistema = sistema + 1
!       para todos los intervalos       
        do i = 1 , NTINTR
!           coeficientes de variables de generacion de rango continuo
            do u = 1 , NumUniRC
                isla = IslaGenRC ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de la variable de generacion
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   si la unidad tiene tiempo de arranque
                    if ( TiempoArraURC ( u ) .gt. 0.0 ) then
!                       coeficiente de la variable de generacion durante arranque
                       aaMILP ( k ) = -1.0
!                       columna asociada
                       jcolMILP( k ) = IGDARC + u + (i-1)*NumUniRC - 1
                       k = k + 1
                    endif
!                   coeficiente de la variable de asignacion
                    aaMILP ( k ) = -PotMinGRC(u,i)
!                   columna asociada
                    jcolMILP( k ) = IARC + u + (i-1)*NumUniRC - 1
                    k = k + 1
!                   para todos los segmentos de curva de ofertas de venta
                    do s = 1, NumBloVRC( u, i )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = -1.0
!                       columna asociada
                        jcolMILP( k ) = kv
                        k = k + 1
                        kv = kv + 1
                    enddo
                endif
            enddo
!           coeficientes de variables de generacion de rango discontinuo
            do u = 1 , NumUniRD
                isla = IslaGenRD ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
                    kvd = kvd + 1
!                   para todos los modos de operacion
                    do modo = 2, NumModRD(u)
!                       para todos los segmentos de curva de ofertas de venta
                        do s = 1, NumBloVRD( u, modo, i )
!                           coeficiente de la variable de generacion en el segmento
                            aaMILP ( k ) = -1.0
!                           columna asociada
                            jcolMILP( k ) = kvd
                            k = k + 1
                            kvd = kvd + 1
                        enddo
!                       coeficiente de la variable de generacion
                        aaMILP ( k ) = 1.0
!                       columna asociada
                        jcolMILP( k ) = IGRD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
!                       si la unidad tiene tiempo de arranque en este modo
                        if ( TransFacti  ( u, 1, modo ) .gt. 0.0 ) then
!                           coeficiente de la variable de generacion durante arranque
                            aaMILP ( k ) = -1.0
!                           columna asociada
                            jcolMILP( k ) = IGDARD + INIURDI ( u, i ) + modo - 1
                            k = k + 1
                        endif
!                       coeficiente de la variable de asignacion
                        aaMILP ( k ) = -PotMinGRD( u, modo, i )
!                       columna asociada
                        jcolMILP( k ) = IARD + INIURDI ( u, i ) + modo - 1
                        k = k + 1
                    enddo
                endif
            enddo
!           coeficientes de variables de generacion hidro
            do u = 1 , NumUniHid
                isla = IslaGenH ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de generacion
                    aaMILP ( k ) = 1.0
                    jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
                    k = k + 1
                endif
            enddo
!           coeficientes de variables de generacion renovable
            do u = 1 , NumUniRE
                isla = IslaGenRE ( u )
!               si la unidad pertenece a la isla
                if ( isla.eq.l ) then
!                   coeficiente de la variable de generacion
                    aaMILP ( k ) = 1.0
!                   columna asociada
                    jcolMILP( k ) = IGRE + u + (i-1)*NumUniRE - 1
                    k = k + 1
!                   para todos los segmentos de curva de ofertas de venta
                    do s = 1, NumBloVRE( u, i )
!                       coeficiente de la variable de generacion aceptada en el bloque
                        aaMILP ( k ) = -1.0
!                       columna asociada
                        jcolMILP( k ) = kvr
                        k = k + 1
                        kvr = kvr + 1
                    enddo
                endif
            enddo
            m = m + 1
!           variable de perdidas
!           coeficiente de la variable
            aaMILP ( k ) = 1.0 
            jcolMILP ( k ) = IPERD + i + (sistema-1)*numsis_act - 1
            k = k + 1
!            ubMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = 0.0
            objMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = 1.0
!            objMILP ( IPERD + i + (sistema-1)*numsis_act - 1 ) = CostoExced
!           lados derechos de las restriciones
            bMILP ( m )   = 0.0
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( lets, 200 ) l
            write ( 779,* ) m, ',', '"Balance de energia por isla '//trim(lets)//' intervalo '//trim(leti)//'"'
        enddo        
    endif
enddo

200 format (i3)

return
end

    
subroutine ActualizaRestRama ( sistema )
! ---------------------------------------------------------------------
! Se actualiza lado derecho de restriccion de rama                    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2019                                                  *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: CoeSnsGruRar, potmaxgruram, potmingruram, &
                     NumGruRamSis, bangruram, RamActiva
use symtypes

!use cplex_ifaces, only: CPXchgrhs , CPXwriteprob      !Windows (comentarizar para Linux)

implicit none

integer                 CPXchgrhs , CPXwriteprob     !Linux (comentarizar para Windows)
 
integer k, m, status, ibanbit, i, r, cnt, indices, sistema, inicio, fin
integer ierror

REAL*8     values 
character*1 ssistema
CHARACTER fecha_Ej*19

DIMENSION   values  ( maxresMILP )
DIMENSION   indices ( maxvarMILP )

data status  / 0 /

ibanbit = 1
cnt = 0

! Si se emplea solucion inicial
if ( SiSolucionInicial .eq. 1 ) then
    inicio = IRPERD
    fin = inicio + NumResAdi - 1
else
    inicio = NumResAsig + 1
    fin = NumResAsig + NumResAdi
endif

! Actualiza termino constante
! para todos los intervalos       
do i = 1 , NTINTR
!   se calcuan sensibilidades
    call CalculaSensibilidadesFlujos ( sistema, i, 1, 1 )
enddo

! para todos los grupos de ramas
do r = 1, NumGruRamSis ( sistema )
    if ( bangruram ( r ) .eq. 0 .and. RamActiva ( r ) .eq. 1 ) then
        k = 0
!       se revisan las restricciones adicionales
        do m = inicio, fin
            k = k + 1
!           si la restriccion corresponde a esta rama ya violada antes
            if ( InfRestAdi ( k ) .eq. r ) then
                cnt = cnt + 1
                indices ( cnt ) = m - 1
                if ( IsenRestAdi ( k ) .eq. 'L' ) then
                    values ( cnt ) = potmaxgruram ( r, IntRestAdi ( k ) ) + CoeSnsGruRar ( r, IntRestAdi ( k ) )
                else if ( IsenRestAdi ( k ) .eq. 'G' ) then
                    values ( cnt ) = potmingruram ( r, IntRestAdi ( k ) ) + CoeSnsGruRar ( r, IntRestAdi ( k ) )
                endif
            endif
        enddo
    endif
enddo

! si hay elementos para actualizar
if ( cnt .gt. 0 ) then
!   se actualizan lados derechos de restricciones
    status = CPXchgrhs (enb, lpMILP, cnt, indices, values)
    if ( status .ne. 0) then
        write (*,*) ' Error al actualizar lados derechos de MILP'
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write(*,*) '1'
!       Se ecribe resultado de semaforos
        call EscSemaforosError
!       algoritmo no termina bien
        call SalidaError
        stop
    end if

    write( ssistema, '(I1)' )  sistema
!   Se escribe el modelo actuaizado MILP a un archivo
!    status = CPXwriteprob (enb, lpMILP, 'mt'//ssistema//'.lp', 'LP')
!   status = CPXwriteprob (enb, lpMILP, 'mt'//ssistema//'.sav', 'SAV')
endif

return
end


Subroutine Almacena_Hidro ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4 )
! ---------------------------------------------------------------------
! Se asigna la solucion la solucion del problema de asignacion (MILP) *
! a los arreglos internos de generacion hidro.                        *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2017                                                  *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only: qgasto, VolEmb, nomemb, nivini, namino, &
                        poliemb, local_uni_emb, numuni_x_embalse, &
                        unidad_x_embalse, WEmbFin, EFijEmb, TFijEmb

implicit none

integer    i, u, unidad, embalse, UniIni, IntIni, dia, IntFin, ierror, ibanbit
real*8     Deseado(nmxemb), Calculado(nmxemb), ALUTIL, aux

character*5  a1(nmxemb)
real*8       a2(nmxemb), a3(nmxemb), aa4(nmxemb, maxdia, 7)
character*11 a4(nmxemb), a5(nmxemb), a6(nmxemb)
character*9  blanco, solici, calcula
DATA blanco / '         ' /

real*8   volumhR ( maxuh, maxint ), cargahR ( maxuh, maxint ), &
         a_tra_res ( maxint + 25, nmxvia ), gplah ( nmxpla, maxint ), &
         qplah ( nmxpla, maxint )
integer  UniOnHid ( nmxpla, maxint )

CHARACTER fecha_Ej*19
character*15 aaux3
character*20 aaux2

ibanbit = 1
ierror = 0

aa4 = 0.0

! solucion de volumenes turbinados (gasto)
do u = 1 , NumuniHid
   do i = 1 , NTINTR
      qgasto ( u, i ) = xMILP ( ITURB + u + (i-1)*NumuniHid - 1 )
   enddo
enddo
! escribe solucion de volumenes turbinados (gasto)
write ( 95, * ) 'Solucion MILP'
IntIni = 1
do dia = 1 , DURDIA
   write ( 95, * )
   write ( 95, * ) 'Dia :', dia
   write ( 95, * )
!  Intervalo inicial del dia
!  Escribe solucion de turbinados
   do u = 1 , NumuniHid
!     para todos los intervalos de planeacion
      write ( 95, 600 ) u, nombunih(u), ( qgasto ( u, i )*3.6, i=IntIni,IntIni+intdia(dia)-1 )
   enddo
   write ( 95, * )
   IntIni = IntIni + intdia ( dia )
enddo


! solucion de volumenes almacenados
do embalse = 1, NumEmbalses
   do i = 1 , NTINTR
      VolEmb ( embalse, i ) = xMILP ( IVOLU + embalse + (i-1)*NumEmbalses - 1 )
   enddo
enddo

write ( 96, * ) 'Solucion MILP'
! escribe solucion de volumenes almacenados
write ( 96, * )
IntIni = 1
do dia = 1 , DURDIA
   write ( 96, * )
   write ( 96, * ) 'Dia :', dia
   write ( 96, * )
!  Intervalo inicial del dia
   IntIni = 1 + (dia-1)*24
!  Escribe solucion de volumenes
   do embalse = 1, NumEmbalses
!     para todos los intervalos de planeacion
      write ( 96, 600 ) embalse, NOMEMB(embalse), ( VolEmb ( embalse, i )/1000.0, i=IntIni,IntIni+intdia(dia)-1 )
!     nivel final
      aa4( embalse, dia, 4 ) = ALUTIL ( embalse , VolEmb ( embalse, NTINTR ) ) + NAMINO ( embalse )
!     volumen final
      aa4( embalse, dia, 5 ) = VolEmb ( embalse, NTINTR )/1000.0
   enddo
   write ( 96, * )
!  escribe solucion de variables artificiales en balance de volumenes almacenados
   do embalse = 1, NumEmbalses
      write ( 96, 600 ) embalse, NOMEMB(embalse), ( xMILP ( IEBAL + embalse + (i-1)*NumEmbalses - 1 )/1000.0, i=IntIni,IntIni+intdia(dia)-1 )
      write ( 96, 600 ) embalse, NOMEMB(embalse), ( xMILP ( IDBAL + embalse + (i-1)*NumEmbalses - 1 )/1000.0, i=IntIni,IntIni+intdia(dia)-1 )
   enddo
   IntIni = IntIni + intdia ( dia )
enddo

write ( 97, * )
! se escriben resultados de politicas de operacion
! para todos los embalses (vasos)
do embalse = 1, NumEmbalses

!  clave del vaso
   a1(embalse) = NOMEMB(embalse)
!  nivel inicial
   a2(embalse) = NIVINI(embalse)
!  nivel final
   a3(embalse) = NAMINO(embalse) + ALUTIL ( embalse , VolEmb (embalse, NTINTR) )
   
   Calculado(embalse) = 0.0
   
   SELECT CASE ( PoliEmb(embalse) )
   
!     maxima extraccion
      CASE (1)
         aux = 0.0
!        politica de operacion
         a4(embalse) = 'MAX. EXT.'
!        valor solicitado
         a5(embalse) = blanco
         !Deseado(embalse) = - EMxExt
         Deseado(embalse) = 0.0
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              generacion hidro de la unidad
               Calculado(embalse) = Calculado(embalse) + xMILP ( IGH + u + (i-1)*NumuniHid - 1 )
!              turbinado de la unidad
               aux = aux + xMILP ( ITURB + u + (i-1)*NumuniHid - 1 )
            enddo
         enddo
         Calculado(embalse) = Calculado(embalse)/10.0
         aux = 3.6*aux / 1000.0
         write ( calcula , 700 ) aux
!        valor calculado
         a6(embalse) = calcula

!     minima extraccion
      CASE (2)
         aux = 0.0
!        politica de operacion
         a4(embalse) = 'MIN. EXT.'
!        valor solicitado
         a5(embalse) = blanco
         !Deseado(embalse) = EMnExt
         Deseado(embalse) = 0.0
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              generacion hidro de la unidad
               Calculado(embalse) = Calculado(embalse) + xMILP ( IGH + u + (i-1)*NumuniHid - 1 )
!              turbinado de la unidad
               aux = aux + xMILP ( ITURB + u + (i-1)*NumuniHid - 1 )
            enddo
         enddo
         Calculado(embalse) = Calculado(embalse)/10.0
         aux = 3.6*aux / 1000.0
         write ( calcula , 700 ) aux
!        valor calculado
         a6(embalse) = calcula
         
!     cota final fija
      CASE (3)
!        politica de operacion
         a4(embalse) = 'COTAF-FIJA'
         !Deseado(embalse) = WEmbFin ( embalse )/1000.0
         Deseado(embalse) = namino ( embalse ) + ALUTIL ( embalse , WEmbFin ( embalse ) ) 
!        Volumen del ultimo intervalo del horizonte
         !Calculado(embalse) = xMa ( embalse + ie + (NTINTR-1)*NumEmbalses )/1000.0
         Calculado(embalse) = namino ( embalse ) + ALUTIL ( embalse , xMILP ( IVOLU + embalse + (NTINTR-1)*NumEmbalses - 1 ) )
         write ( solici , 700 ) Deseado(embalse)
         write ( calcula , 700 ) Calculado(embalse)
!        valor solicitado
         a5(embalse) = solici
!        valor calculado
         a6(embalse) = calcula
        aux = abs(Deseado(embalse)-Calculado(embalse))
!       Si existe infactibilidad en la politica
        if ( aux .gt. 1.0d-4 ) then
            SemBandera ( 6 ) = 1
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD EN POLITIC HIDRO'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( aaux2, 5102 ) nomemb ( embalse )
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 EMBALSE: '//aaux2
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!            aux = abs(solici-calcula)
            write ( aaux3, 5101 )  aux
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD (m)  '//aaux3
            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        endif

!     energia economica
      CASE (4)
!        politica de operacion
         a4(embalse) = 'ECON-ENER'
         Deseado(embalse) = LimSEnerEmb (embalse)/10.0
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              generacion hidro de la unidad
               Calculado(embalse) = Calculado(embalse) + xMILP ( IGH + u + (i-1)*NumuniHid - 1 )
            enddo
         enddo
         Calculado(embalse) = Calculado(embalse)/10.0
         write ( solici , 700 ) Deseado(embalse)
         write ( calcula , 700 ) Calculado(embalse)
!        valor solicitado
         a5(embalse) = solici
!        valor calculado
         a6(embalse) = calcula
         
!     turbinado economico
      CASE (5) 
!        politica de operacion
         a4(embalse) = 'ECON-TURB'
         !Deseado(embalse) = TFijEmb (embalse)*3.6
         Deseado(embalse) = TFijEmb ( embalse )  / 1000.0
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              turbinado de la unidad
               Calculado(embalse) = Calculado(embalse) + xMILP ( ITURB + u + (i-1)*NumuniHid - 1 )
            enddo
         enddo
         !Calculado(embalse) = Calculado(embalse)*3.6
         Calculado(embalse) = Calculado(embalse)  / 1000.0
         write ( solici , 700 ) Deseado(embalse)
         write ( calcula , 700 ) Calculado(embalse)
!        valor solicitado
         a5(embalse) = solici
!        valor calculado
         a6(embalse) = calcula

   END SELECT
   write ( 97, 500 ) embalse, NOMEMB(embalse), PoliEmb(embalse), Deseado(embalse), Calculado(embalse)
enddo

! se llenan resultados de produccion diaria en vasos

! para todos los embalses (vasos)
do embalse = 1, NumEmbalses

!  para todos los dias del horizonte
   do dia = 1 , MAXDIA
!     volumen turbinado
      aa4( embalse, dia, 1 ) = 0.0
!     energia producida
      aa4( embalse, dia, 6 ) = 0.0
!     aportacion
      aa4( embalse, dia, 3 ) = 0.0
   enddo   
!  localidad asociada a la primera unidad del embalse
   UniIni = local_uni_emb (embalse)
!  para todas las unidades asociadas al embalse
   do unidad = 1 , numuni_x_embalse (embalse)
!     unidad asociada 
      u = unidad_x_embalse ( UniIni + unidad - 1 )
      IntIni = 1
!     para todos los dias
      do dia = 1, durdia
         IntFin = IntIni + IntDia ( dia ) - 1
!        para todos los intervalos del dia
         do i = IntIni, IntFin
!           volumen turbinado
            aa4( embalse, dia, 1 ) = aa4( embalse, dia, 1 ) + &
                                     xMILP ( ITURB + u + (i-1)*NumUniHid - 1 )*3.6/1000.0
!           energia producida
            aa4( embalse, dia, 6 ) = aa4( embalse, dia, 6 ) + xMILP (IGH + u + (i-1)*NumUniHid - 1 )/10.0
        enddo
        IntIni = IntIni + IntDia ( dia )
      enddo
   enddo
enddo

!bandera 1 para que imprima a pantalla y bitacora la evaluacion del problema de despacho
call res_agua_quad ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4, 1 )

! se escriben resultados a archivos csv de turbinados y alturas en unidades 
call VolCarUH ( volumhR, cargahR )

! se escriben resultados de producci�n diaria en vasos a archivo RDVAAU.csv
call RDVAAU ( aa4 )

! se escriben resultados de resumen de vasos a archivo REVAAU.csv
call REVAAU ( a1, a2, a3, a4, a5, a6 )

! se escriben resultados de turbinados sobre vias VIAHO
call ResultVIAHO ( a_tra_res )

! se escriben resultados de volumenes finales en vasos
call ResultVASOEN2

600 format ( i4, x, a15, 169(f9.2) )
500 format ( i4, x, a15, x, i4, 2(f9.3) )
700 format ( F9.3 )
5101 FORMAT (F8.2)
5102 FORMAT (A20)

return
end


subroutine LimConsComb ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de energia a unidades termo      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2017                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer IntIni, intervalo, k,  dia, m, u, grupo, iniurc, iniurd, modo, unidad
character*4 letr

IRGPOGAS = m + 1

write ( 777,* ) 'Inicia restricciones de limite maximo de consumo de combustible  :', m + 1

! para todos los grupos con limitacion de combustible
do grupo = 1, NumGruGas
!   si el grupo esta activo
    if ( GpoAct ( grupo ) .eq. 1 ) then
        IntIni = 1
!       para todos los dias del horizonte
        do dia = 1, DURDIA
!           para todos los intervalos del dia
            do intervalo = IntIni, IntIni + intdia (dia) - 1
                iniurc = ApunURCxGpoGas ( grupo )
!               para todas las unidades de rango continuo que estan en ese grupo
                do unidad = 1 , NumURCxGpoGas ( grupo )
                    u = UniRCxGpoGas ( iniurc )
!                   si es unidad disponible
                    if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k ) = RTerURC ( u )
!                       columna asociada
                        jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                        k = k + 1
                    endif
                    iniurc = iniurc + 1
                enddo
!               para las unidades de rango discontinuo que estan en ese grupo
                iniurd = ApunURDxGpoGas ( grupo )
                do unidad = 1 , NumURDxGpoGas ( grupo )
                    u = UniRDxGrupo ( iniurd )
!                   para los modos excepto el apagado
                    do modo = 2, NumModRD ( u )
!                       si es unidad disponible 
                        if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                           coeficiente de la variable de generacion de la unidad
                            aaMILP ( k  ) = RTerURD ( u, modo )
                            jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                            k = k + 1
                        endif
                    enddo
                    iniurd = iniurd + 1
                enddo
            enddo
!           coeficiente de la variable artificial de consumo de combustible
            aaMILP ( k ) = - 1.0
!           columna asociada
            jcolMILP( k ) = IARCG + NumGruGas*(dia-1) + grupo - 1
            k = k + 1
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = LimSupGas ( grupo, dia )
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( letr, 200 ) dia
            write ( 779, * ) m, ',', '"Limite maximo de combustible disponible, grupo '//trim(NomGpoGas(grupo))//' dia: '//trim(letr)//'"'
            IntIni = IntIni + intdia ( dia )
        enddo
    endif
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de consumo de combustible  :', m + 1

! para todos los grupos con limitacion de combustible
do grupo = 1, NumGruGas
!   si el grupo esta activo
    if ( GpoAct ( grupo ) .eq. 1 ) then
        IntIni = 1
!       para todos los dias del horizonte
        do dia = 1, DURDIA
!           para todos los intervalos del dia
            do intervalo = IntIni, IntIni + intdia (dia) - 1
!               para todas las unidades de rango continuo que estan en ese grupo
                iniurc = ApunURCxGpoGas ( grupo )
                do unidad = 1 , NumURCxGpoGas ( grupo )
                    u = UniRCxGpoGas ( iniurc )
!                   si es unidad disponible
                    if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k ) = RTerURC ( u )
!                       columna asociada
                        jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                        k = k + 1
                    endif
                    iniurc = iniurc + 1
                enddo
!               para las unidades de rango discontinuo que estan en ese grupo
                iniurd = ApunURDxGpoGas ( grupo )
                do unidad = 1 , NumURDxGpoGas ( grupo )
                    u = UniRDxGrupo ( iniurd )
!                   para los modos excepto el apagado
                    do modo = 2, NumModRD ( u )
!                       si es unidad disponible 
                        if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                           coeficiente de la variable de generacion de la unidad
                            aaMILP ( k  ) = RTerURD ( u, modo )
                            jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                            k = k + 1
                        endif
                    enddo
                    iniurd = iniurd + 1
                enddo
            enddo
!           coeficiente de la variable artificial de consumo de combustible
            aaMILP ( k ) = 1.0
!           columna asociada
            jcolMILP( k ) = IARCG + NumGruGas*(dia-1) + grupo - 1
            k = k + 1
!           lados derechos de las restriciones
            m = m + 1
            bMILP ( m ) = LimInfGas ( grupo, dia )
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( letr, 200 ) dia
            write ( 779, * ) m, ',', '"Limite minimo de combustible disponible, grupo '//trim(NomGpoGas(grupo))//' dia: '//trim(letr)//'"'
            IntIni = IntIni + intdia ( dia )
        enddo
    endif
enddo

200 format (i3)
    
return
end    



subroutine LimConsComb_new ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de consumo de combustible        *
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

Integer intervalo, k, m, u, grupo, iniurc, iniurd, modo, unidad
character*4 letr

IRGPOGAS = m + 1

write ( 777,* ) 'Inicia restricciones de limite maximo de consumo de combustible  :', m + 1

! para todos los grupos con limitacion de combustible
do grupo = 1, NResComb
!   si el grupo esta activo
    if ( ActResComb ( grupo ) .eq. 1 ) then
!       para todos los intervalos del grupo
        do intervalo = HIResComb(grupo), HFResComb(grupo)
            iniurc = ApunURCxGpoGas ( grupo )
!           para todas las unidades de rango continuo que estan en ese grupo
            do unidad = 1 , NumURCxGpoGas ( grupo )
                u = UniRCxGpoGas ( iniurc )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = RTerURC ( u )
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniurc = iniurc + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniurd = ApunURDxGpoGas ( grupo )
            do unidad = 1 , NumURDxGpoGas ( grupo )
                u = UniRDxGrupo ( iniurd )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible 
                    if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k  ) = RTerURD ( u, modo )
                        jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniurd = iniurd + 1
            enddo
        enddo
!       coeficiente de la variable artificial de consumo de combustible
        aaMILP ( k ) = - 1.0
!       columna asociada
        jcolMILP( k ) = IARCG + grupo - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LSupResComb ( grupo )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( letr, 200 ) grupo
        write ( 779, * ) m, ',', '"Limite maximo de combustible disponible, grupo '//trim(NomGpoResComb(grupo))//' rest: '//trim(letr)//'"'
    endif
enddo

write ( 777,* ) 'Inicia restricciones de limite minimo de consumo de combustible  :', m + 1

! para todos los grupos con limitacion de combustible
do grupo = 1, NResComb
!   si el grupo esta activo
    if ( ActResComb ( grupo ) .eq. 1 ) then
!       para todos los intervalos del grupo
        do intervalo = HIResComb(grupo), HFResComb(grupo)
            iniurc = ApunURCxGpoGas ( grupo )
!           para todas las unidades de rango continuo que estan en ese grupo
            do unidad = 1 , NumURCxGpoGas ( grupo )
                u = UniRCxGpoGas ( iniurc )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   coeficiente de la variable de generacion de la unidad
                    aaMILP ( k ) = RTerURC ( u )
!                   columna asociada
                    jcolMILP( k ) = IGRC + u + (intervalo-1)*NumUniRC - 1
                    k = k + 1
                endif
                iniurc = iniurc + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniurd = ApunURDxGpoGas ( grupo )
            do unidad = 1 , NumURDxGpoGas ( grupo )
                u = UniRDxGrupo ( iniurd )
!               para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
!                   si es unidad disponible 
                    if ( DispoURD ( u, modo, intervalo ) .eq. 1 ) then
!                       coeficiente de la variable de generacion de la unidad
                        aaMILP ( k  ) = RTerURD ( u, modo )
                        jcolMILP ( k ) = IGRD + INIURDI ( u, intervalo ) + modo - 1
                        k = k + 1
                    endif
                enddo
                iniurd = iniurd + 1
            enddo
        enddo
!       coeficiente de la variable artificial de consumo de combustible
        aaMILP ( k ) = 1.0
!       columna asociada
        jcolMILP( k ) = IARCG + grupo - 1
        k = k + 1
!       lados derechos de las restriciones
        m = m + 1
        bMILP ( m ) = LInfResComb ( grupo )
!       sentidos de las restriciones
        sMILP ( m ) = 'G'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( letr, 200 ) grupo
        write ( 779, * ) m, ',', '"Limite m�nimo de combustible disponible, grupo '//trim(NomGpoResComb(grupo))//' rest: '//trim(letr)//'"'
    endif
enddo

200 format (i3)
    
return
end subroutine LimConsComb_new


subroutine LimGpoImportacion ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de grupos de importacion         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Agosto de 2018                                                      *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

integer i, k, m, grupo, j, u

character*3 leti

! para todos los grupos con limitacion de combustible
do grupo = 1, NumGpoImp
    do i = 1, NTINTR
        do j = ApuUniGpoImp(grupo), ApuUniGpoImp(grupo+1)-1
            u = LisUniGpoImp(j)
    !       coeficiente de la variable de generacion
            aaMILP ( k ) = 1.0
    !       columna asociada
            jcolMILP( k ) = IGRC + u + (i-1)*NumUniRC - 1
            k = k + 1
        enddo
        m = m + 1
        bMILP ( m ) = max ( PotMaxGpoImp ( grupo, i ) + 0.001, 0.001 )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, "(i3)" ) i
        write ( 779, * ) m, ',', '"Limite maximo de importacion grupo '//trim(NomGpoImp(grupo))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

return
end

subroutine LimGpoExportacion ( k, m )
! ---------------------------------------------------------------------
! Se forman restricciones de limites de grupos de exportacion         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Agosto de 2018                                                      *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

integer i, k, m, grupo, j, d, s

character*3 leti

! para todos los grupos con limitacion de combustible
do grupo = 1, NumGpoExp
    do i = 1, NTINTR
        do j = ApuCarGpoExp(grupo), ApuCarGpoExp(grupo+1)-1
            d = LisCarGpoExp(j)
!           para todos los segmentos de curva de ofertas de compra
            do s = 1, NumBloDem( d, i )
!              coeficiente de demanda en el bloque
               aaMILP ( k ) = 1.0
!              columna asociada
               jcolMILP( k ) = IDBC + s + INDDE ( d, i ) - 1
               k = k + 1
            enddo
        enddo
        m = m + 1
        bMILP ( m ) = max ( ( PotMaxGpoExp ( grupo, i ) - DemFija(d,i) ), 0.0 )
!       sentidos de las restriciones
        sMILP ( m ) = 'L'
!       apuntador al siguiente renglon
        irowMILP ( m + 1 ) = k
        write ( leti, "(i3)" ) i
        write ( 779, * ) m, ',', '"Limite maximo de exportacion grupo '//trim(NomGpoExp(grupo))//' intervalo: '//trim(leti)//'"'
    enddo
enddo

return
end