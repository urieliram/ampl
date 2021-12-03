! ---------------------------------------------------------------------
! Determina el numero de variables en el problema de asignacion       *
! (MILP).                                                             *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2019                                                        *
! ---------------------------------------------------------------------
Subroutine VarProAsign (sistema )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, NumNodSis

Implicit none

Integer i, d, k,  m, u, sistema, l, ro

Integer NBRC, NBARC, NBRD, NBARD, NMRD, NOFED, NBRE, NumVarBin, &
        NumVarCon, INMRD, NORORC, NOROH, NVREGRC, NVRERORC, &
        NVREGH, NVREROH, NVREGRD

integer NumBloRR10T, NumBloR10T, NumBloRSuT, NumBloRRegT

Integer IERROR, ibanbit, Porcentaje
CHARACTER fecha_Ej*19
character*16 aaux
real    Total, Econ

! Total de bloques de reserva
NumBloRR10T = 0
NumBloR10T = 0
NumBloRSuT = 0
NumBloRRegT = 0

do i = 1, NTINTR
    NumBloRR10T = NumBloRR10T + NumBloRR10( i )
    NumBloR10T = NumBloR10T + NumBloR10( i )
    NumBloRSuT = NumBloRSuT + NumBloRSu( i )
    NumBloRRegT = NumBloRRegT + NumBloRReg( i )
enddo

ibanbit = 1
ierror = 0
Total = 0
Econ = 0
Porcentaje = 0.0

! informacion de bandas prohibidas
IABPRC = 0
IABPH = 0
INVBPRC = 0
INVBPH = 0
NORORC = 0
NOROH = 0
! si se desea considerar bandas prohibidas
if ( SiBandProh .eq. 1 ) then
!   unidades de rango continuo
    k = 1
    do u = 1, NumUniRC
        if ( NoRaOpRC ( u ) .gt. 0 ) then
            INVBPRC ( u ) = k
            k = k + NoRaOpRC ( u )*NTINTR
            NORORC = NORORC + NoRaOpRC ( u )*NTINTR
        endif
    enddo
!   unidades hidro
    k = 1
    do u = 1, NumUniHid
        if ( NoRaOpH ( u ) .gt. 0 ) then
            INVBPH ( u ) = k
            k = k + NoRaOpH ( u )*NTINTR
            NOROH = NOROH + NoRaOpH ( u )*NTINTR
        endif
    enddo
endif

! informacion de variables binarias de regulacion rango continuo
IVREGRC = 0
INREGRC = 0
NVREGRC = 0
INVRERORC = 0
IRERORC = 0
NVRERORC = 0
do u = 1, NumUniRC
    do i = 1, NTINTR
!       si existe oferta de regulacion de la unidad
        if ( OferResRegRC ( u, i ) .gt. 0 ) then
!           si se desea considerar bandas prohibidas y la unidad tiene
            if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
                IRERORC ( u, i ) = NVRERORC
                do ro = 1, NoRaOpRC ( u )
                    NVRERORC = NVRERORC + 1
                enddo
            else
                NVREGRC = NVREGRC + 1
                INREGRC ( u, i ) = NVREGRC
            endif
        endif
    enddo
enddo

m = 0
do u = 1, NumUniRC*0
!   si existe oferta de regulacion de la unidad
    if ( OferResRegRC ( u, 1 ) .gt. 0 ) then
!       si se desea considerar bandas prohibidas y la unidad tiene
        if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
            IRERORC ( u, 1 ) = m
            m = m + NTINTR*NoRaOpRC ( u )
        endif
    endif
enddo

! informacion de variables binarias de regulacion hidro
IVREGH = 0
INREGH = 0
NVREGH = 0
INVREROH = 0
IREROH = 0
NVREROH = 0
do u = 1, NumUniHid
    do i = 1, NTINTR
!       si existe oferta de regulacion de la unidad
        if ( OferResRegH ( u, i ) .gt. 0 ) then
!           si se desea considerar bandas prohibidas y la unidad tiene
            if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
                IREROH ( u, i ) = NVREROH
                do ro = 1, NoRaOpH ( u )
                    NVREROH = NVREROH + 1
                enddo
            else
                NVREGH = NVREGH + 1
                INREGH ( u, i ) = NVREGH
            endif
        endif
    enddo
enddo

m = 0
do u = 1, NumUniHid*0
!   si existe oferta de regulacion de la unidad
    if ( OferResRegH ( u, 1 ) .gt. 0 ) then
!       si se desea considerar bandas prohibidas y la unidad tiene
        if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
            IREROH ( u, 1 ) = m
            m = m + NTINTR*NoRaOpH ( u )
        endif
    endif
enddo

! informacion de variables binarias de regulacion rango discontinuo
IVREGRD = 0
INREGRD = 0
NVREGRD = 0
do u = 1, NumUniRD
    do i = 1, NTINTR
!       para todos los modos de operacion
        do m = 1, NumModRD(u)
!           si existe oferta de regulacion de la unidad
            if ( CalOferResRegRD ( u, m, i ) .gt. 0.0 ) then
                NVREGRD = NVREGRD + 1
                INREGRD ( u, m, i ) = NVREGRD
            endif
        enddo
    enddo
enddo

! numero total de bloques en unidades de rango continuo
NBRC = 0
NBARC = 0
do i = 1, NTINTR
    do u = 1, NumUniRC
        NBRC = NBRC + NumBloVRC( u, i )
        NBARC = NBARC + NmBloArrURC( u )
        if ( DispoURC ( u , i ) .eq. 1 ) then
            Total = Total + 1
            if ( AsignURC ( u, i ) .eq. 1 ) then
                Econ = Econ + 1
            endif
        endif
    enddo
enddo

! numero total de bloques en unidades de rango discontinuo
NBRD = 0
NBARD = 0
do i = 1, NTINTR
    do u = 1, NumUniRD
        do m = 1, NumModRD( u )
            NBRD = NBRD + NumBloVRD( u, m, i )
            NBARD = NBARD + NmBloARRURD( u, m )
            if ( DispoURD ( u, m, i ) .eq. 1 ) then
                Total = Total + 1
                if ( AsignURD ( u, m, i ) .eq. 1 ) then
                    Econ = Econ + 1
                endif
            endif
        enddo
    enddo
enddo

! numero total de unidades hidro
do u = 1, NumUniHid
    do i = 1, NTINTR
        if ( DispoUH ( u, i ) .eq. 1 ) then
            Total = Total + 1
            if ( AsignUH ( u, i ) .eq. 1 ) then
                Econ = Econ + 1
            endif
        endif
    enddo
enddo
! numero total de modos en unidades de rango discontinuo
NMRD = 0
INMRD = 0
do i = 1, NTINTR
    do u = 1, NumUniRD
        NMRD = NMRD + NumModRD( u )
        INMRD = INMRD + NumModRD( u )*NumModRD( u )
    enddo
enddo

! numero total de bloques en unidades renovables
NBRE = 0
do i = 1, NTINTR
    do u = 1, NumUniRE
        NBRE = NBRE + NumBloVRE( u, i )
    enddo
enddo

! inicios de variables de unidades de rango discontinuo, e intervalo
k = 0
l = 0
do u = 1, NumUniRD
    do i = 1, NTINTR
        INIURDI ( u, i ) = k
        k = k + NumModRD ( u )
        INALPHAT ( u, i ) = l
        l = l + NumModRD ( u )*NumModRD ( u )
    enddo
enddo

! numero total de ofertas de demanda
NOFED = 0
do d = 1, NumOferDem
    do i = 1, NTINTR
        INDDE ( d, i ) = NOFED
        NOFED = NOFED + NumBloDem( d, i )
    enddo
enddo

! Porcentaje de unidades economicas en el escenario
Porcentaje = nint((Econ/Total)*100)
 
! Se escriben dimensiones del modelo de asignacion (MILP)
OPEN (UNIT = 777, FILE = RUT_RES//'DimensModelMILP.txt', &
      IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

    write ( 777, * )
    write ( 777, * ) 'Sistema :', sistema, nomsis(sistema)
    write ( 777, * ) 
    write ( 777, * ) 'Fecha de horizonte de planeacion    : ', HORIZO
    write ( 777, * ) 'Intervalos                          : ', NTINTR
    write ( 777, * ) 'Unidades rango continuo             : ', NumUniRC*SiUniRC
    write ( 777, * ) 'Unidades rango discontinuo          : ', NumUniRD*SiUniRD
    write ( 777, * ) 'Unidades hidro                      : ', NumUNiHid*SiUniH
    write ( 777, * ) 'Unidades renovables                 : ', NumUniRE*SiUniRE
    write ( 777, * ) 'Ramas restringidas                  : ', NumGruRamSis (sistema)
    write ( 777, * ) 'Grupos de reserva                   : ', NumGruRes*SiOferComResZona
    write ( 777, * ) 'Demandas participantes              : ', NumOferDem
    write ( 777, * ) 'Nodos electricos                    : ', NumNodSis (sistema)
    write ( 777, * ) 'Embalses                            : ', NumEmbalses
    write ( 777, * ) 'Grupos de energia termica           : ', NumGruUTer
    write ( 777, * ) 'Consideracion de transmision        : ', SiTransmision
    write ( 777, * ) 'Consideracion de perdidas           : ', SiPerdidas
    write ( 777, * ) 'Iteraciones para perdidas           : ', IterPerdidas
    write ( 777, * ) 
    write ( 777, * ) 'Consideracion de limites ener hidro : ', SiEnerHid
    write ( 777, * ) 'Consideracion de limites ener termo : ', SiEnerTer
    write ( 777, * ) 'Consideracion de bandas prohibidas  : ', SiBandProh
    write ( 777, * ) 'Consideracion de limites regulacion : ', SiLimReg
    write ( 777, * ) 'Consideracion de arra no simultaneo : ', SiArrNoSimul
    write ( 777, * ) 'Consideracion de modelado hidraulico: ', SiModHid
    write ( 777, * ) 'Consideracion de reserva distribuida: ', SiResRegDis
    write ( 777, * ) 'Consideracion de consumo de combusti: ', SiGpoGas
    write ( 777, * ) 
    write ( 777, * ) 'Porcentaje de unidades economicas, %: ', Porcentaje
    write ( 777, * ) 'GAP solicitado %                    : ', GAPCPLEX*100.0
    write ( 777, * ) 'Tiempo maximo para un MILP (seg)    : ', Limit_Time_Linear
    if ( TipoEjecu .eq. 3 .and. nomsis(sistema) .eq. 'SIN' ) then
    write ( 777, * ) 'Calculo interno de penalizaciones   : ', SiCalPen
    write ( 777, * ) 'Uso de una solucion inicial previa  : ', SiSolucionInicial
    write ( 777, * ) 'Escribe a un archivo el modelo MILP : ', SiEscLP
    endif
    if ( TipoEjecu .eq. 1  ) then
    write ( 777, * ) 'Mantener reservas de MDA en AUGC    : ', SiMantReservas
    write ( 777, * ) 'Relajar reservas de regulacion      : ', SiRelReg
    write ( 777, * ) 'Relajar reserva rodante de 10 min   : ', SiRelRod
    write ( 777, * ) 'Mantener reservas no rodantes       : ', SiRelNRod
    endif
    if ( TipoEjecu .eq. 0  ) then
    write ( 777, * ) 'Relajar reservas de regulacion      : ', SiRelReg
    endif

    Call FechaEjecucion (fecha_Ej)
    BMensaje = fecha_Ej//'         '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5001 ) nomsis(sistema)
    BMensaje = fecha_Ej//' '//NomEjecu//'001 SISTEMA               :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    BMensaje = fecha_Ej//'         '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5001 ) HORIZO
    BMensaje = fecha_Ej//' '//NomEjecu//'001 FECHA DE HORIZONTE    :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    write ( aaux, 5000 ) NTINTR
    BMensaje = fecha_Ej//' '//NomEjecu//'001 INTERVALOS            :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumUniRC*SiUniRC
    BMensaje = fecha_Ej//' '//NomEjecu//'001 UNIDADES R CONTINUO   :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumUniRD*SiUniRD
    BMensaje = fecha_Ej//' '//NomEjecu//'001 UNIDADES R DISCONTINUO:   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumUNiHid*SiUniH
    BMensaje = fecha_Ej//' '//NomEjecu//'001 UNIDADES HIDRO        :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumUniRE
    BMensaje = fecha_Ej//' '//NomEjecu//'001 UNIDADES RENOVABABLES :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumGruRamSis (sistema)
    BMensaje = fecha_Ej//' '//NomEjecu//'001 RAMAS RESTRINGIDAS    :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumGruRes*SiOferComResZona
    BMensaje = fecha_Ej//' '//NomEjecu//'001 GRUPOS DE RESERVA     :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumOferDem
    BMensaje = fecha_Ej//' '//NomEjecu//'001 DEMANDAS PARTICIPANTES:   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumNodSis (sistema)
    BMensaje = fecha_Ej//' '//NomEjecu//'001 NODOS ELECTRICOS      :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumEmbalses
    BMensaje = fecha_Ej//' '//NomEjecu//'001 EMBALSES              :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) NumGruUTer
    BMensaje = fecha_Ej//' '//NomEjecu//'001 GPOS DE ENERG TERMICA :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiTransmision
    BMensaje = fecha_Ej//' '//NomEjecu//'001 TRANSMIS (1=SI, 0=NO) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!   En AUGC no hay estimacion de perdidas
!    if ( TipoEjecu .eq. 0 ) then
        write ( aaux, 5000 ) SiPerdidas
        BMensaje = fecha_Ej//' '//NomEjecu//'001 PERDIDAS (1=SI, 0=NO) :   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux, 5000 ) IterPerdidas
        BMensaje = fecha_Ej//' '//NomEjecu//'001 ITERACIONES PERDIDAS  :   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!    endif
    BMensaje = fecha_Ej//'         '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiEnerHid
    BMensaje = fecha_Ej//' '//NomEjecu//'001 LIM EN HI (1=SI, 0=NO):   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiEnerTer
    BMensaje = fecha_Ej//' '//NomEjecu//'001 LIM EN TE (1=SI, 0=NO):   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiBandProh
    BMensaje = fecha_Ej//' '//NomEjecu//'001 BANDPROH (1=SI, 0=NO) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiLimReg
    BMensaje = fecha_Ej//' '//NomEjecu//'001 LIM REGUL (1=SI, 0=NO):   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiArrNoSimul
    BMensaje = fecha_Ej//' '//NomEjecu//'001 ARR NO S (1=SI, 0=NO) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiModHid
    BMensaje = fecha_Ej//' '//NomEjecu//'001 RED HIDRO(1=SI, 0=NO) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiResRegDis
    BMensaje = fecha_Ej//' '//NomEjecu//'001 RES DISTR(1=SI, 0=NO) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) SiGpoGas
    BMensaje = fecha_Ej//' '//NomEjecu//'001 REST COMBU(1=SI, 0=NO):   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    BMensaje = fecha_Ej//'         '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5000 ) Porcentaje
    BMensaje = fecha_Ej//' '//NomEjecu//'001 Unidades economicas % :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5002 ) GAPCPLEX*100.0
    BMensaje = fecha_Ej//' '//NomEjecu//'001 GAP MILP solicitado % :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( aaux, 5003 ) Limit_Time_Linear
    BMensaje = fecha_Ej//' '//NomEjecu//'001 Tiempo max MILP (seg) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    if ( TipoEjecu .eq. 3 .and. nomsis(sistema) .eq. 'SIN' ) then
        write ( aaux, 5000 ) SiCalPen
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Calc Pena (1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux, 5000 ) SiSolucionInicial
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Sol Inicio(1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif
    write ( aaux, 5000 ) SiEscLP
    BMensaje = fecha_Ej//' '//NomEjecu//'001 ESCR MILP(1=SI, 0=NO) :   '//aaux
    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    if ( TipoEjecu .eq. 1  ) then
        write ( aaux, 5000 ) SiMantReservas
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Mant Reser(1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux, 5000 ) SiRelReg
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Rel Res Re(1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux, 5000 ) SiRelRod
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Rel Res Ro(1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux, 5000 ) SiRelNRod
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Mant ResNR(1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif
    if ( TipoEjecu .eq. 0  ) then
        write ( aaux, 5000 ) SiRelReg
        BMensaje = fecha_Ej//' '//NomEjecu//'001 Rel Res Re(1=SI, 0=NO):   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif
    BMensaje = fecha_Ej//'         '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    
    if ( SiPerdidas .eq. 0 ) then
        IterPerdidas = 0
    endif
    
write ( 777,* )
write ( 777,* ) ' Variables de Unidades de Rango Continuo'
IGRC = 1
IGDARC = NumUniRC*NTINTR + IGRC
write ( 777,* ) ' Generacion de operacion    :', IGRC,' - ', IGDARC - 1
IARC = NumUniRC*NTINTR + IGDARC
write ( 777,* ) ' Generacion durante arranque:', IGDARC,' - ', IARC - 1
IADARC = NumUniRC*NTINTR + IARC
write ( 777,* ) ' Asignacion de operacion    :', IARC,' - ', IADARC - 1
IARRC = NumUniRC*NTINTR + IADARC
write ( 777,* ) ' Asignacion durante arranque:', IADARC,' - ', IARRC - 1
IPRC = NumUniRC*NTINTR + IARRC
write ( 777,* ) ' Arranque                   :', IARRC,' - ', IPRC - 1
IRR10RC = NumUniRC*NTINTR + IPRC
write ( 777,* ) ' Paro                       :', IPRC,' - ', IRR10RC - 1
IRNR10RC = NumUniRC*NTINTR + IRR10RC
write ( 777,* ) ' Rodante de 10 minutos      :', IRR10RC,' - ', IRNR10RC - 1
IRRSRC = NumUniRC*NTINTR + IRNR10RC
write ( 777,* ) ' No rodante de 10 minutos   :', IRNR10RC,' - ', IRRSRC - 1
IRNRSRC = NumUniRC*NTINTR + IRRSRC
write ( 777,* ) ' Rodante Suplementaria      :', IRRSRC,' - ', IRNRSRC - 1
IRRERC = NumUniRC*NTINTR + IRNRSRC
write ( 777,* ) ' No Rodante Suplementaria   :', IRNRSRC,' - ', IRRERC - 1
IGABRC = NumUniRC*NTINTR + IRRERC
write ( 777,* ) ' Regulacion Secundaria      :', IRRERC,' - ', IGABRC - 1
IBOARC = NBRC + IGABRC
write ( 777,* ) ' Generacion en cada bloque  :', IGABRC,' - ', IBOARC - 1
if ( SiBandProh .eq. 1 ) then
    IABPRC = NBARC + IBOARC
    write ( 777,* ) ' Bloque de oferta arranque  :', IBOARC,' - ', IABPRC - 1
    IVREGRC = NORORC + IABPRC
    write ( 777,* ) ' Rangos operativos          :', IABPRC,' - ', IVREGRC - 1
else
    IVREGRC = NBARC + IBOARC
    write ( 777,* ) ' Bloque de oferta arranque  :', IBOARC,' - ', IVREGRC - 1
endif
INVRERORC = NVREGRC + IVREGRC
write ( 777,* ) ' Binarias Regulacion Secunda:', IVREGRC,' - ', INVRERORC - 1
IGRD = NVRERORC + INVRERORC
write ( 777,* ) ' Binarias Reg Sec Rango Oper:', INVRERORC,' - ', IGRD - 1
write ( 777,* )
write ( 777,* ) ' Variables de Unidades de Rango Discontinuo'
IGDARD = NMRD + IGRD
write ( 777,* ) ' Generacion en operacion    :', IGRD,' - ', IGDARD - 1
IARD = NMRD + IGDARD
write ( 777,* ) ' Generacion durante aranque :', IGDARD,' - ', IARD - 1
IADARD = NMRD + IARD
write ( 777,* ) ' Asignacion para operacion  :', IARD,' - ', IADARD - 1
IARNRRD = NMRD + IADARD
write ( 777,* ) ' Asignacion durante arranque:', IADARD,' - ', IARNRRD - 1
IARRD = NMRD + IARNRRD
write ( 777,* ) ' Asignacion reserva no rodan:', IARNRRD,' - ', IARRD - 1
IOMARD = NMRD + IARRD
write ( 777,* ) ' Arranque                   :', IARRD,' - ', IOMARD - 1
IPRD = INMRD + IOMARD
write ( 777,* ) ' Inicio de operacion modos  :', IOMARD,' - ', IPRD - 1
IRR10RD = NumUniRD*NTINTR + IPRD
write ( 777,* ) ' Paro                       :', IPRD,' - ', IRR10RD - 1
IRNR10RD = NMRD + IRR10RD
write ( 777,* ) ' Rodante de 10 minutos      :', IRR10RD,' - ', IRNR10RD - 1
IRRSRD = NMRD + IRNR10RD
write ( 777,* ) ' No rodante de 10 minutos   :', IRNR10RD,' - ', IRRSRD - 1
IRNRSRD = NMRD + IRRSRD
write ( 777,* ) ' Rodante Suplementaria      :', IRRSRD,' - ', IRNRSRD - 1
IRRERD = NMRD + IRNRSRD
write ( 777,* ) ' No Rodante Suplementaria   :', IRNRSRD,' - ', IRRERD - 1
IGABRD = NMRD + IRRERD
write ( 777,* ) ' Regulacion Secundaria      :', IRRERD,' - ', IGABRD - 1
IBOARD = NBRD + IGABRD
write ( 777,* ) ' Generacion en cada bloque  :', IGABRD,' - ', IBOARD - 1
IVREGRD = NBARD + IBOARD
write ( 777,* ) ' Bloque de oferta arranque  :', IBOARD,' - ', IVREGRD - 1
IGH = NVREGRD + IVREGRD
write ( 777,* ) ' Binarias Regulacion Secunda:', IVREGRD,' - ', IGH - 1
write ( 777,* )
write ( 777,* ) ' Variables de Unidades Hidro'
IAH = NumUniHid*NTINTR + IGH
write ( 777,* ) ' Generacion                 :', IGH,' - ', IAH - 1
IARH = NumUniHid*NTINTR + IAH
write ( 777,* ) ' Asignacion                 :', IAH,' - ', IARH - 1
IPH = NumUniHid*NTINTR + IARH
write ( 777,* ) ' Arranque                   :', IARH,' - ', IPH - 1
IRR10H = NumUniHid*NTINTR + IPH
write ( 777,* ) ' Paro                       :', IPH,' - ', IRR10H - 1
IRNR10H = NumUniHid*NTINTR + IRR10H
write ( 777,* ) ' Rodante de 10 minutos      :', IRR10H,' - ', IRNR10H - 1
IRRSH = NumUniHid*NTINTR + IRNR10H
write ( 777,* ) ' No rodante de 10 minutos   :', IRNR10H,' - ', IRRSH - 1
IRNRSH = NumUniHid*NTINTR + IRRSH
write ( 777,* ) ' Rodante Suplementaria      :', IRRSH,' - ', IRNRSH - 1
IRREH = NumUniHid*NTINTR + IRNRSH
write ( 777,* ) ' No Rodante Suplementaria   :', IRNRSH,' - ', IRREH - 1
if ( SiBandProh .eq. 1 ) then
    IABPH = NumUniHid*NTINTR + IRREH
    write ( 777,* ) ' Regulacion Secundaria      :', IRREH,' - ', IABPH - 1
    IVREGH = NOROH + IABPH
    write ( 777,* ) ' Rangos operativos          :', IABPH,' - ', IVREGH - 1
else
    IVREGH = NumUniHid*NTINTR + IRREH
    write ( 777,* ) ' Regulacion Secundaria      :', IRREH,' - ', IVREGH - 1
endif
INVREROH = NVREGH + IVREGH
write ( 777,* ) ' Binarias Regulacion Secunda:', IVREGH,' - ', INVREROH - 1
IGRE = NVREROH + INVREROH
write ( 777,* ) ' Binarias Reg Sec Rango Oper:', INVREROH,' - ', IGRE - 1
write ( 777,* )
write ( 777,* ) ' Variables de Unidades Renovables'
IARE = NumUniRE*NTINTR + IGRE
write ( 777,* ) ' Generacion                 :', IGRE,' - ', IARE - 1
IGABRE = NumUniRE*NTINTR + IARE
write ( 777,* ) ' Asignacion                 :', IARE,' - ', IGABRE - 1
IDF = NBRE + IGABRE
write ( 777,* ) ' Generacion en cada bloque  :', IGABRE,' - ', IDF - 1
write ( 777,* )
write ( 777,* ) ' Variables de Demandas (cargas)'
!ICCS = NumOferDem*NTINTR + ICC10
!write ( 777,* ) ' Controlables de 10 minutos :', ICC10,' - ', ICCS - 1
!IDF = NumOferDem*NTINTR + ICCS
!write ( 777,* ) ' Controlables suplementarias:', ICCS,' - ', IDF - 1
IDBC = NumOferDem*NTINTR + IDF
write ( 777,* ) ' Corte de carga             :', IDF,' - ', IDBC - 1
ICARR10G = NOFED + IDBC
write ( 777,* ) ' Demanda del bloque         :', IDBC,' - ', ICARR10G - 1
write ( 777,* )
write ( 777,* ) ' Variables de Reservas aceptadas por CENACE en grupos'
ICAR10G = NumBloRR10T*NumGruRes*SiOferComResZona + ICARR10G
write ( 777,* ) ' Rodante de 10 minutos      :', ICARR10G,' - ', ICAR10G - 1
ICARSG = NumBloR10T*NumGruRes*SiOferComResZona + ICAR10G
write ( 777,* ) ' De 10 minutos              :', ICAR10G,' - ', ICARSG - 1
ICARRG = NumBloRSuT*NumGruRes*SiOferComResZona + ICARSG
write ( 777,* ) ' Suplementaria              :', ICARSG,' - ', ICARRG - 1
ICARR10S = NumBloRRegT*NumGruRes*SiOferComResZona + ICARRG
write ( 777,* ) ' Regulacion                 :', ICARRG,' - ', ICARR10S - 1
write ( 777,* )
write ( 777,* ) ' Variables de Reservas aceptadas por CENACE en el sistema'
ICAR10S = NumBloRR10T*SiOferComResSis + ICARR10S
write ( 777,* ) ' Rodante de 10 minutos      :', ICARR10S,' - ', ICAR10S - 1
ICARSS = NumBloR10T*SiOferComResSis + ICAR10S
write ( 777,* ) ' De 10 minutos              :', ICAR10S,' - ', ICARSS - 1
ICARRS = NumBloRSuT*SiOferComResSis + ICARSS
write ( 777,* ) ' Suplementaria              :', ICARSS,' - ', ICARRS - 1
IEXC = NumBloRRegT*SiOferComResSis + ICARRS
write ( 777,* ) ' Regulacion                 :', ICARRS,' - ', IEXC - 1
write ( 777,* )
write ( 777,* ) ' Variables artificiales'
IAEF = NumNodSis (sistema)*NTINTR + IEXC
write ( 777,* ) ' Excedentes                 :', IEXC,' - ', IAEF - 1
IACF = NumGruRamSis (sistema)*NTINTR + IAEF
write ( 777,* ) ' Excedente de flujo         :', IAEF,' - ', IACF - 1
IARGT = NumGruRamSis (sistema)*NTINTR + IACF
write ( 777,* ) ' Excedente de contraflujo   :', IACF,' - ', IARGT - 1
IAREE = NumGruUTer*DURDIA*SiEnerTer + IARGT
!IAREE = NResEner*SiEnerTer + IARGT
write ( 777,* ) ' Excedente energia fija term:', IARGT,' - ', IAREE - 1
IPERD = NumEmbalses + IAREE
write ( 777,* ) ' Excedente energia fija emba:', IAREE,' - ', IPERD - 1
ITURB = numsis_act*NTINTR + IPERD
write ( 777,* ) ' Variables para estimar perd:', IPERD,' - ', ITURB - 1
write ( 777,* )
write ( 777,* ) ' Variables hidraulicas'
IVOLU = NumUniHid*NTINTR*SiModHid + ITURB
write ( 777,* ) ' Variables de turbinad hidro:', ITURB,' - ', IVOLU - 1
IDPOL = NumEmbalses*NTINTR*SiModHid + IVOLU
write ( 777,* ) ' Variables de volumen  hidro:', IVOLU,' - ', IDPOL - 1
IEPOL = NumEmbalses*SiModHid + IDPOL
write ( 777,* ) ' Variables de deficit en pol:', IDPOL,' - ', IEPOL - 1
IEBAL = NumEmbalses*SiModHid + IEPOL
write ( 777,* ) ' Variables de exceden en pol:', IEPOL,' - ', IEBAL - 1
IDBAL = NumEmbalses*NTINTR*SiModHid + IEBAL
write ( 777,* ) ' Variables de exceden en bal:', IEBAL,' - ', IDBAL - 1
IARCG = NumEmbalses*NTINTR*SiModHid + IDBAL
write ( 777,* ) ' Variables de deficit en bal:', IDBAL,' - ', IARCG - 1
if ( TipoEjecu .le. 1 ) then
    IREUSO10 = NumGruGas*DURDIA*SiGpoGas + IARCG
!    IREUSO10 = NResComb*SiGpoGas + IARCG
    write ( 777,* ) ' Variables artif combustible:', IARCG,' - ', IREUSO10 - 1
    IREUSORE = NumUniRC*NTINTR + IREUSO10
    write ( 777,* ) ' Variables de uso de rese 10:', IREUSO10,' - ', IREUSORE - 1
    IREUSOREH = NumUniRC*NTINTR + IREUSORE
    write ( 777,* ) ' Variables de uso de rese re:', IREUSORE,' - ', IREUSOREH - 1
!    Numero de variables de decision en el problema de asignacion (MILP)
    NumVarAsig = NumUniHid*NTINTR + IREUSOREH - 1
    write ( 777,* ) ' Variables de uso de res reh:', IREUSOREH,' - ', NumVarAsig
else
!    Numero de variables de decision en el problema de asignacion (MILP)
    NumVarAsig = NumGruGas*DURDIA*SiGpoGas + IARCG - 1
!    NumVarAsig = NResComb*SiGpoGas + IARCG - 1
    write ( 777,* ) ' Variables artif combustible:', IARCG,' - ', NumVarAsig
endif
write ( 777,* )
write ( 777,* ) ' Numero de variables decision en el problema de asignacion (MILP):', NumVarAsig
! Numero de variables binarias en el problema de asignacion (MILP)
NumVarBin = ( 4*NumUniRC + NumUniRD + 3*NumUniHid + NumUniRE ) * NTINTR + 4*NMRD + INMRD + NORORC + NOROH + &
            NVREGRC + NVRERORC + NVREGH + NVREROH + NVREGRD
! Numero de variables continuas en el problema de asignacion (MILP)
NumVarCon = NumVarAsig - NumVarBin
write ( 777,* )
write ( 777,* ) ' Numero de variables binarias en el problema de asignacion (MILP):', NumVarBin
write ( 777,* ) ' Numero de variables continuas en el problema de asignacion(MILP):', NumVarCon
write ( 777,* )

! Se inicializa los tipos de las variables de desicion
do i = 1, NumVarAsig
    ctypeMILP ( i ) = 'C'
    lbMILP ( i ) = 0.0
    ubMILP ( i ) = 100000.0/Base
enddo

k = IPERD + NTINTR
! Cotas superiores para variables de perdidas
do i = IPERD, k - 1
    ubMILP ( i ) = 5000.0/Base
enddo

! inicios de variables de unidades de rango discontinuo, e intervalo
k = 0
l = 0
do u = 1, NumUniRD
    do i = 1, NTINTR
        INIURDI ( u, i ) = k
        k = k + NumModRD ( u )
        INALPHAT ( u, i ) = l
        l = l + NumModRD ( u )*NumModRD ( u )
    enddo
enddo

! inicios de variables de unidades de rango discontinuo, modo e intervalo
k = 0
l = 0
do u = 1, NumUniRD
    do i = 1, NTINTR
        do m = 1, NumModRD ( u )
            INBURD ( u, m, i ) = k
            k = k + NumBloVRD ( u, m, i )
            INBAURD ( u, m, i ) = l
            l = l + NmBloArrURD ( u, m )
        enddo
    enddo
enddo
! Imprime variables del problema de optimizacion
call VarDiccMILP

5000 FORMAT (I5)
5001 FORMAT (A12)
5002 FORMAT (F9.6)
5003 FORMAT (F9.1)

return
end

    
subroutine BloReqRes
! ---------------------------------------------------------------------
! Se determinan los bloques de reserva para requerimientos del CENACE *
! del problema de asignacion (MILP)                                   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Agosto de 2019                                                      *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

Integer i, intervalo, k, S, sistema
integer NBRR10, NBR10, NBRSU, NBRRE
          
! para todas las zonas
do i = 1, NumGruRes*SiOferComResZona
    do intervalo = 1, NTINTR

        NumBloRR10( intervalo ) = maxsegce
        NumBloR10( intervalo ) = maxsegce
        NumBloRSu( intervalo ) = maxsegce
        NumBloRReg( intervalo ) = maxsegce

!       se determina le numero de bloques por tipo de reserva
        k = 1
        do while ( ReqResR10 ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloRR10( intervalo ) )
            k = k + 1
        enddo
        NBRR10 = k-1
        k = 1
        do while ( ReqRes10 ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloR10( intervalo ) )
            k = k + 1
        enddo
        NBR10 = k-1
        k = 1
        do while ( ReqResSup ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloRSu( intervalo ) )
            k = k + 1
        enddo
        NBRSU = k-1
        k = 1
        do while ( ReqResReg ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloRReg( intervalo ) )
            k = k + 1
        enddo
        NBRRE = k-1

        NumBloRR10( intervalo ) = NBRR10
        NumBloR10( intervalo ) = NBR10
        NumBloRSu( intervalo ) = NBRSU
        NumBloRReg( intervalo ) = NBRRE
        
!       segmentos de reserva suplementaria            
        do s = NBRSU, 1,-1
            if ( s .gt. 1) then
                ReqResSup ( i, s, intervalo ) = ReqResSup ( i, s, intervalo ) - ReqResSup ( i, s-1, intervalo )
            else
                if ( NBR10 .gt. 0 ) then
                    ReqResSup ( i, s, intervalo ) = ReqResSup ( i, s, intervalo ) - ReqRes10 ( i, NBR10, intervalo )
                endif
            endif
        enddo

!       segmentos de reserva de 10
        do s = NBR10, 1,-1
            if ( s .gt. 1) then
                ReqRes10 ( i, s, intervalo ) = ReqRes10 ( i, s, intervalo ) - ReqRes10 ( i, s-1, intervalo )
            else
                if ( NBRR10 .gt. 0 ) then
                    ReqRes10 ( i, s, intervalo ) = ReqRes10 ( i, s, intervalo ) - ReqResR10 ( i, NBRR10, intervalo )
                endif
            endif
        enddo

!       si la reserva de regulacion no contribuye a la reserva rodante
        if ( SiRegEnRod .eq. 0 ) then
!           segmentos de reserva rodante de 10
            if ( NBRR10 .gt. 1 ) then
                do s = NBRR10, 2,-1
                    ReqResR10 ( i, s, intervalo ) = ReqResR10 ( i, s, intervalo ) - ReqResR10 ( i, s-1, intervalo )
                enddo
            endif
        else
            do s = NBRR10, 1,-1
                if ( s .gt. 1) then
                    ReqResR10 ( i, s, intervalo ) = ReqResR10 ( i, s, intervalo ) - ReqResR10 ( i, s-1, intervalo )
                else
                    if ( NBRRE .gt. 0 ) then
                        ReqResR10 ( i, s, intervalo ) = ReqResR10 ( i, s, intervalo ) - ReqResReg ( i, NBRRE, intervalo )
                    endif
                endif
            enddo
        endif
        
!       segmentos de reserva de regulacion
        if ( NBRRE .gt. 1 ) then
            do s = NBRRE, 2,-1
                ReqResReg ( i, s, intervalo ) = ReqResReg ( i, s, intervalo ) - ReqResReg ( i, s-1, intervalo )
            enddo
        endif

555 continue
    enddo
enddo

i = 0
! para todos los sistemas
do sistema = 1, numsis*SiOferComResSis
    if ( EstadoIsla ( sistema ) .eq. 1 ) then
        i = i + 1
!       segmentos de reserva suplementaria
        do intervalo = 1, NTINTR
!           se determina le numero de bloques por tipo de reserva
            k = 1
            do while ( ReqResR10S ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloRR10( intervalo ) )
                k = k + 1
            enddo
            NBRR10 = k-1
            k = 1
            do while ( ReqRes10S ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloR10( intervalo ) )
                k = k + 1
            enddo
            NBR10 = k-1
            k = 1
            do while ( ReqResSupS ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloRSu( intervalo ) )
                k = k + 1
            enddo
            NBRSU = k-1
            k = 1
            do while ( ReqResRegS ( i, k, intervalo ) .gt. 0 .and. k.le.NumBloRReg( intervalo ) )
                k = k + 1
            enddo
            NBRRE = k-1
        
!           segmentos de reserva suplementaria
            do s = NBRSU, 1,-1
                if ( s .gt. 1) then
                    ReqResSupS ( i, s, intervalo ) = ReqResSupS ( i, s, intervalo ) - ReqResSupS ( i, s-1, intervalo )
                else
                    if ( NBR10 .gt. 0 ) then
                        ReqResSupS ( i, s, intervalo ) = ReqResSupS ( i, s, intervalo ) - ReqRes10S ( i, NBR10, intervalo )
                    endif
                endif
            enddo

!           segmentos de reserva de 10
            do s = NBR10, 1,-1
                if ( s .gt. 1) then
                    ReqRes10S ( i, s, intervalo ) = ReqRes10S ( i, s, intervalo ) - ReqRes10S ( i, s-1, intervalo )
                else
                    if ( NBRR10 .gt. 0 ) then
                        ReqRes10S ( i, s, intervalo ) = ReqRes10S ( i, s, intervalo ) - ReqResR10S ( i, NBRR10, intervalo )
                    endif
                endif
            enddo

!           si la reserva de regulacion no contribuye a la reserva rodante
            if ( SiRegEnRod .eq. 0 ) then
!               segmentos de reserva rodante de 10
                if ( NBRR10 .gt. 1 ) then
                    do s = NBRR10, 2,-1
                        ReqResR10S ( i, s, intervalo ) = ReqResR10S ( i, s, intervalo ) - ReqResR10S ( i, s-1, intervalo )
                    enddo
                endif
            else
                do s = NBRR10, 1,-1
                    if ( s .gt. 1) then
                        ReqResR10 ( i, s, intervalo ) = ReqResR10 ( i, s, intervalo ) - ReqResR10 ( i, s-1, intervalo )
                    else
                        if ( NBRRE .gt. 0 ) then
                            ReqResR10 ( i, s, intervalo ) = ReqResR10 ( i, s, intervalo ) - ReqResReg ( i, NBRRE, intervalo )
                        endif
                    endif
                enddo
            endif

!           segmentos de reserva de regulacion
            if ( NBRRE .gt. 1 ) then
                do s = NBRRE, 2,-1
                    ReqResRegS ( i, s, intervalo ) = ReqResRegS ( i, s, intervalo ) - ReqResRegS ( i, s-1, intervalo )
                enddo
            endif
        enddo
    endif
enddo

return
end

! ---------------------------------------------------------------------
! Escribir el diccionario de variables para el modelo de asignacion   *
! de unidades (MILP).                                                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2019                                                        *
! ---------------------------------------------------------------------
Subroutine VarDiccMILP

use ParAUHE, only: NUMEMBALSES
use ProblemaAUHE
use ParAuHeHidro, only: nomemb

use ParGloRed, only: NumGruRamSis, NumNodSis, nomnod, nomgruram

Implicit none

Integer i, d, k,  m, u, ro, s, r, is, ie, dia, grupo

Integer IERROR, ibanbit
character*3 leti, lets, letk

ibanbit = 1
ierror = 0
m = 0

OPEN (UNIT = 780, FILE = RUT_RES//'VariablesModelMILP.txt', &
      IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Generacion de despacho unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Generacion de sincronizacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Asignacion de operacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Asignacion de sincronizacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Arranque de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Paro de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva rodante de 10 minutos de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva no rodante de 10 minutos de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva rodante suplementaria de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva no rodante suplementaria de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
!   Para todas las unidades de rango continuo
    do u = 1 , NumUniRC
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva regulacion secundaria de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
!   para todas las unidades de rango continuo
do u = 1 , NumUniRC
!   Para todos los intervalos
    do i = 1 , NTINTR
        write ( leti, 200 ) i
        do s = 1, NumBloVRC( u, i )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Generacion en cada bloque de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! para todas las unidades de rango continuo
do u = 1 , NumUniRC
!   Para todos los intervalos
    do i = 1 , NTINTR
        write ( leti, 200 ) i
        do s = 1, NmBloArrURC( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Bloque de oferta de arranque de unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo

if ( SiBandProh .eq. 1 ) then
!   para todas las unidades de rango continuo
    do u = 1 , NumUniRC
!       Para todos los intervalos
        do i = 1 , NTINTR
            write ( leti, 200 ) i
            do s = 1,NoRaOpRC ( u )
                m = m + 1
                write ( lets, 200 ) s
                write ( 780,* ) m,',', '"Binaria de rango operativo unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//' rango: '//trim(lets)//'"'//'"'
            enddo
        enddo
    enddo
endif

do u = 1, NumUniRC
    do i = 1, NTINTR
        write ( leti, 200 ) i
!       si existe oferta de regulacion de la unidad
        if ( OferResRegRC ( u, i ) .gt. 0 ) then
!           si se desea considerar bandas prohibidas y la unidad tiene
            if ( .not. (SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0)  ) then
                m = m + 1
                write ( 780,* ) m,',', '"Binarias Regulacion Secundaria unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        endif
    enddo
enddo

do u = 1, NumUniRC
    do i = 1, NTINTR
        write ( leti, 200 ) i
!       si existe oferta de regulacion de la unidad
        if ( OferResRegRC ( u, i ) .gt. 0 ) then
!           si se desea considerar bandas prohibidas y la unidad tiene
            if ( SiBandProh .eq. 1 .and. NoRaOpRC ( u ) .gt. 0 ) then
                do ro = 1, NoRaOpRC ( u )
                    m = m + 1
                    write ( lets, 200 ) ro
                    write ( 780,* ) m,',', '"Binarias Regulacion Secundaria por rango operativo unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//' rango: '//trim(lets)//'"'//'"'
                enddo
            endif
        endif
    enddo
enddo
!
!----------------------------------------------------------------------------------------
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Generacion de operacion unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Generacion durante el arranque unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Asignacion de operacion unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo

! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Asignacion durante el arranque unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Asignacion reserva no rodante unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Arranque unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do k = 1, NumModRD( u )
            write ( letk, 200 ) k
            do s = 1, NumModRD( u )
                m = m + 1
                write ( lets, 200 ) s
                write ( 780,* ) m,',', '"inicio de operación para modos no arrancables unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(letk)//' a modo: '//trim(lets)//'"'//'"'
            enddo
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do i = 1,  NTINTR
    write ( leti, 200 ) i
    do u = 1, NumUniRD
         m = m + 1
         write ( lets, 200 ) s
         write ( 780,* ) m,',', '"Paro unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Unidades de rango discontinuo
do i = 1,  NTINTR
    write ( leti, 200 ) i
    do u = 1, NumUniRD
         m = m + 1
         write ( lets, 200 ) s
         write ( 780,* ) m,',', '"Costo de transicion unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Reserva rodante de 10 minutos unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Reserva no rodante de 10 minutos unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Reserva rodante suplementaria unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Reserva no rodante suplementaria unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do s = 1, NumModRD( u )
            m = m + 1
            write ( lets, 200 ) s
            write ( 780,* ) m,',', '"Reserva regulacion secundaria unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do k = 1, NumModRD( u )
            write ( letk, 200 ) k
            do s = 1,  NumBloVRD( u, k, i )
                m = m + 1
                write ( lets, 200 ) s
                write ( 780,* ) m,',', '"Generación en cada bloque unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(letk)//'bloque: '//trim(lets)//'"'//'"'
            enddo
        enddo
    enddo
enddo
! Unidades de rango discontinuo
do u = 1, NumUniRD
    do i = 1,  NTINTR
        write ( leti, 200 ) i
        do k = 1, NumModRD( u )
            write ( letk, 200 ) k
            do s = 1,  NumBloVRD( u, k, i )
                m = m + 1
                write ( lets, 200 ) s
                write ( 780,* ) m,',', '"Oferta de arranque en cada bloque unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(letk)//'bloque: '//trim(lets)//'"'//'"'
            enddo
        enddo
    enddo
enddo
do u = 1, NumUniRD
    do i = 1, NTINTR
        write ( leti, 200 ) i
!       para todos los modos de operacion
        do s = 1, NumModRD(u)
!           si existe oferta de regulacion de la unidad
            if ( CalOferResRegRD ( u, s, i ) .gt. 0.0 ) then
                m = m + 1
                write ( lets, 200 ) s
                write ( 780,* ) m,',', '"Binarias de reserva regulacion secundaria unidad de RD: '//trim(nombunird(u))//' intervalo: '//trim(leti)//' modo: '//trim(lets)//'"'//'"'
            endif
        enddo
    enddo
enddo
!------------------------------------------------------------------------------------------------
! Unidades Hidro
! Para todos los intervalos
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Generacion de despacho unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Asignacion unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Arranque unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Paro unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva rodante de 10 minutos unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva no rodante de 10 minutos unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva rodante rodante suplementaria unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva no rodante suplementaria unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
do i = 1 , NTINTR
!   para todas las unidades hidro
    do u = 1 , NumUniHid
        m = m + 1
        write ( leti, 200 ) i
        write ( 780,* ) m,',', '"Reserva regulacion secundaria unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo

if ( SiBandProh .eq. 1 ) then
!   para todas las unidades de rango continuo
    do u = 1 , NumUniHid
!       Para todos los intervalos
        do i = 1 , NTINTR
            write ( leti, 200 ) i
            do s = 1, NoRaOpH ( u )
                m = m + 1
                write ( lets, 200 ) s
                write ( 780,* ) m,',', '"Binaria de rango operativo de la unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//' rango: '//trim(lets)//'"'//'"'
            enddo
        enddo
    enddo
endif
do u = 1, NumUniHid
    do i = 1, NTINTR
!       si existe oferta de regulacion de la unidad
        if ( OferResRegH ( u, i ) .gt. 0 ) then
!           si no se desea considerar bandas prohibidas o la unidad no las tiene
            if ( not (SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0) ) then
                m = m + 1
                write ( leti, 200 ) i
                write ( 780,* ) m,',', '"Binarias de regulacion secundaria unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
            endif
        endif
    enddo
enddo
do u = 1, NumUniHid
    do i = 1, NTINTR
        write ( leti, 200 ) i
!       si existe oferta de regulacion de la unidad
        if ( OferResRegH ( u, i ) .gt. 0 ) then
!           si se desea considerar bandas prohibidas y la unidad las tiene
            if ( SiBandProh .eq. 1 .and. NoRaOpH ( u ) .gt. 0 ) then
                do ro = 1, NoRaOpH ( u )
                    m = m + 1
                    write ( lets, 200 ) ro
                    write ( 780,* ) m,',', '"Binarias de regulacion secundaria rango operativo unidad Hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//' rango: '//trim(lets)//'"'//'"'
                enddo
            endif
        endif
    enddo
enddo
!----------------------------------------------------------------------------------
! Renovables
! Para todos los intervalos
do i = 1 , NTINTR
    write ( leti, 200 ) i
!   para todas las unidades renovables
    do u = 1 , NumUniRE
        m = m + 1
        write ( 780,* ) m,',', '"Generacion de despacho unidad renovable: '//trim(nombunire(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
    write ( leti, 200 ) i
!   para todas las unidades renovables
    do u = 1 , NumUniRE
        m = m + 1
        write ( 780,* ) m,',', '"Asignacion de despacho unidad renovable: '//trim(nombunire(u))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
!   para todas las unidades renovables
do u = 1 , NumUniRE
!   Para todos los intervalos
    do i = 1 , NTINTR
        write ( leti, 200 ) i
        do s = 1, NumBloVRE( u, i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Generacion en cada bloque de unidad renovable: '//trim(nombunire(u))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
!---------------------------------------------------------------------------------
!Renovables
! Para todos los intervalos
do i = 1 , NTINTR*0
    write ( leti, 200 ) i
!   para todas las cargas
    do d = 1 , Numoferdem
        m = m + 1
        write ( 780,* ) m,',', '"Reserva de 10 minutos de carga: '//trim(nombcar(d))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR*0
    write ( leti, 200 ) i
!   para todas las cargas
    do d = 1 , Numoferdem
        m = m + 1
        write ( 780,* ) m,',', '"Reserva suplementaria de carga: '//trim(nombcar(d))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todos los intervalos
do i = 1 , NTINTR
    write ( leti, 200 ) i
!   para todas las cargas
    do d = 1, Numoferdem
        m = m + 1
        write ( 780,* ) m,',', '"Corte de carga: '//trim(nombcar(d))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
! Para todas las cargas
do d = 1 , Numoferdem
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloDem( d, i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Corte de carga por bloque: '//trim(nombcar(d))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas las zonas de reserva
do r = 1 , NumGruRes*SiOferComResZona
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloRR10( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva rodante de 10 minutos aceptada por zona CENACE: '//trim(NomZonaRes ( r ))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas las zonas de reserva
do r = 1 , NumGruRes*SiOferComResZona
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloR10( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva de 10 minutos aceptada por zona CENACE: '//trim(NomZonaRes ( r ))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas las zonas de reserva
do r = 1 , NumGruRes*SiOferComResZona
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloRSu( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva supementaria aceptada por zona CENACE: '//trim(NomZonaRes ( r ))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas las zonas de reserva
do r = 1 , NumGruRes*SiOferComResZona
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloRReg( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva de regulacion aceptada por zona CENACE: '//trim(NomZonaRes ( r ))//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo

! Para todas los sistemas
do is = 1, numsis*SiOferComResSis
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloRR10( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva rodante de 10 aceptada por sistema CENACE: '//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas los sistemas
do is = 1, numsis*SiOferComResSis
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloR10( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva de 10 aceptada por sistema CENACE: '//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas los sistemas
do is = 1, numsis*SiOferComResSis
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloRsu( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva suplementaria aceptada por sistema CENACE: '//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
! Para todas los sistemas
do is = 1, numsis*SiOferComResSis
!   para todas los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        do s = 1, NumBloRReg( i )
            write ( lets, 200 ) s
            m = m + 1
            write ( 780,* ) m,',', '"Reserva de regulacion aceptada por sistema CENACE: '//' intervalo: '//trim(leti)//' bloque: '//trim(lets)//'"'//'"'
        enddo
    enddo
enddo
!--------------------------------------------------------------------------------
!   para todos los intervalos
do i = 1, NTINTR 
    write ( leti, 200 ) i
!   para todos los nodos
    do k = 1, numnodos
       m = m + 1
       write ( 780,* ) m,',', '"Excedente de generacion de nodo: '//trim(Nomnod ( k ))//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo
!   para todos los intervalos
do is = 1, numsis
    do i = 1, NTINTR 
        write ( leti, 200 ) i
    !   para todos los grupos de ramas
        do k = 1, NumGruRamSis ( is )
           m = m + 1
           write ( 780,* ) m,',', '"Excedente de flujo de grupo de ramas: '//trim(nomgruram(k))//' intervalo: '//trim(leti)//'"'//'"'
        enddo
     enddo
enddo
! para todos los sistemas
do is = 1, numsis
    !   para todos los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
    !   para todos los grupos de ramas
        do k = 1, NumGruRamSis ( is )
           m = m + 1
           write ( 780,* ) m,',', '"Excedente de contra flujo de grupo de ramas: '//trim(nomgruram(k))//' intervalo: '//trim(leti)//'"'//'"'
        enddo
    enddo
enddo

! para todos los grupos termicos
do k = 1, NumGruUTer*SiEnerTer
    m = m + 1
    write ( 780,* ) m,',', '"Excedente de energia de grupo termico: '//trim(NomGpoTer(k))//'"'//'"'
enddo

! para todas las restricciones con limitacion de energia
do grupo = 1, NResEner*SiEnerTer*0
    write ( leti, 200 ) grupo
    m = m + 1
    write ( 780,* ) m,',', '"Variable artificial de limite de energia : '//trim(NomGpoResEner(grupo))//' restriccion : '//trim(leti)//'"'//'"'
enddo
! para todos los embalses
do k = 1, NumEmbalses !*SiEnerHid
    m = m + 1
    write ( 780,* ) m,',', '"Excedente de energia de embalse: '//trim(NOMEMB(k))//'"'//'"'
enddo

! para todos los sistemas
do is = 1, numsis
    !   para todos los intervalos
    do i = 1, NTINTR 
        write ( leti, 200 ) i
        m = m + 1
        write ( 780,* ) m,',', '"Variable de estimacion de perdidas: '//' intervalo: '//trim(leti)//'"'//'"'
    enddo
enddo

! Variables de turbinado hidro
do i = 1, NTINTR*SiModHid
   do u = 1, NumUniHid
        write ( leti, 200 ) i
        m = m + 1
        write ( 780,* ) m,',', '"Variable de turbinado hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
   enddo
enddo

! Variables de volumen hidro en embalse
do i = 1, NTINTR*SiModHid
   do ie = 1, NumEmbalses
        write ( leti, 200 ) i
        m = m + 1
        write ( 780,* ) m,',', '"Variable de volumen hidro en emblalse: '//trim(nomemb(ie))//' intervalo: '//trim(leti)//'"'//'"'
   enddo
enddo

! Variables de deficit en politica hidro en embalse

do ie = 1, NumEmbalses*SiModHid
   m = m + 1
   write ( 780,* ) m,',', '"Variable de deficit en politica hidro en emblalse: '//trim(nomemb(ie))//'"'//'"'
enddo


! Variables de excedente en politica hidro en embalse
do ie = 1, NumEmbalses*SiModHid
   write ( leti, 200 ) i
   m = m + 1
   write ( 780,* ) m,',', '"Variable de excedente en politica hidro en emblalse: '//trim(nomemb(ie))//'"'//'"'
enddo

! Variables de excedente en balance hidro en embalse
do i = 1, NTINTR*SiModHid
   do ie = 1, NumEmbalses
        write ( leti, 200 ) i
        m = m + 1
        write ( 780,* ) m,',', '"Variable de excedente en balance hidro en emblalse: '//trim(nomemb(ie))//' intervalo: '//trim(leti)//'"'//'"'
   enddo
enddo

! Variables de deficit en balance hidro en embalse
do i = 1, NTINTR*SiModHid
   do ie = 1, NumEmbalses
        write ( leti, 200 ) i
        m = m + 1
        write ( 780,* ) m,',', '"Variable de deficit en balance hidro en emblalse: '//trim(nomemb(ie))//' intervalo: '//trim(leti)//'"'//'"'
   enddo
enddo

! para todos los grupos con limitacion de combustible
do grupo = 1, NumGruGas*SiGpoGas
!   para todos los dias del horizonte
    do dia = 1, DURDIA
        write ( leti, 200 ) dia
        m = m + 1
        write ( 780,* ) m,',', '"Variable artificial de limite de combustible : '//trim(NomGpoGas(grupo))//' dia      : '//trim(leti)//'"'//'"'
    enddo
enddo

! para todas las restricciones con limitacion de combustible
do grupo = 1, NResComb*SiGpoGas*0
    write ( leti, 200 ) grupo
    m = m + 1
    write ( 780,* ) m,',', '"Variable artificial de limite de combustible : '//trim(NomGpoResComb(grupo))//' restricc : '//trim(leti)//'"'//'"'
enddo

if ( TipoEjecu .le. 1 ) then
	! Para todos los intervalos
	do i = 1 , NTINTR
	!   para todas las unidades de rango continuo
	    do u = 1 , NumUniRC
	        m = m + 1
	        write ( leti, 200 ) i
	        write ( 780,* ) m,',', '"Uso de reserva rodante de 10 unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
	    enddo
	enddo
	
	! Para todos los intervalos
	do i = 1 , NTINTR
	!   para todas las unidades de rango continuo
	    do u = 1 , NumUniRC
	        m = m + 1
	        write ( leti, 200 ) i
	        write ( 780,* ) m,',', '"Uso de reserva de regulacion unidad de RC: '//trim(nombunirc(u))//' intervalo: '//trim(leti)//'"'//'"'
	    enddo
	enddo
	
	! Para todos los intervalos
	do i = 1 , NTINTR
	!   para todas las unidades hidro
	    do u = 1 , NumUniHid
	        m = m + 1
	        write ( leti, 200 ) i
	        write ( 780,* ) m,',', '"Uso de reserva de regulacion unidad hidro: '//trim(nombunih(u))//' intervalo: '//trim(leti)//'"'//'"'
	    enddo
	enddo
endif
close (780)

200 format ( i3 )

return
end
    
subroutine CotaReqRes10 ( k, m )
! ---------------------------------------------------------------------
! Se determina la cota superior de la variable de asignacion de       *
! reserva de 10 minutos.                                              *
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

integer i, k, m, r, s, k10, kr10
character*3 leti
          
kr10 = ICARR10G
k10 = ICAR10G

IRESRE10 = m + 1
write ( 777,* ) 'Inicia restricciones de cota superior de req res 10 min por zona :', m + 1

! para los grupos de reserva
do r = 1, NumGruRes*SiOferComResZona
!   para todos los intervalos
    do i = 1, NTINTR
!       para los requerimientos de reserva de 10 minutos
        do s = 1, NumBloR10( i )
!           coeficiente de la variable de reserva de 10 aceptada por CENACE
            aaMILP ( k  ) = -1.0
            jcolMILP ( k ) = k10
            k = k + 1
            k10 = k10 + 1
!           coeficiente de la variable de reserva rodante de 10 aceptada por CENACE
            aaMILP ( k  ) = -1.0
            jcolMILP ( k ) = kr10
            k = k + 1
            kr10 = kr10 + 1
!           lado derecho de la restricion
            m = m + 1
            bMILP ( m ) = -ReqRes10 (r, s, i) - ReqResR10 (r, s, i)
!           sentidos de la restricion
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Cota supe req del CENACE reserva 10 min porzona '//trim(NomZonaRes(r))//' intervalo '//trim(leti)//'"'
        enddo
    enddo
enddo

200 format (i3)

return
end
