! ---------------------------------------------------------------------
! Modulo de definicion de variables globales que seran empleadas en el*
! problema de asignacion y despacho del problema de AUHE (MILP y LP)  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2020                                                     *
! ---------------------------------------------------------------------
!
Module ProblemaAUHE

use ParAUHE

implicit none

INTEGER IGRC, IGDARC, IARC, IADARC, IARRC, IPRC, IRR10RC, IRNR10RC, &
        IRRSRC, IRNRSRC, IRRERC, IGABRC, IBOARC, SiCorte, SiExced, &
        SiUniRC, SiUniRD, SiUniH, SiUniRE, SiOferDem, SiOferRes, &
        SiOferComResZona, SiOferComResSis, SiEnerHid, SiEnerTer, &
        SiTransmision, SiArtTrans, SiOferDemCon, SiPerdidas,  &
        SiSolucionInicial, IterPerdidas, SiBandProh, SiModHid, &
        TipoOferta, TipoEscPre, SiRefHid, SiGpoGas, SiEscCorExc, &
        SiSegProb, TipoProblema, SiCalPen, SiEscLP, SiEscalaConex

INTEGER IGRD, IGDARD, IARD, IADARD, IARRD, IOMARD, IPRD, &
        IRR10RD, IRNR10RD, IRRSRD, IRNRSRD, IRRERD, IGABRD, IBOARD, &
        IARNRRD, INIURDI, INBURD, INALPHAT, INBAURD, INDDE, IARCG

INTEGER IGH, IAH, IARH, IPH, IRR10H, IRNR10H, IRRSH, IRNRSH, IRREH

INTEGER IGRE, IARE, IRTRANS

INTEGER ICC10, ICCS, IDF, IDBC

INTEGER ICARR10G, ICAR10G, ICARSG, ICARRG, &
        ICARR10S, ICAR10S, ICARSS, ICARRS

INTEGER IAREE, IGABRE, IABPRC, IABPH, INVBPRC, INVBPH

INTEGER IEXC, IAEF, IACF, IARGT, IPERD

INTEGER IRND, IRBAL, IRR10Z, IR10Z, IRSZ, IREZ, IRR10S, IR10S, IRSS, &
        IRES, IRTANS, IRPERD, IVREGRC, INREGRC, INVRERORC, IRERORC, &
        INREGH, INVREROH, IREROH, IVREGH, IVREGRD, INREGRD, IRGUNH, IRTUNH

INTEGER ITURB, IVOLU, IDPOL, IEPOL, IEBAL, IDBAL

INTEGER NumVarAsig, jcolMILP, irowMILP, NumResAsig, nelemMaMILP
INTEGER SiViolacion, NumResAdi, InfRestAdi, IntRestAdi, IRLMRC, IRLMRD, &
        IRLMH, SiLimReg, SiArrNoSimul, SiResRegDis, IRGPOGAS, IRGPTER, IRESRE10

integer ApResEner, APResLimSH, APResLimIH

REAL*8  objMILP, aaMILP, bMILP, lbMILP, ubMILP, xMILP
REAL*8  dualbalance, dualresr10s, dualres10s, dualresss, dualresres
REAL*8  dualresr10z, dualres10z, dualressz, dualresrez, dualperdidas
real*8  dualenerbal, dualsgpogas, dualigpogas, dualigpter, dualsgpter
real*8  dualsupgpogas, dualinfgpogas, dualinfener, dualsupener

REAL*8  GAPCPLEX, LIMIT_TIME_LINEAR
CHARACTER*1  sMILP, ctypeMILP, IsenRestAdi

integer(8) lpMILP

INTEGER SemBandera

REAL*8  PrecioTopeEner, PrecioTopeEM, PrecioTopeArr, PrecioTopeReg, &
        PrecioTopeRod, PrecioTopeOper, PrecioTopeSup, epsilonreserva, &
        PorcenCorte


! Variables de Unidades de Rango Continuo

! Inicio de variables de generacion de unidades de rango continuo
common / IGRC  / IGRC

! Inicio de variables de generacion durante el arranque de unidades de rango continuo
common / IGDARC  / IGDARC

! Inicio de variables de asignacion de unidades de rango continuo
common / IARC  / IARC

! Inicio de variables de asignacion durante el arranque de unidades de rango continuo
common / IADARC  / IADARC

! Inicio de variables de arranque de unidades de rango continuo
common / IARRC  / IARRC

! Inicio de variables de paro de unidades de rango continuo
common / IPRC  / IPRC

! Inicio de variables de reserva rodante aceptada de 10 minutos para unidades de rango continuo
common / IRR10RC  / IRR10RC

! Inicio de variables de reserva no rodante aceptada de 10 minutos para unidades de rango continuo
common / IRNR10RC  / IRNR10RC

! Inicio de variables de reserva rodante suplementaria aceptada  para unidades de rango continuo
common / IRRSRC  / IRRSRC

! Inicio de variables de reserva no rodante suplementaria aceptada para unidades de rango continuo
common / IRNRSRC  / IRNRSRC

! Inicio de variables de reserva de regulación secundaria aceptada para unidades de rango continuo
common / IRRERC  / IRRERC

! Inicio de variables de generación aceptada en cada bloque para unidades de rango continuo
common / IGABRC  / IGABRC

! Inicio de variables de bloque de oferta de arranque de unidades de rango continuo
common / IBOARC  / IBOARC

! Variables de Unidades de Rango Discontinuo

! Inicios de variables de unidades de rango discontinuo, e intervalo
common / INIURDI  / INIURDI  ( maxurd, maxint )

! Inicios de variables de unidades de rango discontinuo, e intervalo para la variable alphat
common / INALPHAT  / INALPHAT  ( maxurd, maxint )

! Inicios de variables de unidades de rango discontinuo, modo e intervalo
common / INBURD  / INBURD  ( maxurd, maxmodos, maxint )

! Inicios para arranque de variables de unidades de rango discontinuo, modo e intervalo
common / INBAURD  / INBAURD  ( maxurd, maxmodos, maxint )

! Inicio de variables de generacion de unidades de rango discontinuo
common / IGRD  / IGRD

! Inicio de variables de generacion durante el arranque de unidades de rango discontinuo
common / IGDARD  / IGDARD

! Inicio de variables de asignacion de unidades de rango discontinuo
common / IARD  / IARD

! Inicio de variables de asignacion durante el arranque de unidades de rango discontinuo
common / IADARD  / IADARD

! Inicio de variables de asignacion de reserva no rodante de unidades de rango discontinuo
common / IARNRRD  / IARNRRD

! Inicio de variables de arranque de unidades de rango discontinuo
common / IARRD  / IARRD

! Inicio de variables de inicio de operacion para modos no arrancables de unidades de rango discontinuo
common / IOMARD  / IOMARD

! Inicio de variables de paro de unidades de rango dicontinuo
common / IPRD  / IPRD

! Inicio de variables de reserva rodante aceptada de 10 minutos para unidades de rango discontinuo
common / IRR10RD  / IRR10RD

! Inicio de variables de reserva no rodante aceptada de 10 minutos para unidades de rango discontinuo
common / IRNR10RD  / IRNR10RD

! Inicio de variables de reserva rodante suplementaria aceptada  para unidades de rango discontinuo
common / IRRSRD  / IRRSRD

! Inicio de variables de reserva no rodante suplementaria aceptada para unidades de rango discontinuo
common / IRNRSRD  / IRNRSRD

! Inicio de variables de reserva de regulación secundaria aceptada para unidades de rango discontinuo
common / IRRERD  / IRRERD

! Inicio de variables de generación aceptada en cada bloque para unidades de rango discontinuo
common / IGABRD  / IGABRD

! Inicio de variables de bloque de oferta de arranque de unidades de rango discontinuo
common / IBOARD  / IBOARD

! Variables de Unidades Hidro

! Inicio de variables de generacion de unidades hidro
common / IGH  / IGH

! Inicio de variables de asignacion de unidades hidro
common / IAH  / IAH

! Inicio de variables de arranque de unidades hidro
common / IARH  / IARH

! Inicio de variables de paro de unidades hidro
common / IPH  / IPH

! Inicio de variables de reserva rodante aceptada de 10 minutos para unidades hidro
common / IRR10H  / IRR10H

! Inicio de variables de reserva no rodante aceptada de 10 minutos para unidades hidro
common / IRNR10H  / IRNR10H

! Inicio de variables de reserva rodante suplementaria aceptada  para unidades hidro
common / IRRSH  / IRRSH

! Inicio de variables de reserva no rodante suplementaria aceptada para unidades hidro
common / IRNRSH  / IRNRSH

! Inicio de variables de reserva de regulación secundaria aceptada para unidades hidro
common / IRREH  / IRREH


! Variables de Unidades Renovables

! Inicio de variables de generacion de unidades renovables
common / IGRE  / IGRE

! Inicio de variables de asignacion de unidades renovables
common / IARE  / IARE

! Inicio de variables de generacion aceptada en cada bloque para unidades renovables
common / IGABRE  / IGABRE


! Variables de demandas (cargas)

! Inicio de variables de cargas controlables de 10 minutos
common / ICC10  / ICC10

! Inicio de variables de cargas controlables suplementarias
common / ICCS  / ICCS

! Inicio de variables de corte de carga
common / IDF  / IDF

! Inicio de variables de demanda del bloque de compra de energia
common / IDBC  / IDBC

! Inicio de variables de oferta de demanda en los intervalos
common / INDDE  / INDDE ( maxdem, maxint )

! Variables de requerimientos del CENACE

! Inicio de variables de cantidad aceptada de reserva rodante de 10 minutos del CENACE, para los grupos
common / ICARR10G  / ICARR10G

! Inicio de variables de cantidad aceptada de reserva de 10 minutos del CENACE, para los grupos
common / ICAR10G  / ICAR10G

! Inicio de variables de cantidad aceptada de reserva suplementaria del CENACE, para los grupos
common / ICARSG  / ICARSG

! Inicio de variables de cantidad aceptada de reserva de regulacion del CENACE, para los grupos
common / ICARRG  / ICARRG

! Inicio de variables de cantidad aceptada de reserva rodante de 10 minutos del CENACE, para el sistema
common / ICARR10S  / ICARR10S

! Inicio de variables de cantidad aceptada de reserva de 10 minutos del CENACE, para el sistema
common / ICAR10S  / ICAR10S

! Inicio de variables de cantidad aceptada de reserva suplementaria del CENACE, para el sistema
common / ICARSS  / ICARSS

! Inicio de variables de cantidad aceptada de reserva de regulación del CENACE, para el sistema
common / ICARRS  / ICARRS

! Variables de artificiales

! Inicio de variables de excedentes de energia en nodos
common / IEXC  / IEXC

! Inicio de variables artificiales de excedente de flujo
common / IAEF  / IAEF

! Inicio de variables artificiales de excedente de contraflujo
common / IACF  / IACF

! Inicio de variables artificiales de excedente de energia fija en grupos termicos
common / IARGT  / IARGT

! Inicio de variables artificiales de excedente de energia fija en embalses
common / IAREE  / IAREE

! Inicio de variables para estimacion de perdidas en transmision
common / IPERD  / IPERD

! Inicio de variables de asignacion de bandas prohibidas de unidades de Rango Continuo
common / IABPRC  / IABPRC

! Inicio de variables de asignacion de bandas prohibidas de unidades Hidro
common / IABPH  / IABPH

! Inicios de bandas prohibidas de unidades de Rango Continuo
common / INVBPRC  / INVBPRC  ( maxurc )

! Inicio de binarias de regulacion de unidades de Rango Continuo sin bandas prohibidas
common / IVREGRC  / IVREGRC

! Apuntadores a variables binarias de regulacion de unidades de Rango Continuo sin bandas prohibidas
common / INREGRC  / INREGRC  ( maxurc, maxint )

! Inicio de binarias de regulacion de unidades de Rango Continuo con bandas prohibidas
common / INVRERORC  / INVRERORC

! Apuntadores a variables binarias de regulacion de unidades de Rango Continuo con bandas prohibidas
common / IRERORC  / IRERORC  ( maxurc, maxint )

! Inicio de binarias de regulacion de unidades hidro sin bandas prohibidas
common / IVREGH  / IVREGH

! Apuntadores a variables binarias de regulacion de unidades hidro sin bandas prohibidas
common / INREGH  / INREGH  ( maxuh, maxint )

! Inicio de binarias de regulacion de unidades hidro con bandas prohibidas
common / INVREROH  / INVREROH

! Apuntadores a variables binarias de regulacion de unidades hidro con bandas prohibidas
common / IREROH  / IREROH  ( maxuh, maxint )

! Inicios de bandas prohibidas de unidades Hidro
common / INVBPH  / INVBPH  ( maxuh )

! Inicio de binarias de regulacion de unidades de Rango Discontinuo
common / IVREGRD  / IVREGRD

! Apuntadores a variables binarias de regulacion de unidades de Rango Discontinuo
common / INREGRD  / INREGRD  ( maxurd, maxmodos, maxint )


! Inicios de tipos de restricciones

! Inicio de nivel de demanda
common / IRND / IRND

! Inicio de balance de potencia
common / IRBAL / IRBAL

! Inicio de reserva rodante de 10 minutos por zona
common / IRR10Z / IRR10Z

! Inicio de reserva de 10 minutos por zona
common / IR10Z / IR10Z

! Inicio de reserva suplementaria por zona
common / IRSZ / IRSZ

! Inicio de reserva de regulacion por zona
common / IREZ / IREZ

! Inicio de reserva rodante de 10 minutos por sistema
common / IRR10S / IRR10S

! Inicio de reserva de 10 minutos por sistema
common / IR10S / IR10S

! Inicio de reserva suplementaria por sistema
common / IRSS / IRSS

! Inicio de reserva de regulacion por sistema
common / IRES / IRES

! Inicio de restricciones de transmision
common / IRTRANS / IRTRANS

! Inicio de restricciones de perdidas
common / IRPERD / IRPERD

! Inicio de restricciones de funcion de generacion hidro
common / IRGUNH / IRGUNH

! Inicio de restricciones de trubinado hidro
common / IRTUNH / IRTUNH

! Inicio de variables de volumen turbinado
common / ITURB / ITURB

! Inicio de variables de volumen almacenado
common / IVOLU / IVOLU

! Inicio de variables de deficit en politica
common / IDPOL / IDPOL

! Inicio de variables de excedente en politica
common / IEPOL / IEPOL

! Inicio de variables de excedente en balance
common / IEBAL / IEBAL

! Inicio de variables de deficit en balance
common / IDBAL / IDBAL

! Inicio de variables artificiales de consumo de combustible
common / IARCG / IARCG


! Informacion sobre la dimension del problema de asignacion 

! Numero de variables decision en el problema de asignacion (MILP)
common / NumVarAsig  / NumVarAsig

! Numero de restricciones en el problema de asignacion (MILP)
common / NumResAsig  / NumResAsig

! Numero de restricciones adicionales (transmision y/o perdidas) en el problema de asignacion (MILP)
common / NumResAdi  / NumResAdi

! Numero de elementos distintos de cero en la matriz de restricciones del MILP
common / nelemMaMILP  / nelemMaMILP

! Coeficientes en la funcion objetivo en el problema de asignacion (MILP)
common / objMILP    / objMILP          ( maxvarMILP )

! Tipos de las variables en el problema de asignacion (MILP)
common / ctypeMILP    / ctypeMILP      ( maxvarMILP )

! cotas inferiores del problema de asignacion (MILP)
common / lbMILP    / lbMILP            ( maxvarMILP )

! cotas superiores del problema de asignacion (MILP)
common / ubMILP    / ubMILP            ( maxvarMILP )

! coeficientes de las restricciones en el problema de asignacion (MILP)
common / aaMILP    / aaMILP            ( maxresMILP*25 )

! indicador de la variable asociada a cada coeficiente de las restricciones en el problema de asignacion (MILP)
common / jcolMILP    / jcolMILP        ( maxresMILP*25)

! lados derechos de las restricciones en el problema de asignacion (MILP)
common / bMILP    / bMILP              ( maxresMILP )

! sentidos de las restricciones en el problema de asignacion (MILP)
common / sMILP    / sMILP              ( maxresMILP )

! indicador en el vector aaMILP del inicio de las restricciones en el problema de asignacion (MILP)
common / irowMILP / irowMILP           ( maxresMILP )

! indicador de tipo de restriccion adicional en el problema de asignacion (MILP) (0=perdidas, =! 0 un grupo de rama)
common / InfRestAdi / InfRestAdi       ( maxresMILP )

! indicador de intervalo de restriccion adicional en el problema de asignacion (MILP)
common / IntRestAdi / IntRestAdi       ( maxresMILP )

! indicador de sentido de restriccion adicional en el problema de asignacion (MILP)
common / IsenRestAdi / IsenRestAdi     ( maxresMILP )

! numero de problema asociado al problema maestro binario-mixto lineal Windows 8.1
common / lpMILP / lpMILP

! indicador de violacion en las restricciones de transmision
common / SiViolacion    / SiViolacion

! solucion primal del problema de asignacion (MILP)
common / xMILP    / xMILP            ( maxvarMILP )

! solucion dual del problema MILP (fijando las variables enteras)

!duales de balance
common / dualbalance / dualbalance ( maxsis, maxint )

!duales de reserva rodante de 10 minutos por zona
common / dualresr10z / dualresr10z ( maxgrure, maxint )

!duales de reserva de 10 minutos por zona
common / dualres10z / dualres10z ( maxgrure, maxint )

!duales de reserva suplementaria por zona
common / dualressz / dualressz ( maxgrure, maxint )

!duales de reserva de regulacion por zona
common / dualresrez / dualresrez ( maxgrure, maxint )


!duales de reserva rodante de 10 minutos por sistema
common / dualresr10s / dualresr10s ( maxsis, maxint )

!duales de reserva de 10 minutos por sistema
common / dualres10s / dualres10s ( maxsis, maxint )

!duales de reserva suplementaria por sistema
common / dualresss / dualresss ( maxsis, maxint )

!duales de reserva de regulacion por sistema
common / dualresres / dualresres ( maxsis, maxint )

!duales de restricciones de perdidas
common / dualperdidas / dualperdidas ( maxsis, maxint )

! Multiplicador asociado restricciones de energia maxima en los embalses
common / dualenerbal   / dualenerbal   ( nmxemb )

! Multiplicador asociado a las restricciones de limite superior de combustible
common / dualsgpogas   / dualsgpogas   ( maxgrute, maxdia  )

! Multiplicador asociado a las restricciones de limite inferior de combustible
common / dualigpogas   / dualigpogas   ( maxgrute, maxdia )

! Multiplicador asociado a las restricciones de limite superior de combustible
common / dualsupgpogas   / dualsupgpogas   ( maxgrute*15 )

! Multiplicador asociado a las restricciones de limite inferior de combustible
common / dualinfgpogas   / dualinfgpogas   ( maxgrute*15 )

! Multiplicador asociado a las restricciones de limite superior de energia termo
common / dualsgpter   / dualsgpter   ( maxgrute, maxdia )

! Multiplicador asociado a las restricciones de limite inferior de energia termo
common / dualigpter   / dualigpter   ( maxgrute, maxdia )

! Multiplicador asociado a las restricciones de limite inferior de energia termo
common / dualinfener   / dualinfener   ( maxgrute*15 )

! Multiplicador asociado a las restricciones de limite superior de energia termo
common / dualsupener   / dualsupener   ( maxgrute*15 )

! Apuntador de inicio de restricciones de limites de energia termo
common / IRGPTER / IRGPTER

! Apuntador asociado a las restricciones de energia maxima en los embalses
common / ApResEner   / ApResEner   ( nmxemb )

! Apuntador asociado a las restricciones de limite superior de energia hidro
common / APResLimSH   / APResLimSH

! Apuntador asociado a las restricciones de limite inferior de energia hidro
common / APResLimIH   / APResLimIH

! parametros temporales de sintonizacion

!bandera para activar cortes ( 1 = activa, 0 = inactiva )
common / SiCorte / SiCorte

!bandera para activar excedentes ( 1 = activa, 0 = inactiva )
common / SiExced / SiExced

!bandera para activar unidades de Rango Continuo ( 1 = si, 0 = no )
common / SiUniRC / SiUniRC

!bandera para activar unidades de Rango discontinuo ( 1 = si, 0 = no )
common / SiUniRD / SiUniRD

!bandera para activar unidades Hidro ( 1 = si, 0 = no )
common / SiUniH / SiUniH

!bandera para activar unidades renovables ( 1 = si, 0 = no )
common / SiUniRE / SiUniRE

!bandera para activar ofertas de demanda ( 1 = si, 0 = no )
common / SiOferDem / SiOferDem

!bandera para activar ofertas de reserva ( 1 = si, 0 = no )
common / SiOferRes / SiOferRes

!bandera para escribir a archivo el detalle de cortes/excdentes por nodo ( 1 = si, 0 = no )
common / SiEscCorExc / SiEscCorExc

!bandera para activar ofertas de compra de reserva por zona ( 1 = si, 0 = no )
common / SiOferComResZona / SiOferComResZona

!bandera para activar ofertas de compra de reserva por sistema ( 1 = si, 0 = no )
common / SiOferComResSis / SiOferComResSis

!bandera para activar ofertas de demanda controlable ( 1 = si, 0 = no )
common / SiOferDemCon / SiOferDemCon

!bandera para activar restricciones de energia hidro ( 1 = activa, 0 = inactiva )
common / SiEnerHid / SiEnerHid

!bandera para activar restricciones de energia termo ( 1 = activa, 0 = inactiva )
common / SiEnerTer / SiEnerTer

!bandera para activar restricciones de transmision ( 1 = activa, 0 = inactiva )
common / SiTransmision / SiTransmision

!bandera para activar estimacion de perdidas en transmision ( 1 = activa, 0 = inactiva )
common / SiPerdidas / SiPerdidas

!Tipo de ofertas a considerar ( 1 = ofertas de costo a generacion minima, 0 = ofertas de costo en vacio )
common / TipoOferta / TipoOferta

!bandera para considerar una solucion inicial ( 1 = si, 0 = no )
common / SiSolucionInicial / SiSolucionInicial

!bandera para considerar limites de regulacion ( 1 = si, 0 = no )
common / SiLimReg / SiLimReg

!bandera para arranque no simultaneo de unidades ( 1 = si, 0 = no )
common / SiArrNoSimul / SiArrNoSimul

!bandera para reserva de regulacion distribuida en zonas ( 1 = si, 0 = no )
common / SiResRegDis / SiResRegDis

! Numero de iteraciones para aproximar perdidas en transmision
common / IterPerdidas / IterPerdidas

! Bandera para activar variables artificiales de transmision ( 1 = activa, 0 = inactiva )
common / SiArtTrans / SiArtTrans

! Bandera para activar bandas prohibidas de operacion ( 1 = activa, 0 = inactiva )
common / SiBandProh / SiBandProh

! Porcentaje de cercania a la solucion optima de un problema de asignacion y despacho (MILP) en %
common / GAPCPLEX / GAPCPLEX

! Tiempo maximo (segundos) de ejecucion para resolver un problema de asignacion y despacho (MILP)
common / LIMIT_TIME_LINEAR / LIMIT_TIME_LINEAR

! Bandera para considerar modelado hidraulico ( 1 = si, 0 = no )
common / SiModHid / SiModHid

! Bandera para considerar refinamiento en modelado hidraulico ( 1 = si, 0 = no )
common / SirefHid / SiRefHid

! Bandera para considerar limitacion de combustible ( 1 = si, 0 = no )
common / SiGpoGas / SiGpoGas

! Bandera para considerar la solucion del segundo problema ( 1 = si, 0 = no )
common / SiSegProb / SiSegProb

!bandera para calculo interno de penalizaciones ( 1 = si, 0 = no )
common / SiCalPen / SiCalPen

!bandera para escritura de modelo LP ( 1 = si, 0 = no )
common / SiEscLP / SiEscLP

! Arreglo de banderas que indican algun problema en la calidad de las solucion obtenida 
! (1 = problema, 0 = no problema)
common / SemBandera / SemBandera    ( 20 )

! Apuntador de inicio de restricciones de limite superior en unidades de rango continuo
common / IRLMRC / IRLMRC

! Apuntador de inicio de restricciones de limite superior en unidades de rango discontinuo
common / IRLMRD / IRLMRD

! Apuntador de inicio de restricciones de limite superior en unidades hidro
common / IRLMH / IRLMH

! Apuntador de inicio de restricciones de limites de combustible
common / IRGPOGAS / IRGPOGAS

! Apuntador de inicio de restricciones de cota superior de reserva de 10 minutos
common / IRESRE10 / IRESRE10

! Precio Tope de Energia (Oferta incremental mas cara)
common / PrecioTopeEner / PrecioTopeEner

! Precio Tope de Costo Minimo de Energia (Oferta mas cara)
common / PrecioTopeEM / PrecioTopeEM

! Precio Tope de Costo de Arranque (Oferta mas cara)
common / PrecioTopeArr / PrecioTopeArr

! Precio Tope de Precio de Escasez de Reserva de Regulacion (Oferta mas cara)
common / PrecioTopeReg / PrecioTopeReg

! Precio Tope de Precio de Escasez de Reserva Rodante (Oferta mas cara)
common / PrecioTopeRod / PrecioTopeRod

! Precio Tope de Precio de Escasez de Reserva Operativa (Oferta mas cara)
common / PrecioTopeOper / PrecioTopeOper

! Precio Tope de Precio de Escasez de Reserva Suplementaria (Oferta mas cara)
common / PrecioTopeSup / PrecioTopeSup

!Tipo de escalamiento de precios a considerar ( 1 = maxima oferta incremental, 0 = curva de demanda de reservas )
common / TipoEscPre / TipoEscPre

!Tipo de problema a resolver ( 1 = entero, 0 = continuo )
common / TipoProblema / TipoProblema

!Escalamiento de precios de servicios conexos ( 1 = si, 0 = no )
common / SiEscalaConex / SiEscalaConex

! Epsilon para relajar requerimientos de reserva en segundo problema
common / epsilonreserva / epsilonreserva

! duales de reserva rodante de 10 por zona escalados
REAL*8  dualresr10zesc
common / dualresr10zesc / dualresr10zesc ( maxgrure, maxint )

! duales de reserva de 10 por zona escalados
REAL*8  dualres10zesc
common / dualres10zesc / dualres10zesc ( maxgrure, maxint )

! duales de reserva suplementaria por zona escalados
REAL*8  dualresszesc
common / dualresszesc / dualresszesc ( maxgrure, maxint )

! duales de reserva regulacion por zona escalados
REAL*8  dualresrezesc
common / dualresrezesc / dualresrezesc ( maxgrure, maxint )

! cotas inferiores originales de las variables de asignacion
REAL*8  lbOMILP
common / lbOMILP    / lbOMILP            ( maxvarMILP )

! cotas superiores originales de las variables de asignacion
REAL*8  ubOMILP
common / ubOMILP    / ubOMILP            ( maxvarMILP )

! porcentaje de la demanda nodal para permitir el corte
common / PorcenCorte    / PorcenCorte

!Reserva de regulacion contribuye a la reserva rodante ( 1 = si, 0 = no )
integer SiRegEnRod
common / SiRegEnRod / SiRegEnRod

!Mantener asignacion de reservas de MDA en AUGC ( 1 = si, 0 = no )
integer SiMantReservas
common / SiMantReservas / SiMantReservas

!Relajar reserva de regulacion ( 1 = si, 0 = no )
integer SiRelReg
common / SiRelReg / SiRelReg

!Relajar reserva rodante ( 1 = si, 0 = no )
integer SiRelRod
common / SiRelRod / SiRelRod

!Proteger reserva no rodante ( 1 = si, 0 = no )
integer SiRelNRod
common / SiRelNRod / SiRelNRod

!Fecha del horizonte resuelto
character*12 Horizo
common / Horizo / Horizo

integer IREUSO10
! Apuntador de inicio de uso de reserva drodante de 10 en el AUGC
common / IREUSO10 / IREUSO10

integer IREUSORE
! Apuntador de inicio de uso de reserva de regulacion en el AUGC
common / IREUSORE / IREUSORE

integer IREUSOREH
! Apuntador de inicio de uso de reserva de regulacion hidro en el AUGC
common / IREUSOREH / IREUSOREH

real*8 PenRelReg
! Penalizacion por relajar reserva de regulacion
common / PenRelReg  / PenRelReg

real*8 PenRelRod
! Penalizacion por relajar reserva rodante
common / PenRelRod  / PenRelRod

!Escribir resultados de la red electrica a archivos CSV ( 1 = si, 0 = no )
integer SiEscResRed
common / SiEscResRed / SiEscResRed

End Module
