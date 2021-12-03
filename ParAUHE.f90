!
!**************************************************************************
!******************************* CHT-AU-AUHE *******************************
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
!     Prop�sito:                                                          *
!         Declaraci�n de variables necesarios para el programa CHT-AU-AUHE*
!                                                                         *
!     Nombre y fecha de implementaci�n:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Agosto 2015                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Septiembre 2019                       *
!**************************************************************************
!
Module ParAUHE

implicit none

!include 'C:\AUCHT\Parametros\param.par'  !Windows (comentarizar para Linux)

include 'param.par'   !Linux (comentarizar para Windows)

!Mensaje a pantalla y bitacoras
character*255 bmensaje

!ruta de entrada de archivos *.csv
character * 40 rut_dat_1 

character*20 nomsis, nombunirc, proprc, nodo_area, nodo_subsis, unidadrc_area, &
             carga_area, nombcar, propcar, unidadnpr_area, nombuninpr, propnpr, &
             NomZonaRes, NodInt_area, unidadh_area, nombunih, proph, nombunire, &
             nombunird, proprd, unidadrd_area, nombcomurd, unidadrd_area_rd, unidadre_area, &
             propre, NomGpoTer

character*7 tiunidrc, tiunidnpr, tiunidrd, ticompurd, tiunidre

!character*6 NOMEMB

!character*9 NOMPLAH

!Variable de ambiente de CPLEX
integer(8) enb

!longitud de la ruta de entrada de archivos *.csv
integer long_ruta

INTEGER NTINTR, NumGruRes, NumNodos, NumUniRC, NumBloVRC, NumBloArrURC, &
        NumUniRD, NumModRD, NumBloVRD, NumUNiHid, NumOferDem, &
        NumBloDem, NumBloRR10, NumBloR10, NumBloRSu, NumBloRReg, &
        NumEmbalses, NumRamasR, NumUniRE, NumBloVRE, indsis, EstadoIsla, SlackIsla, &
        numsis_act, numsis, nodorc, IslaGenRC, IslaGenNPR, corresprc, &
        durintr, IslaDem, NumUniNPR, nodonpr, correspnpr, nodocar, correspcar, &
        TiemInicioArrRCS, NmBloArrURC, ZonaResURC, NumZonaRes, &
        zona_subsis, UniGruResRC, ApunURCxZona, UniRCxZona, NumURCxZona, NumNodInt, &
        NodoInt, IslaNodInt, IslaGenH, nodoh, corresph, NumCenHEmb, NumPlaH, &
        ApunPlantaEmbalse, ListaPlantasH, ApunUnidadPlanta, ListaUnidadesH, &
        DispoUH, UniGruResH, ApunUHxZona, UniHxZona, &
        NumUHxZona, DispoURE, AsignURE, IslaGenRD, NumCompRD, ListCompURD, corresprd, &
        ApunCompURD, nodocompurd, TransFacti, NumMaxTrans, TiempoTrans, TiemInicioArrRDS, &
        NmBloArrURD, UniGruResRD, NumURDxZona, UniRDxZona, ApunURDxZona, CompXModo, NumCompXModo, &
        CarGruRes, NumCarxZona, CarxZona, ApunCarxZona, IslaGenRE, nodounre, correspre, UniGruTerRC, &
        UniGruTerRD, NumGruUTer, NumURCxGrupo, NumURDxGrupo, ApunURCxGrupo, ApunURDxGrupo, UniRCxGrupo, &
        UniRDxGrupo, NumGpoTer, SisUniRC, SisUniRD, SisUniH, SisUniRE, NoRaOpRC, NoRaOpH, NoNodDisRC, &
        NoNodDisCar, Tempnodorc, Tempnodocar, Tempnodonpr, NoNodDisNPR

INTEGER EstadoCIURC, NumMaxParoURC, TminOperURC, TminParoURC, NumHCIURC, &
        TiempoArraURC, AsignURC, DispoURC, CoordURC, RestEnergia, EstadoCIURD, &
        NumHCIURD, TminModoURD, AsignURD, DispoURD, CoordURD

real*8  CostoArrRCS, CostoMinGRC, PreVenEnerRC, PreVenResR10RC, &
        PreVenResNR10RC, PreVenResRxRC, PreVenResNRxRC, PreVenResRegRC, &
        CostoArrRDS, CostoMinGRD, PreVenEnerRD, PreVenResR10RD, &
        PreVenResNR10RD, PreVenResRxRD, PreVenResNRxRD, PreVenResRegRD, &
        CostoOporUH, PreVenResR10H, PreVenResNR10H, PreVenResRxH, &
        PreVenResNRxH, PreVenResRegH, PreVenEnerRE, PreOferRes10, &
        PreOferResS, PreResRR10, PreResR10, PreResSup, PreResReg, &
        PreResRR10S, PreResR10S, PreResSupS, PreResRegS, CostoCorte, &
        PreComEner, CostoExced, PenRamas, PenEnerEmb, PenEnerUTerm, &
        GenCIURC, PotSincURC, RampArraURC, CostArrUniURC, RampaSubURC, &
        RampaBajURC, RamEmer10RC, RamEmerxRC, RamRegRC, PotSincNR10URC, &
        PotSincNRSURC, OferResR10RC, OferResNR10RC, OferResRxRC, OferResNRxRC, &
        OferResRegRC, CalOferResR10RC, CalOferResRxRC, CalOferResRegRC, CalOferResNR10RC, &
        CalOferResNRxRC, ReqResR10, ReqRes10, ReqResSup, ReqResReg, ReqResR10S, &
        ReqRes10S, ReqResSupS, ReqResRegS, PotNodInt, RamEmer10H, RamEmerxH, RamRegH, &
        OferResR10H, OferResRxH, OferResRegH, CalOferResNRxH, CalOferResNR10H, LimIEnerEmb, &
        LimSEnerEmb, OferVenEnerRD, GenCIURD, PotSincURD, PotSincNR10URD, PotSincNRSURD, RampArraURD, &
        CostoTrans, RampaSubURD, RampaBajURD, RamEmer10RD, RamEmerxRD, RamRegRD, OferResR10RD, &
        OferResRxRD, OferResRegRD, OferResNR10RD, OferResNRxRD, CalOferResR10RD, CalOferResRxRD, CalOferResRegRD, &
        CalOferResNR10RD, CalOferResNRxRD, GenCompXModo, OferRes10, OferResS, LimEnerIUTermo, LimEnerSUTermo, &
        ResNR10URC, ResNRSUURC, ResNR10URD, ResNRSUURD, ResNR10UH, ResNRSUUH, RaOpSupRC, RaOpInfRC, RaOpSupH, RaOpInfH
real*8  CalOferRegRORC, CalOferRegROH, facdistgen, facdistcar, facdistnpr, RaRegInfRC, RaRegSupRC, RaRegInfH, RaRegSupH
 

real*8  PotMinGRC, OferVenEnerRC, PotMaxGRC, DemFija, OferComDem, PotNPR, &
        PotMinGRE, OferVenEnerRE, PotMaxGRE, PotMinGRD, PotMaxGRD, PotMinRRC, PotMaxRRC, PotMinRRD, PotMaxRRD

integer AsignUH, CoordUH, EstadoCIUH, RESMODO, PrioridadH

real*8  CalOferResR10H, CalOferResRxH, CalOferResRegH, OferResNR10H, &
        OferResNRxH, PotMinUniH, PotMaxUniH, PotMinUniH_orig, PotMaxUniH_orig, PotMinRUnih, PotMaxRUnih, NoUnidades_plantaH  
real*8  GENUNRC, GENUNH, GENUNRE, GENUNRD, RTerURD, RTerURC

integer UnichauRC, Unirdes, Unirzn, UnirznuRC, Unirsn, UnirsnuRC, &
        UnichauH, UnirznuH, UnirsnuH, UnichauRE, UnirEnH, UnichauRD, &
        UnirznuRD, UnirsnuRD, UniFlujo, UniGLimT, UnirCar, Unimarreg

integer Unipmr, Unipmrgen, Unipmrper, Unipmrcon, Unilmp, Unilmpgen, &
        Unilmpper, Unilmpcon, UniSemaf, TipoEjecu, NumMaxParoUH, NoActParRC, NoActParH, CompSincH, CompSincRC
character*6  NomEjecu

! Datos basicos JLC

! Tipo de ejecucion
common / TipoEjecu  / TipoEjecu

! Nombre de ejecucion
common / NomEjecu  / NomEjecu

! Numero de intervalos
common / NTINTR  / NTINTR

! Duracion del intervalo en minuntos
common / durintr  / durintr

! Numero de grupos de reserva
common / NumGruRes  / NumGruRes

! Numero de grupos de unidades termicas con limitaciones de energia
common / NumGruUTer  / NumGruUTer

! Numero de nodos
common / NumNodos  / NumNodos

! Numero de unidades de rango continuo
common / NumUniRC  / NumUniRC

! Numero de unidades de rango continuo de todos los sistemas en la base de datos
common / SisUniRC  / SisUniRC

! Numero de unidades hidro de todos los sistemas en la base de datos
common / SisUniH  / SisUniH

! Numero de unidades renovables de todos los sistemas en la base de datos
common / SisUniRE  / SisUniRE

! Numero de componentes de unidades de rango discontinuo
common / NumCompRD  / NumCompRD ( maxurd )

! Numero de bloques de oferta de venta de unidades de rango continuo
common / NumBloVRC  / NumBloVRC   ( maxurc, maxint )

! Numero de bloques de costos de arranque de unidades de rango continuo
common / NumBloArrURC  / NumBloArrURC   ( maxurc, maxint )

! Numero de unidades de rango discontinuo
common / NumUniRD  / NumUniRD

! Numero toal de unidades de rango discontinuo de todos los sistemas de la base de datos
common / SisUniRD  / SisUniRD

! Numero de modos de unidades de rango discontinuo
common / NumModRD  / NumModRD   ( maxurd )

! Numero de bloques de oferta de venta de unidades de rango discontinuo
common / NumBloVRD  / NumBloVRD   ( maxurd, maxmodos, maxint )

! Limite inferior de potencia de regulacion de una unidad de rango discontinuo 
common / PotMinRRD  / PotMinRRD  ( maxurd, maxmodos, maxint )

! Limite superior de potencia de regulacion de una unidad de rango discontinuo 
common / PotMaxRRD  / PotMaxRRD  ( maxurd, maxmodos, maxint )

! Numero de unidades hidro
common / NumUniHid  / NumUniHid

! Numero de unidades renovables
common / NumUniRE  / NumUniRE

! Numero de bloques de oferta de venta de unidades renovables
common / NumBloVRE  / NumBloVRE   ( maxure, maxint )

! Numero de ofertas de demanda
common / NumOferDem  / NumOferDem

! Numero de bloques de demanda
common / NumBloDem  / NumBloDem   ( maxdem, maxint )

! Demandas Fijas
common / DemFija  / DemFija   ( maxdem, maxint )

! Numero de bloques de reserva rodante de 10 minutos de CENACE
common / NumBloRR10  / NumBloRR10 ( maxint )

! Numero de bloques de reserva de 10 minutos de CENACE
common / NumBloR10  / NumBloR10 ( maxint )

! Numero de bloques de reserva suplementaria de CENACE
common / NumBloRSu  / NumBloRSu ( maxint )

! Numero de bloques de reserva de regulacion de CENACE
common / NumBloRReg  / NumBloRReg ( maxint )

! Numero de embalses
common / NumEmbalses  / NumEmbalses

!Indice de sistemas
common / Indsis / Indsis ( maxsis )

!Nombre del subsistema
common / Nomsis / Nomsis ( maxsis )

!Estado del subsistema
common / EstadoIsla / EstadoIsla ( maxsis )

!Nodo de referencia por subsistema
common / SlackIsla / SlackIsla ( maxsis )
!cadena de caracteres conteniendo el mensaje del programa
common / bmensaje / bmensaje

!Numero de subsistemas activos
common / numsis_act / numsis_act

!Numero de subsistemas
common / numsis / numsis

!Nombre de unidades con rango continuo
common / nombunirc / nombunirc ( maxurc )

!Nombre de unidades con rango discontinuo
common / nombunird / nombunird ( maxurd )

!Nombre los componentes de las unidades con rango discontinuo
common / nombcomurd / nombcomurd ( maxcompurd * maxurd )

!Nombre de unidades hidro
common / nombunih / nombunih ( maxuh )

!Nombre de unidades renovables
common / nombunire / nombunire ( maxure )

!Nodo de las unidades con rango continuo
common / nodorc / nodorc ( maxurc, maxint )

common / Tempnodorc / Tempnodorc ( maxurc, maxnodist, maxint )

!Nodo de las unidades renovables intermitentes
common / nodounre / nodounre ( maxure, maxint )

!Nodo de las componentes de las unidades con rango discontinuo
common / nodocompurd / nodocompurd ( maxcompurd * maxurd, maxint )

!Nodo de las unidades hidro
common / nodoh / nodoh ( maxuh, maxint )

!Propietario de las unidades con rango continuo
common / proprc / proprc ( maxurc )

!Propietario de las unidades renovables intermitentes
common / propre / propre ( maxure )

!Propietario de las unidades con rango continuo
common / proprd / proprd ( maxurd )

!Propietario de las unidades hidro
common / proph / proph ( maxuh )

!Prioridad de operacion de unidades hidro
common / PrioridadH / PrioridadH ( maxuh )

!Tipo de las unidades con rango continuo
common / tiunidrc / tiunidrc ( maxurc )

!Tipo de las unidades renovables intermitentes
common / tiunidre / tiunidre ( maxure )

!Tipo de las unidades con rango continuo
common / tiunidrd / tiunidrd ( maxurd )

!Tipo del componente de las unidades con rango continuo
common / ticompurd / ticompurd ( maxcompurd * maxurd )

!Tiempo minimo de operacion de una unidad de rango continuo
common / TminOperURC / TminOperURC ( maxurc )

!Tiempo minimo de paro de una unidad de rango continuo
common / TminParoURC / TminParoURC ( maxurc )

!Tiempo de arranque de una unidad de rango continuo
common / TiempoArraURC / TiempoArraURC ( maxurc )

!Relacion nodo - area
common / nodo_area / nodo_area ( maxnod )

!Relacion nodo - subsistema
common / nodo_subsis / nodo_subsis ( maxnod )

!Estado de la condicion inicial de una unidad de rango continuo
common / EstadoCIURC / EstadoCIURC ( maxurc )

!Numero actual de paros en unidades de rango continuo
common / NoActParRC / NoActParRC ( maxurc )

!Estado de la condicion inicial de una unidad de rango discontinuo
common / EstadoCIURD / EstadoCIURD ( maxurd, maxmodos )

!Asignabilidad de una unidad de rango continuo
common / AsignURC / AsignURC ( maxurc, maxint )

!Asignabilidad de los modos de una unidad de rango discontinuo
common / AsignURD / AsignURD ( maxurd, maxmodos, maxint )


!Estado de la condicion inicial de una unidad hidro
common / EstadoCIUH / EstadoCIUH ( maxuh )

!Numero actual de paros en unidades hidro
common / NoActParH / NoActParH ( maxuh )

!Asignabilidad de una unidad hidro
common / AsignUH / AsignUH ( maxuh, maxint )

!Numero de horas en paro o en operacion en condiciones iniciales de una unidad de rango continuo
common / NumHCIURC / NumHCIURC ( maxurc )

!Numero de horas en paro o en operacion en condiciones iniciales de una unidad de rango discontinuo
common / NumHCIURD / NumHCIURD ( maxurd, maxmodos )

!Numero maximo de paros de una unidad de rango continuo
common / NumMaxParoURC / NumMaxParoURC ( maxurc )

!Numero maximo de paros de una unidad hidro
common / NumMaxParoUH / NumMaxParoUH ( maxuh )
!Isla asociada a una unidad de rango continuo
common / IslaGenRC / IslaGenRC ( maxurc )

!Isla asociada a una unidad renovable intermitente
common / IslaGenRE / IslaGenRE ( maxure )

!Isla asociada a una unidad de rango discontinuo
common / IslaGenRD / IslaGenRD ( maxurd )

!Isla asociada a una unidad hidro
common / IslaGenH / IslaGenH ( maxuh )

!Correspondencia unidades de rango continuo consecutivo - por isla activa
common / corresprc / corresprc ( maxurc )

!Correspondencia unidades renovables intermitentes consecutivo - por isla activa
common / correspre / correspre ( maxure )

!Correspondencia unidades de rango discontinuo consecutivo - por isla activa
common / corresprd / corresprd ( maxurd )

!Correspondencia unidades hidro consecutivo - por isla activa
common / corresph / corresph ( maxuh )

! Costo de arranque para unidades de rango continuo
common / CostoArrRCS  / CostoArrRCS  ( maxurc, maxint )

! Costo de arranque para unidades de rango discontinuo
common / CostoArrRDS  / CostoArrRDS  ( maxurd, maxmodos, maxsegarrd )

! Costo minimo de generacion para unidades de rango continuo
common / CostoMinGRC  / CostoMinGRC  ( maxurc, maxint )

! Precio de oferta de venta de energia para unidades de rango continuo
common / PreVenEnerRC  / PreVenEnerRC  ( maxurc, maxsegrc, maxint )

! Oferta de venta de generacion para unidades de rango continuo
common / OferVenEnerRC  / OferVenEnerRC  ( maxurc, maxsegrc, maxint )

! Precio de oferta de venta de reserva rodante de diez minutos para unidades de rango continuo
common / PreVenResR10RC  / PreVenResR10RC  ( maxurc, maxint )

! Precio de oferta de venta de reserva rodante de diez minutos para unidades hidro
common / PreVenResR10H  / PreVenResR10H  ( maxuh, maxint )

! Precio de oferta de venta de reserva no rodante de diez minutos para unidades de rango continuo
common / PreVenResNR10RC  / PreVenResNR10RC  ( maxurc, maxint )

! Precio de oferta de venta de reserva no rodante de diez minutos para unidades de rango discontinuo
common / PreVenResNR10RD  / PreVenResNR10RD  ( maxurd, maxmodos, maxint )

! Precio de oferta de venta de reserva rodante suplementaria para unidades de rango continuo
common / PreVenResRxRC  / PreVenResRxRC  ( maxurc, maxint )

! Precio de oferta de venta de reserva no rodante suplementaria para unidades de rango continuo
common / PreVenResNRxRC  / PreVenResNRxRC  ( maxurc, maxint )

! Precio de oferta de venta de reserva de regulacion para unidades de rango continuo
common / PreVenResRegRC  / PreVenResRegRC  ( maxurc, maxint )

! Precio de oferta de venta de reserva de regulacion para unidades de rango discontinuo
common / PreVenResRegRD  / PreVenResRegRD  ( maxurd, maxmodos, maxint )

! Costo minimo de generacion para unidades de rango discontinuo
common / CostoMinGRD  / CostoMinGRD  ( maxurd, maxmodos, maxint )

! Precio de oferta de venta de energia para unidades de rango discontinuo
common / PreVenEnerRD  / PreVenEnerRD  ( maxurd, maxmodos, maxsegrd, maxint )

! Potencia de oferta de venta de energia para unidades de rango discontinuo
common / OferVenEnerRD  / OferVenEnerRD  ( maxurd, maxmodos, maxsegrd, maxint )

! Precio de oferta de venta de reserva rodante de diez minutos para unidades de rango discontinuo
common / PreVenResR10RD  / PreVenResR10RD  ( maxurd, maxmodos, maxint )

! Precio de oferta de venta de reserva rodante suplementaria para unidades de rango discontinuo
common / PreVenResRxRD  / PreVenResRxRD  ( maxurd, maxmodos, maxint )

! Precio de oferta de venta de reserva no rodante suplementaria para unidades de rango discontinuo
common / PreVenResNRxRD  / PreVenResNRxRD  ( maxurd, maxmodos, maxint )

! Costo de oportunidad del agua para una unidad Hidro
common / CostoOporUH  / CostoOporUH  ( maxuh, maxint )

! Precio de oferta de venta de reserva no rodante de diez minutos para unidades hidro
common / PreVenResNR10H  / PreVenResNR10H  ( maxuh, maxint )

! Precio de oferta de venta de reserva rodante suplementaria para unidades hidro
common / PreVenResRxH  / PreVenResRxH  ( maxuh, maxint )

! Precio de oferta de venta de reserva no rodante suplementaria para unidades hidro
common / PreVenResNRxH  / PreVenResNRxH  ( maxuh, maxint )

! Precio de oferta de venta de reserva de regulacion para unidades unidades hidro
common / PreVenResRegH  / PreVenResRegH  ( maxuh, maxint )

! Precio de oferta de venta de energia para unidades renovables
common / PreVenEnerRE  / PreVenEnerRE  ( maxure, maxsegre, maxint )

! Precio de oferta de reserva de 10 minutos (demanda controlable)
common / PreOferRes10  / PreOferRes10  ( maxdem, maxint )

! Precio de oferta de reserva suplementaria (demanda controlable)
common / PreOferResS  / PreOferResS  ( maxdem, maxint )

! Precio de oferta de compra de energia
common / PreComEner  / PreComEner  ( maxdem, maxsegde, maxint )

! Oferta de compra de energia
common / OferComDem  / OferComDem  ( maxdem, maxsegde, maxint )

!Relacion isla - demanda
common / IslaDem / IslaDem ( maxdem )

! Precio de oferta de compra de reserva rodante de 10 minutos, por grupo, del CENACE
common / PreResRR10  / PreResRR10  ( maxgrure, maxsegce, maxint )

! Precio de oferta de compra de reserva de 10 minutos, por grupo, del CENACE
common / PreResR10  / PreResR10  ( maxgrure, maxsegce, maxint )

! Precio de oferta de compra de reserva suplementaria, por grupo, del CENACE
common / PreResSup  / PreResSup  ( maxgrure, maxsegce, maxint )

! Precio de oferta de compra de reserva de regulacion, por grupo, del CENACE
common / PreResReg  / PreResReg  ( maxgrure, maxsegce, maxint )

! Costo por corte de carga
common / CostoCorte  / CostoCorte

! Costo por excedente de generacion 
common / CostoExced  / CostoExced

! Penalizacion por violacion en el flujo de los grupos de ramas electricas
common / PenRamas  / PenRamas

! Penalizacion por violacion en la limitacion de energia del embalse
common / PenEnerEmb  / PenEnerEmb

! Penalizacion por violacion en la limitacion de energia del grupo de unidades termicas
common / PenEnerUTerm  / PenEnerUTerm

! Limite inferior de potencia de generacion de una unidad de rango continuo 
common / PotMinGRC  / PotMinGRC  ( maxurc, maxint )

! Limite superior de potencia de generacion de una unidad de rango continuo 
common / PotMaxGRC  / PotMaxGRC  ( maxurc, maxint )

! Limite inferior de potencia de regulacion de una unidad de rango continuo 
common / PotMinRRC  / PotMinRRC  ( maxurc, maxint )

! Limite superior de potencia de regulacion de una unidad de rango continuo 
common / PotMaxRRC  / PotMaxRRC  ( maxurc, maxint )

! Bandera que indica si una unidad de rango continuo es compensador sincrono (bandera = 2)
common / CompSincRC  / CompSincRC  ( maxurc, maxint )

! Limite inferior de potencia de generacion de una unidad de rango discontinuo 
common / PotMinGRD  / PotMinGRD  ( maxurd, maxmodos, maxint )

! Limite superior de potencia de generacion de una unidad de rango discontinuo 
common / PotMaxGRD  / PotMaxGRD  ( maxurd, maxmodos, maxint )

! Limite inferior de potencia de generacion de una unidad hidro
common / PotMinUniH  / PotMinUniH  ( maxuh, maxint )

! Limite superior de potencia de generacion de una unidad hidro
common / PotMaxUniH  / PotMaxUniH  ( maxuh, maxint )

! Limite inferior de potencia de generacion de una unidad hidro originales
common / PotMinUniH_orig  / PotMinUniH_orig  ( maxuh, maxint )

! Limite superior de potencia de generacion de una unidad hidro originales
common / PotMaxUniH_orig  / PotMaxUniH_orig  ( maxuh, maxint )

! Limite inferior de potencia de regulacion de una unidad hidro
common / PotMinRUniH  / PotMinRUniH  ( maxuh, maxint )

! Limite superior de potencia de regulacion de una unidad hidro
common / PotMaxRUniH  / PotMaxRUniH  ( maxuh, maxint )

! Bandera que indica si una unidad hidro es compensador sincrono (bandera = 2)
common / CompSincH  / CompSincH  ( maxuh, maxint )

! Numero de unidades no programables
common / NumUniNPR  / NumUniNPR

!Isla asociada a una unidad no programable
common / IslaGenNPR / IslaGenNPR ( maxunpr )

!Potencia de generaci�n de una unidad no programable
common / PotNPR / PotNPR ( maxunpr, maxint )

!Nombre de unidades no programables
common / nombuninpr / nombuninpr ( maxunpr )

!Nombre de cargas
common / nombcar / nombcar ( maxdem )

!Nodo de las unidades no programables
common / nodonpr / nodonpr ( maxunpr, maxint )

common / Tempnodonpr / Tempnodonpr ( maxunpr, maxnodist, maxint )


!Nodo de las cargas
common / nodocar / nodocar ( maxdem, maxint )

common / Tempnodocar / Tempnodocar ( maxdem, maxnodist, maxint )

!Propietario de las unidades no programables
common / propnpr / propnpr ( maxunpr )

!Propietario de la carga
common / propcar / propcar ( maxdem )

!Tipo de las unidades con rango continuo
common / tiunidnpr / tiunidnpr ( maxunpr )

!Relacion unidad rango continuo - area
common / unidadrc_area / unidadrc_area ( maxurc )

!Relacion unidad renovable intermitente - area
common / unidadre_area / unidadre_area ( maxure )

!Relacion unidad rango continuo - area
common / unidadrd_area / unidadrd_area ( maxurd )

!Relacion unidad rango cdisontinuo - area
common / unidadrd_area_rd / unidadrd_area_rd ( maxurd )

!Relacion unidad hidro - area
common / unidadh_area / unidadh_area ( maxuh )

!Relacion unidad no programable - area
common / unidadnpr_area / unidadnpr_area ( maxunpr )

!Relacion carga - area
common / carga_area / carga_area ( maxdem )

!Correspondencia unidades no programables consecutivo - por isla activa
common / correspnpr / correspnpr ( maxunpr )

!Correspondencia cargas  - por isla activa
common / correspcar / correspcar ( maxdem )

! Generacion en condiciones iniciales de unidades de rango continuo
common / GenCIURC  / GenCIURC  ( maxurc )

! Generacion en condiciones iniciales de unidades de rango discontinuo
common / GenCIURD  / GenCIURD  ( maxurd, maxmodos )

!Potencia de sincroniazacion de una unidad de rango continuo
common / PotSincURC / PotSincURC ( maxurc )

!Potencia de sincroniazacion de una unidad de rango discontinuo
common / PotSincURD / PotSincURD ( maxurd, maxmodos )

!Potencia de sincroniazacion para reserva no rodante de 10 min. de una unidad de rango continuo
common / PotSincNR10URC / PotSincNR10URC ( maxurc )

!Potencia de sincroniazacion para reserva no rodante de 10 min. de una unidad de rango discontinuo
common / PotSincNR10URD / PotSincNR10URD ( maxurd, maxmodos )

!Potencia de sincroniazacion para reserva no rodante suplementaria de una unidad de rango continuo
common / PotSincNRSURC / PotSincNRSURC ( maxurc )

!Potencia de sincroniazacion para reserva no rodante suplementaria de una unidad de rango discontinuo
common / PotSincNRSURD / PotSincNRSURD ( maxurd, maxmodos )

!Rampa de arranque de una unidad de rango continuo
common / RampArraURC / RampArraURC ( maxurc )

!Rampa de arranque de una unidad de rango discontinuo
common / RampArraURD / RampArraURD ( maxurd, maxmodos )

!Costo de arranque �nico de una unidad de rango continuo
common / CostArrUniURC / CostArrUniURC ( maxurc )

!Tiempo horas en paro para el bloque de arranque
common / TiemInicioArrRCS / TiemInicioArrRCS ( maxurc, maxsegarrc )

!Tiempo horas en paro para el bloque de arranque unidades de rango discontinuo
common / TiemInicioArrRDS / TiemInicioArrRDS ( maxurd, maxmodos, maxsegarrd )

!N�mero de bloques de arranque para unidades de rango continuo
common / NmBloArrURC / NmBloArrURC ( maxurc )

!N�mero de bloques de arranque para unidades de rango continuo
common / NmBloArrURD / NmBloArrURD ( maxurd, maxmodos )

!Rampa de subida de operacion para unidades de rango continuo
common / RampaSubURC / RampaSubURC ( maxurc )

!Rampa de bajada de operacion para unidades de rango continuo
common / RampaBajURC / RampaBajURC ( maxurc )

!Rampa de emergencia para reserva rodante de diez minutos para unidades de rango continuo
common / RamEmer10RC / RamEmer10RC ( maxurc )

!Rampa de emergencia para reserva rodante de diez minutos para unidades de rango discontinuo
common / RamEmer10RD / RamEmer10RD ( maxurd, maxmodos )

!Rampa de emergencia para reserva rodante suplementaria para unidades de rango continuo
common / RamEmerxRC / RamEmerxRC ( maxurc )

!Regimen termico para unidades de rango continuo
common / RTerURC / RTerURC ( maxurc )

!Rampa de emergencia para reserva rodante suplementaria para unidades de rango discontinuo
common / RamEmerxRD / RamEmerxRD ( maxurd, maxmodos )

!Rampa de regulacion para reserva de regulacion secundaria para unidades de rango continuo
common / RamRegRC / RamRegRC ( maxurc )

!Rampa de regulacion para reserva de regulacion secundaria para unidades de rango discontinuo
common / RamRegRD / RamRegRD ( maxurd, maxmodos )

!Regimen termico para unidades de rango discontinuo
common / RTerURD / RTerURD ( maxurd, maxmodos )

! Oferta de reserva rodante de 10 min unidades de rango continuo
common / OferResR10RC  / OferResR10RC  ( maxurc, maxint )

! Oferta de reserva rodante de 10 min unidades de rango discontinuo
common / OferResR10RD  / OferResR10RD  ( maxurd, maxmodos, maxint )

! Oferta de reserva rodante de 10 min unidades hidro
common / OferResR10H  / OferResR10H  ( maxuh, maxint )

! Oferta de reserva de 10 min cargas participantes
common / OferRes10  / OferRes10  ( maxdem, maxint )

! Oferta de reserva suplementaria cargas participantes
common / OferResS  / OferResS  ( maxdem, maxint )

! Oferta de reserva rodante suplementaria unidades hidro
common / OferResRxH  / OferResRxH  ( maxuh, maxint )

! Oferta de reserva no rodante de 10 min unidades de rango continuo
common / OferResNR10RC  / OferResNR10RC  ( maxurc, maxint )

! Oferta de reserva no rodante de 10 min unidades de rango discontinuo
common / OferResNR10RD  / OferResNR10RD  ( maxurd, maxmodos, maxint )

! Oferta de reserva rodante suplementaria de x min unidades de rango continuo
common / OferResRxRC  / OferResRxRC  ( maxurc, maxint )

! Oferta de reserva rodante suplementaria de x min unidades de rango discontinuo
common / OferResRxRD  / OferResRxRD  ( maxurd, maxmodos, maxint )

! Oferta de reserva no rodante suplementaria de x min unidades de rango continuo
common / OferResNRxRC  / OferResNRxRC  ( maxurc, maxint )

! Oferta de reserva no rodante suplementaria de x min unidades de rango discontinuo
common / OferResNRxRD  / OferResNRxRD  ( maxurd, maxmodos, maxint )

! Oferta de reserva de regulacion secundaria de x min unidades de rango continuo
common / OferResRegRC  / OferResRegRC  ( maxurc, maxint )

! Oferta de reserva de regulacion secundaria de x min unidades de rango discontinuo
common / OferResRegRD  / OferResRegRD  ( maxurd, maxmodos, maxint )

! C�lculo oferta de reserva rodante de diez minutos unidades de rango continuo
common / CalOferResR10RC  / CalOferResR10RC  ( maxurc, maxint )

! C�lculo oferta de reserva rodante de diez minutos unidades de rango discontinuo
common / CalOferResR10RD  / CalOferResR10RD  ( maxurd, maxmodos, maxint )

! C�lculo oferta de reserva rodante suplementaria unidades de rango continuo
common / CalOferResRxRC  / CalOferResRxRC  ( maxurc, maxint )

! C�lculo oferta de reserva rodante suplementaria unidades de rango discontinuo
common / CalOferResRxRD  / CalOferResRxRD  ( maxurd, maxmodos, maxint )

! C�lculo oferta de reserva de regulacion secundaria unidades de rango continuo
common / CalOferResRegRC  / CalOferResRegRC  ( maxurc, maxint )

! C�lculo oferta de reserva de regulacion secundaria unidades de rango continuo con bandas prohibidas
common / CalOferRegRORC  / CalOferRegRORC  ( maxurc, maxzonproh+1, maxint )

! C�lculo oferta de reserva de regulacion secundaria unidades de rango continuo
common / CalOferResRegRD  / CalOferResRegRD  ( maxurd, maxmodos, maxint )

! C�lculo del limite minimo de la oferta de reserva no rodante de diez minutos unidades de rango continuo
common / CalOferResNR10RC  / CalOferResNR10RC  ( maxurc, maxint )

! C�lculo del limite minimo de la oferta de reserva no rodante de diez minutos unidades de rango discontinuo
common / CalOferResNR10RD  / CalOferResNR10RD  ( maxurd, maxmodos, maxint )

! C�lculo del limite minimo de la oferta de reserva no rodante suplementaria unidades de rango continuo
common / CalOferResNRxRC  / CalOferResNRxRC  ( maxurc, maxint )

! C�lculo del limite minimo de la oferta de reserva no rodante suplementaria unidades de rango discontinuo
common / CalOferResNRxRD  / CalOferResNRxRD  ( maxurd, maxmodos, maxint )

! C�lculo del limite minimo de la oferta de reserva no rodante de diez minutos unidades hidro
common / CalOferResNR10H  / CalOferResNR10H  ( maxurc, maxint )

! C�lculo del limite minimo de la oferta de reserva no rodante suplementaria unidades hidro
common / CalOferResNRxH  / CalOferResNRxH  ( maxurc, maxint )

! Disponibilidad unidades de rango continuo
common / DispoURC  / DispoURC  ( maxurc, maxint )

! Disponibilidad de modos de unidades de rango discontinuo
common / DispoURD  / DispoURD  ( maxurd, maxmodos, maxint )

! Coordinabilidad unidades de rango continuo
common / CoordURC  / CoordURC  ( maxurc, maxint )

! Coordinabilidad de modos de unidades de rango discontinuo
common / CoordURD  / CoordURD  ( maxurd, maxmodos, maxint )

! C�lculo oferta de reserva rodante de diez minutos unidades hidro
common / CalOferResR10H  / CalOferResR10H  ( maxuh, maxint )

! C�lculo oferta de reserva rodante suplementaria unidades hidro
common / CalOferResRxH  / CalOferResRxH  ( maxuh, maxint )

! C�lculo oferta de reserva de regulacion secundaria unidades hidro
common / CalOferResRegH  / CalOferResRegH  ( maxuh, maxint )

! C�lculo oferta de reserva de regulacion secundaria unidades de rango continuo con bandas prohibidas
common / CalOferRegROH  / CalOferRegROH  ( maxuh, maxzonproh+1, maxint )

! Oferta de reserva de regulacion secundaria unidades hidro
common / OferResRegH  / OferResRegH  ( maxuh, maxint )

! Oferta de reserva no rodante de diez minutos unidades hidro
common / OferResNR10H  / OferResNR10H  ( maxuh, maxint )

! Oferta de reserva no rodante suplementaria unidades hidro
common / OferResNRxH  / OferResNRxH  ( maxuh, maxint )

! Coordinabilidad unidades hidro
common / CoordUH  / CoordUH  ( maxuh, maxint )

! Zona de reserva a la que pertenecen las unidades de rango continuo
common / ZonaResURC  / ZonaResURC  ( maxurc )

! Nombre zona de reserva
common / NomZonaRes / NomZonaRes ( maxgrure ) 

! Numero de la zona de reserva zona de reserva
common / NumZonaRes / NumZonaRes ( maxgrure ) 

! Nombre grupo con limitacion de energia
common / NomGpoTer / NomGpoTer ( maxgrute ) 

! Numero grupo con limitacion de energia
common / NumGpoTer / NumGpoTer ( maxgrute ) 

! Limite inferior del grupo con limitacion de energia
common / LimEnerIUTermo / LimEnerIUTermo ( maxgrute, maxdia ) 

! Limite superior del grupo con limitacion de energia
common / LimEnerSUTermo / LimEnerSUTermo ( maxgrute, maxdia ) 

! Relacion zona de reserva - subsitema (numerico)
common / zona_subsis / zona_subsis ( maxgrure ) 

! Zonas a las que pertenecen las unidades de rango continuo
common / UniGruResRC / UniGruResRC ( maxurc, maxgrure )

! Grupos con limitaci�n de energ�a a las que pertenecen las unidades de rango continuo
common / UniGruTerRC / UniGruTerRC ( maxurc, maxgrute )

! Grupos con limitaci�n de energ�a a las que pertenecen las unidades de rango continuo
common / UniGruTerRD / UniGruTerRD ( maxurd, maxgrute )

! Zonas a las que pertenecen las cargas participantes
common / CarGruRes / CarGruRes ( maxdem, maxgrure )

! Zonas a las que pertenecen las unidades de rango discontinuo
common / UniGruResRD / UniGruResRD ( maxurd, maxgrure )

! Zonas a las que pertenecen las unidades hidro
common / UniGruResH / UniGruResH ( maxurc, maxgrure )

! Apuntador que indica la localidad donde empiezan las unidades de rango continuo de la zona de reserva
common / ApunURCxZona / ApunURCxZona ( maxgrure + 1 )

! Apuntador que indica la localidad donde empiezan las unidades de rango continuo del grupo con limitacion de energia
common / ApunURCxGrupo / ApunURCxGrupo ( maxgrute + 1 )


! Apuntador que indica la localidad donde empiezan las unidades de rango discontinuo del grupo con limitacion de energia
common / ApunURDxGrupo / ApunURDxGrupo ( maxgrute + 1 )

! Apuntador que indica la localidad donde empiezan las cargas participantes de la zona de reserva
common / ApunCarxZona / ApunCarxZona ( maxgrure + 1 )

! Apuntador que indica la localidad donde empiezan las unidades de rango discontinuo de la zona de reserva
common / ApunURDxZona / ApunURDxZona ( maxgrure + 1 )

! Apuntador que indica la localidad donde empiezan las unidades hidro de la zona de reserva
common / ApunUHxZona / ApunUHxZona ( maxgrure + 1 )

! Apuntador que indica la localidad donde empiezan las componentes de la unidad de rango discontinuo
common / ApunCompURD / ApunCompURD ( maxurd )

! Arreglo que indica cuantas unidades de Rango Continuo hay por zona de reserva 
common / NumURCxZona / NumURCxZona ( maxgrure )

! Arreglo que indica cuantas unidades de Rango Continuo hay por grupo con restriccion d energia
common / NumURCxGrupo / NumURCxGrupo ( maxgrute )

! Arreglo que indica cuantas unidades de Rango Discontinuo hay por grupo con restriccion d energia
common / NumURDxGrupo / NumURDxGrupo ( maxgrute )

! Arreglo que indica cuantas cargas participantes hay por zona de reserva 
common / NumCarxZona / NumCarxZona ( maxgrure )

! Arreglo que indica cuantas unidades de Rango disontinuo hay por zona de reserva 
common / NumURDxZona / NumURDxZona ( maxgrure )

! Arreglo que indica cuantas unidades hidro hay por zona de reserva 
common / NumUHxZona / NumUHxZona ( maxgrure )

! Lista de unidades de rango continio de las zonas de reserva
common / UniRCxZona / UniRCxZona ( maxurc )

! Lista de unidades de rango continio de los grupos con limitacion de energia
common / UniRCxGrupo / UniRCxGrupo ( maxurc )

! Lista de unidades de rango discontinio de los grupos con limitacion de energia
common / UniRDxGrupo / UniRDxGrupo ( maxurd )

! Lista de cargas participantes de las zonas de reserva
common / CarxZona / CarxZona ( maxdem )

! Lista de unidades de rango discontinio de las zonas de reserva
common / UniRDxZona / UniRDxZona ( maxurd )

! Lista de unidades hidro de las zonas de reserva
common / UniHxZona / UniHxZona ( maxurc )

! Requerimiento del CENACE de reserva rodante de 10 monutos
common / ReqResR10 / ReqResR10 ( maxgrure, maxsegce+1, maxint )

! Requerimiento acumulado del CENACE de reserva de 10 minutos
common / ReqRes10 / ReqRes10 ( maxgrure, maxsegce+1, maxint )

! Requerimiento del CENACE de reserva suplementaria
common / ReqResSup / ReqResSup ( maxgrure, maxsegce+1, maxint )

! Requerimiento del CENACE de reserva de regulacion secundaria
common / ReqResReg / ReqResReg ( maxgrure, maxsegce+1, maxint )

! Requerimiento del CENACE de reserva rodante de 10 min por sistema
common / ReqResR10S / ReqResR10S ( maxsis, maxsegce+1, maxint )

! Precio del requerimiento del CENACE de reserva rodante de 10 min por sistema
common / PreResRR10S / PreResRR10S ( maxsis, maxsegce+1, maxint )

! Requerimiento del CENACE de reserva de 10 min por sistema
common / ReqRes10S / ReqRes10S ( maxsis, maxsegce+1, maxint )

! Precio del requerimiento del CENACE de reserva de 10 min por sistema
common / PreResR10S / PreResR10S ( maxsis, maxsegce+1, maxint )

! Requerimiento del CENACE de reserva suplementaria por sistema
common / ReqResSupS / ReqResSupS ( maxsis, maxsegce+1, maxint )

! Precio del requerimiento del CENACE de reserva suplementaria por sistema
common / PreResSupS / PreResSupS ( maxsis, maxsegce+1, maxint )

! Requerimiento del CENACE de reserva de regulacion secundaria por sistema
common / ReqResRegS / ReqResRegS ( maxsis, maxsegce+1, maxint )

! Precio del requerimiento del CENACE de reserva de regulacion secundaria por sistema
common / PreResRegS / PreResRegS ( maxsis, maxsegce+1, maxint )

! Numero de nodos de intercambio
common / NumNodInt / NumNodInt

! Nodos de intercambio
common / NodoInt / NodoInt ( maxnod, maxint )

! Relacion nodo de intercambio con isla
common / IslaNodInt / IslaNodInt ( maxnod )

! Relacion nodo de intercambio con area 
common / NodInt_area / NodInt_area ( maxnod )

! Potencia activa del nodod de intercambio
common / PotNodInt / PotNodInt ( maxnod, maxint )

! Nombre del embalse
!common / NOMEMB / NOMEMB ( nmxemb )

! Limite min de energia para el embalse
common / LimIEnerEmb / LimIEnerEmb ( nmxemb )

! Limite max de energia para el embalse
common / LimSEnerEmb / LimSEnerEmb ( nmxemb )

! Bandera de restriccion de limitacion de energia en el embalse
common / RestEnergia / RestEnergia ( nmxemb )

! Nombre de la planta hidro
!common / NOMPLAH / NOMPLAH ( nmxpla )	

! Numero de plantas hidro por embalse
common / NumCenHEmb / NumCenHEmb ( nmxemb )	

! Numero total de plantas hidro
common / NumPlaH / NumPlaH

!Apuntador que dice en que localidad empieza la primera planta del embalse
common / ApunPlantaEmbalse / ApunPlantaEmbalse ( nmxemb )

!Lista consecutiva de plantas hidro (relacionado con ApunPlantaEmbalse)
common / ListaPlantasH / ListaPlantasH ( nmxpla )

!Apuntador que dice en que localidad empieza la primera unidad de la planta hidro
common / ApunUnidadPlanta / ApunUnidadPlanta( nmxpla )

!Lista consecutiva de unidades hidro (relacionada con ApunUnidadPlanta)
common / ListaUnidadesH / ListaUnidadesH ( maxuh ) 

!Numero de unidades por planta hidro
common / NoUnidades_plantaH / NoUnidades_plantaH ( nmxpla )

! Disponibilidad unidades hidro
common / DispoUH  / DispoUH  ( maxuh, maxint )

!Rampa de emergencia para reserva rodante de diez minutos para unidades hidro
common / RamEmer10H / RamEmer10H ( maxuh )

!Rampa de emergencia para reserva rodante suplementaria para unidades hidro
common / RamEmerxH / RamEmerxH ( maxuh )

!Rampa de regulacion para reserva de regulacion secundaria para unidades hidro
common / RamRegH / RamRegH ( maxuh )

! Disponibilidad unidades renovables
common / DispoURE  / DispoURE  ( maxure, maxint )

! Asignibilidad unidades renovables
common / AsignURE  / AsignURE  ( maxure, maxint )

! Limite inferior de potencia de generacion de una unidad renovable
common / PotMinGRE  / PotMinGRE  ( maxure, maxint )

! Limite superior de potencia de generacion de una unidad renovable
common / PotMaxGRE  / PotMaxGRE  ( maxure, maxint )

! Oferta de venta de generacion para unidades de rango continuo
common / OferVenEnerRE  / OferVenEnerRE  ( maxure, maxsegre, maxint )

! Lista de componentes de unidades de rango discontinio
common / ListCompURD  / ListCompURD  ( maxurd * 4 )

! Transiciones factibles unidades de rango discontinuo
common / TransFacti  / TransFacti  ( maxurd, maxmodos, maxmodos )

! Numero maximo de transiciones entre modos de unidades de rango discontinuo
common / NumMaxTrans  / NumMaxTrans  ( maxurd, maxmodos, maxmodos )

! Tiempo de transicion entre modos de unidades de rango discontinuo
common / TiempoTrans  / TiempoTrans  ( maxurd, maxmodos, maxmodos )

! Costo de transicion entre modos de unidades de rango discontinuo
common / CostoTrans  / CostoTrans  ( maxurd, maxmodos, maxmodos )

! Tiempo minimo de operacion de los modos de las unidades de rang discontinuo
common / TminModoURD  / TminModoURD  ( maxurd, maxmodos )

!Rampa de subida de operacion para unidades de rango discontinuo
common / RampaSubURD / RampaSubURD ( maxurd, maxmodos )

!Rampa de bajada de operacion para unidades de rango discontinuo
common / RampaBajURD / RampaBajURD ( maxurd, maxmodos )


! Resultados del problema

!Resultado de generacion en unidades de rango continuo
common / GENUNRC / GENUNRC ( maxurc, maxint )

!Resultado de generacion en unidades de rango discontinuo
common / GENUNRD / GENUNRD ( maxurd, maxint )
!Resultado de generacion en unidades hidro
common / GENUNH / GENUNH ( maxuh, maxint )

!Resultado de generacion en unidades renovables
common / GENUNRE / GENUNRE ( maxure, maxint )

!Resultado de estados de las unidades ( RC: 1=operacion, 0=apagada, 11=sincronizando,
!                                       RD: 1=apagada, 11=sincronizando, >=2 = modo,
!                                       H : 1=operacion, 0=apagada,
!                                       RI: 1=operacion, 0=apagada )
common / RESMODO / RESMODO ( maxurc+maxurd+maxuh+maxure, maxint )

!Componentes por modo que forman la unidad de rango discontinuo
common / CompXModo / CompXModo ( maxurd, maxmodos, maxcompurd )

!Numero de componentes que forman el modo de la unidad de rango discontinuo
common / NumCompXModo / NumCompXModo ( maxurd, maxmodos )

!Porcentaje de generacion para cada componente del modo de la unidad de rango discontinuo
common / GenCompXModo / GenCompXModo ( maxurd, maxmodos, maxcompurd )




!archivos de debugger (MILP)

! Generacion de uniaddes de rango continuo
common / UnichauRC / UnichauRC
! Generacion de uniaddes de rango discontinuo
common / UnichauRD / UnichauRD

! Generacion de uniaddes hidro
common / UnichauH / UnichauH

! Generacion de uniaddes renovables
common / UnichauRE / UnichauRE

! Despachos y duales de balance por intervalo
common / Unirdes / Unirdes

! Cargas aceptadas por intervalo
common / UnirCar / UnirCar

! Resultados de reserva por zona
common / Unirzn / Unirzn

common / UnirznuRC / UnirznuRC

common / UnirznuRD / UnirznuRD
common / UnirznuH / UnirznuH

! Resultados de reserva por sistema
common / Unirsn / Unirsn

common / UnirsnuRC / UnirsnuRC
common / UnirsnuRD / UnirsnuRD

common / UnirsnuH / UnirsnuH

! Resultados de limites de energia en embalses
common / UnirEnH / UnirEnH

! Resultados de flujos en grupos de ramas
common / UniFlujo / UniFlujo

! Resultados de grupos de unidades termo con limitacion de energia
common / UniGLimT / UniGLimT

! Resultados de costos marginales por region
common / Unimarreg / Unimarreg

! Resultados de costos marginales por regi�n
common / Unipmr / Unipmr

! Resultados de costos marginales por regi�n t�rmino de balance de energ�a
common / Unipmrgen / Unipmrgen

! Resultados de costos marginales por regi�n t�rmino de p�rdidas
common / Unipmrper / Unipmrper

! Resultados de costos marginales por regi�n t�rmino de congesti�n
common / Unipmrcon / Unipmrcon

! Resultados de costos marginales por nodo
common / Unilmp / Unilmp

! Resultados de costos marginales por nodo t�rmino de balance de energ�a
common / Unilmpgen / Unilmpgen

! Resultados de costos marginales por nodo t�rmino de p�rdidas
common / Unilmpper / Unilmpper

! Resultados de costos marginales por nodo t�rmino de congesti�n
common / Unilmpcon / Unilmpcon

! Calidad de la solucion encontrada por el AUHE
common / UniSemaf / UniSemaf

!Resultado de reserva no rodante de 10 minutos unidades de rango continuo
common / ResNR10URC / ResNR10URC ( maxurc, maxint )

!Resultado de reserva no rodante suplementaria unidades de rango continuo
common / ResNRSUURC / ResNRSUURC ( maxurc, maxint )

!Resultado de reserva no rodante de 10 minutos unidades de rango discontinuo
common / ResNR10URD / ResNR10URD ( maxurd, maxmodos, maxint )

!Resultado de reserva no rodante suplementaria unidades de rango discontinuo
common / ResNRSUURD / ResNRSUURD ( maxurd, maxmodos, maxint )

!Resultado de reserva no rodante de 10 minutos unidades hidro
common / ResNR10UH / ResNR10UH ( maxuh, maxint )

!Resultado de reserva no rodante suplementaria unidades hidro
common / ResNRSUUH / ResNRSUUH ( maxuh, maxint )

!Numero de rangos operativos para unidades de rango continuo
common / NoRaOpRC / NoRaOpRC ( maxurc )

!Limite superior de los rangos operativos para unidades de rango continuo
common / RaOpSupRC / RaOpSupRC ( maxurc, maxzonproh + 1, maxint )

!Limite superior de los rangos operativos para unidades de rango continuo
common / RaOpInfRC / RaOpInfRC ( maxurc, maxzonproh + 1, maxint )

!Limite inferior de regulacionde los rangos operativos para unidades de rango continuo
common / RaRegInfRC / RaRegInfRC ( maxurc, maxzonproh + 1, maxint )

!Limite superior de regulacion de los rangos operativos para unidades de rango continuo
common / RaRegSupRC / RaRegSupRC ( maxurc, maxzonproh + 1, maxint )

!Limite inferior de regulacionde los rangos operativos para unidades de rango continuo
common / RaRegInfH / RaRegInfH ( maxuh, maxzonproh + 1, maxint )

!Limite superior de regulacion de los rangos operativos para unidades de rango continuo
common / RaRegSupH / RaRegSupH ( maxuh, maxzonproh + 1, maxint )

!Numero de rangos operativos para unidades de rango continuo
common / NoRaOpH / NoRaOpH ( maxuh )

!Limite superior de los rangos operativos para unidades de rango continuo
common / RaOpSupH / RaOpSupH ( maxuh, maxzonproh + 1, maxint )

!Limite superior de los rangos operativos para unidades de rango continuo
common / RaOpInfH / RaOpInfH ( maxuh, maxzonproh + 1, maxint )

!Numero de nodos distribuidos por unidad de rango continuo
common / NoNodDisRC / NoNodDisRC ( maxurc, maxint )

!Factores de distribucion de generacion de las unidades de rango continuo
common / facdistgen / facdistgen ( maxurc, maxnodist, maxint )

!Numero de nodos distribuidos por carga
common / NoNodDisCar / NoNodDisCar ( maxdem, maxint )

!Factores de distribucion de cargas
common / facdistcar / facdistcar ( maxdem, maxnodist, maxint )

!Numero de nodos distribuidos por generacion no programable
common / NoNodDisNPR / NoNodDisNPR ( maxunpr, maxint )

!Factores de distribucion de cargas
common / facdistnpr / facdistnpr ( maxunpr, maxnodist, maxint )

integer MaxIntervalo, durdia, intdia, IntEquivalencia

common /  MaxIntervalo /  MaxIntervalo

! Duracion del horizonte de planeacion
common /durdia / durdia

! Numero de intervalos por dia del horizonte de planeacion
common / intdia / intdia ( MAXDIA )

!	Lista de generadores por compa�ia, Col. 1: Compra, Col. 2: Venta 
common / IntEquivalencia /  IntEquivalencia ( MAXINT )

! Numero de grupos de arranque no simultaneo para unidares de rango continuo
integer NumGruArr
common / NumGruArr / NumGruArr 

! Nombre del grupo de arranque no simultaneo para unidares de rango continuo
character*20 NomGruArr
common / NomGruArr / NomGruArr ( maxgrute )

! Numero de unidades que forman el grupo de arranque no simultaneo para unidares de rango continuo
integer numunigruarr
common / numunigruarr / numunigruarr ( maxgrute )

! Indice de la unidad que forman el grupo de arranque no simultaneo para unidares de rango continuo
integer unigruarr
common / unigruarr / unigruarr ( maxgrute, 10 )

! Numero de unidades de propiedad conjunta
integer NumUPC
common / NumUPC / NumUPC 

! Indice de la unidad principal de propiedad conjunta
integer indiceup
common / indiceup / indiceup ( maxurc/2 )

! rango de oferta de la upc
real*8 ofcapupc
common / ofcapupupc / ofcapupc ( maxurc/2, maxint )
! Indice de la unidad primer unidad complementaria de propiedad conjunta
integer indiceuc1
common / indiceuc1 / indiceuc1 ( maxurc/2 )

! Indice de la unidad segunda unidad complementaria de propiedad conjunta
integer indiceuc2
common / indiceuc2 / indiceuc2 ( maxurc/2 )

! Numero minimo de unidades a asignar reserva de regulacion por zona
integer NminRegZ
!common / NminRegZ / NminRegZ ( maxgrure )
common / NminRegZ / NminRegZ ( maxgrure, maxint )

! Minima cantidad (MW) a asignar de reserva de regulacion a unidad de rango continuo
real*8 MrreURC
common / MrreURC / MrreURC ( maxurc, maxint )
! Minima cantidad (MW) a asignar de reserva de regulacion a unidad hidro
real*8 MrreUH
common / MrreUH / MrreUH ( maxuh, maxint )
! Minima cantidad (MW) a asignar de reserva de regulacion a unidad de rango discontinuo
real*8 MrreURD
common / MrreURD / MrreURD ( maxurd, maxmodos, maxint )

! Tipo de lectura a realizar (MDA o AUHE)
character*4 TipoLec
common / TipoLec / TipoLec

! Numero de grupos con limitacion de combustible
integer NumGruGas
common / NumGruGas / NumGruGas

! Nombres de grupos con limitacion de combustible
character*20 NomGpoGas
common / NomGpoGas / NomGpoGas ( maxgrute ) 

! Bamdera para indicar si un grupo de combustible esta activo
integer GpoAct
common / GpoAct / GpoAct ( maxgrute ) 

! Limite inferior del grupo con limitacion de combustible
real*8 LimInfGas
common / LimInfGas / LimInfGas ( maxgrute, maxdia ) 

! Limite superior del grupo con limitacion de combustible
real*8 LimSupGas
common / LimSupGas / LimSupGas ( maxgrute, maxdia  ) 

! Grupos con limitacion de combustible a los que pertenecen las unidades de rango continuo
integer UniGpoGasRC
common / UniGpoGasRC / UniGpoGasRC ( maxurc, maxgrute )

! Grupos con limitacion de combustible a los que pertenecen las unidades de rango continuo
integer UniGpoGasRD
common / UniGpoGasRD / UniGpoGasRD ( maxurd, maxgrute )

! Apuntador que indica la localidad donde empiezan las unidades de rango continuo del grupo con limitacion de combustible
integer ApunURCxGpoGas
common / ApunURCxGpoGas / ApunURCxGpoGas ( maxgrute + 1 )

! Apuntador que indica la localidad donde empiezan las unidades de rango discontinuo del grupo con limitacion de combustible
integer ApunURDxGpoGas
common / ApunURDxGpoGas / ApunURDxGpoGas ( maxgrute + 1 )

! Arreglo que indica cuantas unidades de Rango Continuo hay por grupo con restriccion de combustible
integer NumURCxGpoGas
common / NumURCxGpoGas / NumURCxGpoGas ( maxgrute )

! Arreglo que indica cuantas unidades de Rango Discontinuo hay por grupo con restriccion de combustible
integer NumURDxGpoGas
common / NumURDxGpoGas / NumURDxGpoGas ( maxgrute )

! Lista de unidades de rango continio de los grupos con limitacion de energia
integer UniRCxGpoGas
common / UniRCxGpoGas / UniRCxGpoGas ( maxurc )

! Lista de unidades de rango discontinio de los grupos con limitacion de energia
integer UniRDxGpoGas
common / UniRDxGpoGas / UniRDxGpoGas ( maxurd )

! Bandera para indicar (=1) las unidades de rango continuo que representan importaciones
integer ImpEnt
common / ImpEnt / ImpEnt ( maxurc )

! Bandera para indicar (=1) las unidades de rango continuo que representan importaciones
integer ExpEnt
common / ExpEnt / ExpEnt ( maxdem )

! Numero de restricciones de combustible
integer NResComb
common / NResComb / NResComb

! Numero de grupo de unidades asociado a la restriccion de combustible
integer NGpoResComb
common / NGpoResComb / NGpoResComb ( maxgrute*15 )

! Nombre del grupo de unidades asociado a la restriccion de combustible
character*12 NomGpoResComb
common / NomGpoResComb / NomGpoResComb ( maxgrute*15 )

! Intervalo inicial asociado a la restriccion de combustible
integer HIResComb
common / HIResComb / HIResComb ( maxgrute*15 )

! Intervalo final asociado a la restriccion de combustible
integer HFResComb
common / HFResComb / HFResComb ( maxgrute*15 )

! Limite inferior asociado a la restriccion de combustible
real*8 LInfResComb
common / LInfResComb / LInfResComb ( maxgrute*15 )

! Limite superior asociado a la restriccion de combustible
real*8 LSupResComb
common / LSupResComb / LSupResComb ( maxgrute*15 )

! Bandera para indicar si la restriccion de combustible esta activa
integer ActResComb
common / ActResComb / ActResComb ( maxgrute*15 )

real*8 PenLimComb
! Penalizacion por violacion en la limitacion de combustible
common / PenLimComb  / PenLimComb

! Numero de restricciones de energia termica
integer NResEner
common / NResEner / NResEner

! Numero de grupo de unidades asociado a la restriccion de energia termica
integer NGpoResEner
common / NGpoResEner / NGpoResEner ( maxgrute*15 )

! Nombre del grupo de unidades asociado a la restriccion de energia termica
character*12 NomGpoResEner
common / NomGpoResEner / NomGpoResEner ( maxgrute*15 )

! Intervalo inicial asociado a la restriccion de energia termica
integer HIResEner
common / HIResEner / HIResEner ( maxgrute*15 )

! Intervalo final asociado a la restriccion de energia termica
integer HFResEner
common / HFResEner / HFResEner ( maxgrute*15 )

! Limite inferior asociado a la restriccion de energia termica
real*8 LInfResEner
common / LInfResEner / LInfResEner ( maxgrute*15 )

! Limite superior asociado a la restriccion de energia termica
real*8 LSupResEner
common / LSupResEner / LSupResEner ( maxgrute*15 )

! Bandera para indicar si la restriccion de energia termica esta activa
integer ActResEner
common / ActResEner / ActResEner ( maxgrute*15 )

! Alerta en definici�n de limites de regulacion de unidades de rango continuo
integer alertaregRC
common / alertaregRC / alertaregRC ( maxurc, maxint )

! Alerta en definici�n de limites de regulacion de unidades de rango discontinuo
integer alertaregRD
common / alertaregRD / alertaregRD ( maxurd, maxint )

! Alerta en definici�n de limites de regulacion de unidades hidro
integer alertaregUH
common / alertaregUH / alertaregUH ( maxuh, maxint )

! Bumero de grupos de importacion
integer NumGpoImp
common / NumGpoImp / NumGpoImp

! Apunta a inicio de informacion de unidades del grupo de importacion
integer ApuUniGpoImp
common / ApuUniGpoImp / ApuUniGpoImp ( maxgrute + 1 )

! Lista de unidades del grupo de importacion
integer LisUniGpoImp
common / LisUniGpoImp / LisUniGpoImp ( maxgrute*maxgrute )

! Nombre de grupo de importacion
character*12 NomGpoImp
common / NomGpoImp / NomGpoImp ( maxgrute )

! Numero de unidades del grupo de importacion
integer NumUniGpoImp 
common / NumUniGpoImp / NumUniGpoImp ( maxgrute )

! Potencia maxima del grupo de importacion por intervalo
real*8 PotMaxGpoImp 
common / PotMaxGpoImp / PotMaxGpoImp ( maxgrute, maxint )

! Bumero de grupos de Exportacion
integer NumGpoExp
common / NumGpoExp / NumGpoExp

! Apunta a inicio de informacion de cargas del grupo de exportacion
integer ApuCarGpoExp
common / ApuCarGpoExp / ApuCarGpoExp ( maxgrute + 1 )

! Lista de cargas del grupo de exportacion
integer LisCarGpoExp
common / LisCarGpoExp / LisCarGpoExp ( maxgrute*maxgrute )

! Nombre de grupo de Exportacion
character*12 NomGpoExp
common / NomGpoExp / NomGpoExp ( maxgrute )

! Numero de cargas del grupo de Exportacion
integer NumCarGpoExp 
common / NumCarGpoExp / NumCarGpoExp ( maxgrute )

! Potencia maxima del grupo de Exportacion por intervalo
real*8 PotMaxGpoExp 
common / PotMaxGpoExp / PotMaxGpoExp ( maxgrute, maxint )

! Numero de grupos de ramas en el conjunto activo inicial
integer RamActIni
common / RamActIni / RamActIni

! KVBase del nodo
real*8 kvbase
common / kvbase / kvbase ( maxnod )

! Limite inferior de potencia de generacion de una unidad de rango continuo de oferta 
real*8 PotMinOfeGRC
common / PotMinOfeGRC / PotMinOfeGRC  ( maxurc, maxint )

! URIEL GAP perdidas calculado
real*8 GAPperdidas
common / GAPperdidas / GAPperdidas

! URIEL GAP perdidas requerido
real*8 GAPperreq
common / GAPperreq / GAPperreq

End Module


