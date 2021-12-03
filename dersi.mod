###########################################################################
#### DESPACHO ECONOMICO CON RESTICCIONES DE SEGURIDAD                  ####
####                                                                   ####
####                                                                   ####
#### FECHA DE INICIO: 17 DE AGOSTO DEL 2019                            ####
#### Documento generado sin acentos                                    ####
###########################################################################

#######  CONJUNTOS DEL DERS-I

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set SegOferta;         # {b} Segmentos de Ofertas/Requerimiento.
set RamasElect;        # {br} Grupos de ramas electricas.
set CentHidro;         # {ci} Centrales Hidroelectricas.
set PronDemanda;       # {d} Pronostico de demandas.
set ModoOperacionRD;    # {m} Modos de operacion de unidades con rango discontinuo.
set NodosElec;         # {n} Nodos electricos del sistema. 
set UnidadesDersiRC;   # {u} Unidades de Rango Continuo que participan en el despacho
set UnidadesDersiRD;   # {u} Unidades Rango Discontinuo que participan en el despacho
set UnidadesDersiH;    # {u} Unidades Hidroelectricas que participan en el despacho
set UnidadesDersiRE;   # {u} Unidades Renovables que participan en el despacho .
set UnidadesDersiNPR;  # {u} Unidades No Programables que participan en el despacho .
set UnidadesDersi := UnidadesDersiRC union UnidadesDersiRD union UnidadesDersiH union UnidadesDersiRE union UnidadesDersiNPR;

#######  CONSTANTES DEL DERS-I

param CD;               # {CD}        Costo de la energia no suministrada; en $/MWh.
param CS;               # {CS}        Penalizacion por exedente de generacion; en $/MWh.
param ContFluMax {RamasElect};; 	    # {\bar{fn}}  Limite maximo para el contraflujo en la rama electrica, en MW.
param FluMax {RamasElect};           # {\bar{fp}}  Limite maximo para el flujo en la rama electrica, en MW.
param potenciaMinRD {UnidadesDersiRD, ModoOperacionRD};      # {\underline{g}} Potencia minima operativa; en MW.
param potenciaMaxRD {UnidadesDersiRD, ModoOperacionRD};      # {\bar{g}}   Potencia maxima operativa; en MW.
param potenciaMin  {UnidadesDersi};
param potenciaMax  {UnidadesDersi};
param GB {SegOferta, UnidadesDersi};
param GBRD {SegOferta, ModoOperacionRD, UnidadesDersiRD};   # {GB} Potencia de venta de energia; en MW.
param intercambioPot;   # {IN}        Intercambio de potencia activa, en MW.
param penVioFlujo;      # {P^BR}      Penalizacion por violacion en el flujo de los grupos de ramas electricas; en $/MW.
param costoOportH {UnidadesDersiH};      # {P^COP}     Costo de oportunidad para unidades hidroelectricas basado en el costo del agua a corto plazo, en $/MWh.
param precioVentaEn {SegOferta, UnidadesDersi};    # {P^OVE}     Precio de oferta de venta de energia; en $/MWh.
param pronDemanda {PronDemanda};      # {PD}        Pronostico de demanda; en MW.
param resistenciaElec;  # {R}         Resistencia electrica de la rama electrica.
param rampaBajada;      # {RB}        Rampa de bajada para operación; en MW/h.
param capGenRRegS {UnidadesDersi};
param capGenRRegSRD {UnidadesDersi, ModoOperacionRD};		# {\hat{rre}} Capacidad de generacion asignada para reserva de regulacion secundaria; en MW.
param capGenRRod10 {UnidadesDersi};
param capGenRRod10RD {UnidadesDersi, ModoOperacionRD};     # {\hat{rro}^10} Capacidad de generacion asignada para reserva rodante de dies minutos, en MW.
param capGenRRodS {UnidadesDersi};
param capGenRRodSRD {UnidadesDersi, ModoOperacionRD};      # {\hat{rro}^S}  Capacidad de generacion asignada para reserva rodante suplementaria; en MW.
param rampaRegSec;		# {RR}        Rampa para regulacion secundaria en x minutos; en MW
param rampaSubida;      # {RS}        Rampa de subida para operacion; en MW/h
param sensFlujo;        # {sfbr}      Sensibilidad de flujo en la rama electrica con respecto a los cambios en las inyecciones de neta potencia en los nodos; adimensional.
param sensPerdida;      # {sper}      Sensibilidad de perdidas en el nodo con respecto a los cambios en las inyecciones netas de potencia en el nodo; adimensional.


#######  VARIABLES

var corteCarga {PronDemanda};           # {df}  Corte de carga; en MW.
var excFlujoRamElec {RamasElect};      # {f+}  Variable artificial de excedente de flujo en una rama electrica; en MW.
var excConFluRamElec {RamasElect};     # {f-}  Variable artificial de excedente de contraflujo en una rama electrica; en MW.
var nivelGeneracion {UnidadesDersi};      # {g}   Nivel de generacion total de venta de energia; en MW.
var genSegOferta {SegOferta, UnidadesDersi};         # {gb}  Generacion del segmento de la oferta de venta de enrgia; en MW.
var artReqEneFija;        # {gef} Variable artificial para satisfacer el requerimeinto de energia fija cuando hay un d�ficit o excendente; en MW/h
var inyecPotencia {NodosElec};        # {iny} Inyeccion neta de potencia en un nodo; en MW.
var perdidasIsla;         # {Per} Perdidas en la isla electrica.
var excGeneracion {NodosElec};        # {Y}   Excedente de generacion; en MW.

minimize Costo_Total : (sum {u in UnidadesDersiRC} sum {b in SegOferta} precioVentaEn[b,u] * genSegOferta[b,u]) +
                       (sum {u in UnidadesDersiRD} sum {b in SegOferta} precioVentaEn[b,u] * genSegOferta[b,u]) +
                       (sum {u in UnidadesDersiH} costoOportH[u] * nivelGeneracion[u]) +
                       (sum {u in UnidadesDersiRE} sum {b in SegOferta} precioVentaEn[b,u] * genSegOferta[b,u]) + # las siguientes son costos de penalizaciones
                       (sum {n in NodosElec} CS * excGeneracion[n]) +  # penalizacion por excedente de generacion en los nodos
                       (sum {d in PronDemanda} CD * corteCarga[d]) +    # penalizacion por corte de carga por demanda participante
                       (sum {br in RamasElect} penVioFlujo * (excFlujoRamElec[br] + excConFluRamElec[br]));   # penalizacion por violar limites en los grupos de ramas electricas

###################  RESTRICCIONES

####  UNIDADES CON RANGO DE OPERACION CONTINUO

##    NIVEL DE GENERACION (eq 3)

subject to NivelGenRC {u in UnidadesDersiRC} : nivelGeneracion[u] = potenciaMin[u] + sum{b in SegOferta} genSegOferta[b,u];

##    SEGMENTOS DE OFERTAS DE VENTA DE ENERGIA (eq 4) 
 
subject to GenOfertaVentaRC {u in UnidadesDersiRC, b in SegOferta} : 0 <= genSegOferta[b,u] <= GB[b,u];   # Revisar que asi se escriba la doble desigualdad

##    LIMITE MAXIMO DE GENERACION OPERATIVO  (eq 5)

subject to LimMaxGenOpRC {u in UnidadesDersiRC} :  nivelGeneracion[u] + capGenRRegS[u] + capGenRRod10[u] + capGenRRodS[u] <= potenciaMax[u];

##    LIMITE MINIMO DE GENERACION OPERATIVO (eq 6)

subject to LimMinGenOpRC {u in UnidadesDersiRC} : nivelGeneracion[u] - capGenRRegS[u] >= potenciaMin[u];

########  UNIDADES CON RANGO DE OPERACION DISCONTINUO

##    NIVEL DE GENERACION  (eq 7)

subject to NivelGenRD {u in UnidadesDersiRD, m in ModoOperacionRD} : nivelGeneracion[u] = potenciaMinRD[u,m] + sum{b in SegOferta} genSegOferta[b,u];  # Preguntar qu� onda con los modos. �Como estan en los archivos?

##    SEGMENTOS DE OFERTAS DE VENTA DE ENERGIA (eq 8)
 
subject to GenOfertaVentaRD {u in UnidadesDersiRD, m in ModoOperacionRD, b in SegOferta} : 0 <= genSegOferta[b,u] <= GBRD[b,m,u];

##    LIMITE MAXIMO DE GENERACION OPERATIVO (eq 9)

subject to LimMaxGenOpRD {u in UnidadesDersiRD, m in ModoOperacionRD} :  nivelGeneracion[u] + capGenRRegSRD[u,m] + capGenRRod10RD[u,m] + capGenRRodSRD[u, m] <= potenciaMaxRD[u,m];

##    LIMITE MINIMO DE GENERACION OPERATIVO (eq 10)

subject to LimMinGenOpRD {u in UnidadesDersiRD, m in ModoOperacionRD} :  nivelGeneracion[u] - capGenRRegSRD[u,m]  >= potenciaMinRD[u,m];                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        

########  UNIDADES DE CENTRALES HIDROELECTRICAS

##    OFERTA DE VENTA DE ENERGIA

    # Unidades hidroeléctricas que pertenecen a un mismo embalse 
    # ofrecen un mismo precio de oportunidad para la generación
    # de energia.

##    LIMITE MAXIMO DE GENERACION OPERATIVO (eq 11)

subject to LimMaxGenOpH {u in UnidadesDersiH} : nivelGeneracion[u] + capGenRRod10[u] + capGenRRodS[u] + capGenRRegS[u] <= potenciaMax[u];

##    LIMITE MINIMO DE GENERACION OPERATIVO  (eq 12)

subject to LimMinGenOpH {u in UnidadesDersiH} : nivelGeneracion[u] - capGenRRegS[u] >= potenciaMin[u];


########  UNIDADES RENOVABLES INTERMITENTES

##    NIVEL DE GENERACION  (eq 13)

subject to NivelGenRE {u in UnidadesDersiRE} : nivelGeneracion[u] = sum {b in SegOferta} genSegOferta[b,u];

##    SEGMENTOS DE OFERTA DE VENTA DE ENERGIA (eq 14)

subject to SegOfertasVentEner {u in UnidadesDersiRE, b in SegOferta} : 0 <= genSegOferta[b,u] <= GB[u,b];

##    LIMITE MAXIMO DE GENERACION OPERATIVO (eq 15)

subject to LimMaxGenOpRE {u in UnidadesDersiRE} : nivelGeneracion[u] <= potenciaMax[u]; 

##    LIMITE MINIMO DE GENERACION OPERATIVO (eq 16)

subject to LimMinGenOpRE {u in UnidadesDersiRE} : nivelGeneracion[u]  >= potenciaMin[u];


########  DEMANDA

##    NIVEL DE DEMANDA

	# El pronostico de la demanda utilizada en el DERS-MI para los quince minutos es 
	# remplazado en el DERS-I por el valor de la demanda proveniente del estimador de estado.

####   RESTRICCIONES DEL SISTEMA ELECTRICO

## BALANCE DE POTENCIA  (eq 18)

 subject to BalancePotencia  : sum {n in NodosElec} inyecPotencia[n] + perdidasIsla = 0;

## REPRESENTACION TANGENCIAL DE PERDIDAS DE TRANSMISION

## LIMITE SOBRE FLUJOS EN GRUPOS DE RAMAS ELECTRICAS

##  COTAS SIMPLES

subject to FlujoRamas {br in RamasElect} : 0 <= excFlujoRamElec[br] <= FluMax[br];  # eq 25
subject to ContraFlujoRamas {br in RamasElect} : 0<= excConFluRamElec[br] <= ContFluMax[br]; # eq 26
# subject to X >= 0;   # eq 27
subject to ExcGeneracion {n in NodosElec}: excGeneracion[n] >= 0;   # eq 28

#### 

####  FACTORES DE PARTICIPACION DE REGULACION PARA UNIDADES EN MODO AUTOMATICO





















