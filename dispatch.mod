# conjuntos

set UNIDADES ;
set NODOS ;
set ESCENARIOS ;

# parametros

param MaxGen {UNIDADES};
param CostUnit {UNIDADES};
param CostResU {UNIDADES};
param CostResD {UNIDADES};

param UbGen {UNIDADES, NODOS};

param CargaNodo {NODOS};
param NodosAdyacentes {NODOS, NODOS};
param Reactancia {NODOS, NODOS};
param CapacTransmision {NODOS, NODOS};
param GenViento {NODOS};
param VientoPron {NODOS};
param ProbEsce {ESCENARIOS};
param VientoMax {ESCENARIOS};

param PerdidaCarga;

# variable no negativas

var P {UNIDADES} >= 0;
var Ru {UNIDADES} >= 0;
var Rd {UNIDADES} >= 0;
var ru {UNIDADES, ESCENARIOS} >= 0;
var rd {UNIDADES, ESCENARIOS} >= 0;
var Ws {ESCENARIOS} >= 0;
var Ls {NODOS, ESCENARIOS} >= 0;

var del0 {NODOS};
var del {NODOS, ESCENARIOS};

# funcion objetivo

minimize Costo_Operacion : sum {u in UNIDADES} CostUnit[u]*P[u] + sum {u in UNIDADES} CostResU[u]*Ru[u] + sum {u in UNIDADES} CostResD[u]*Rd[u]  +  
							sum {s in ESCENARIOS} ProbEsce[s]*(sum {u in UNIDADES} CostUnit[u]*(ru[u,s]-rd[u,s]) + PerdidaCarga*sum {n in NODOS}Ls[n,s]);


# Restricciones de Operacion del Dia En Adelanto

subject to BalancePotencia {n in NODOS}:  sum {u in UNIDADES} UbGen[u,n]*P[u] + GenViento[n]*VientoPron[n] - CargaNodo[n] == sum {m in NODOS} NodosAdyacentes[n,m]*(del0[n]-del0[m])/0.13;
subject to NodoSetting: del0['N1'] == 0;

subject to TransmissionCapacity {n in NODOS}: sum {m in NODOS} NodosAdyacentes[n,m]*(del0[n]-del0[m])/0.13 <= sum {m in NODOS} CapacTransmision[n,m];

subject to EnergiaReservaAjenaU {u in UNIDADES} : P[u] + Ru[u] <= MaxGen[u];
subject to EnergiaReservaAjenaD {u in UNIDADES} : P[u] - Rd[u] >= 0;

# Restricciones de Operacion

subject to BalanceDemandaGeneracion {n in NODOS, s in ESCENARIOS} :  sum {u in UNIDADES} UbGen[u,n]*(ru[u,s] - rd[u,s]) + Ls[n,s] + VientoMax[s] - VientoPron[n] - GenViento[n]*Ws[s] == sum {m in NODOS} (del[n,s] - del0[n] + del0[m] - del[m,s])/0.13;
subject to PostBalanceTransCap {n in NODOS, s in ESCENARIOS}: sum {m in NODOS} NodosAdyacentes[n,m]*(del[n,s]-del[m,s])/0.13 <= sum {m in NODOS} CapacTransmision[n,m];
subject to NodoSetting2 : del0['N1'] == 0;
subject to NodoSettingEsc {s in ESCENARIOS} :  del['N1',s] == 0;
subject to WindProdCurtailed {s in ESCENARIOS} : Ws[s]<= VientoMax[s];
subject to BalanceCapReservaU {u in UNIDADES, s in ESCENARIOS} : ru[u,s] <= Ru[u];
subject to BalanceCapReservaD {u in UNIDADES, s in ESCENARIOS} : rd[u,s] <= Rd[u];