 set DIAS;
 set aux;
 set MODOS;
 
param D;
param E;
param L;
param M;
param F0;

param costo {DIAS};
param precio {DIAS};
param pron {DIAS};
param penescazes {DIAS};
param nivelfin {DIAS};
param stock {DIAS};
 
param costoEnvio {MODOS};
param capunidades {MODOS};

param maximopipas {DIAS, MODOS};

var  xP {i in DIAS, j in MODOS} >= 0 integer; # cantidad de pipas
var xV {i in DIAS} >= 0; # cantidad de litros vendidos
var d  {i in DIAS} >= 0; # litros vendidos con penalizacion 

maximize Ganancia : sum {i in DIAS, j in MODOS}(costoEnvio[j] + capunidades[j]*costo[i]) * xP[i,j] + sum {i in DIAS}(costo[i] + E) * xV[i]  - sum {i in DIAS}(pron[i]*E  + d[i]*D);

subject to capacidad {i in DIAS} : sum {j in MODOS} sum {k in aux: k <= i} xP[k,j]*capunidades[j] - sum {k in aux: k <= i-1} xV[k] <= M - F0;
subject to abasto {i in DIAS} : - sum {j in MODOS} sum {k in aux: k <= i} xP[k,j]*capunidades[j] + sum {k in aux: k <= i} xV[k] <= F0 - L;
subject to pipapordia {i in DIAS} : sum {j in MODOS} xP[i,j] <= sum {j in MODOS} maximopipas[i,j];
subject to reststock {i in DIAS} : - sum {j in MODOS} sum {k in aux: k <= i}(-xP[k,j]*capunidades[j]) + sum {k in aux: k <= i} xV[k] + xV[i] - d[i] <= F0 - nivelfin[i];
subject to ventasPron {i in DIAS} : xV[i] <= pron[i];




