##  Se tiene un tanque de capacidad máxima M > 0 y capacidad mínima L > 0.
##  Se requiere que al final de cada día el tanque no contenga menos de S > 0
##  unidades. Cada unidad menos que contenga provoca una penalización de D
##  pesos.
##  Hay T > 0 modos de comprar la mercancía y, para j = 1, ..., T, el j-ésimo modo
##  ofrece Qj unidades con un costo de envío de Rj pesos.
##  El modelo cubre N > 0 días y, para cada i = 1, ...,N, el precio de compra unitario
##  es P(i) y el precio de venta unitario es W(i). Se pronostica que en el i-ésimo día
##  se vendan V(i) unidades. Por cada unidad no abastecida hay una penalización
##  de E pesos.
##  La cantidad de pedidos de tipo j hechos en el i-ésimo día serán x_j_i
##  Las unidades que se vendieron en el i-ésimo día serán y(i).
##  Al principio del periodo el tanque cuenta con F0 >= L unidades.
##  Al final del i-ésimo día el tanque cuenta con F_i unidades

##  DISCUSIÓN DEL PROBLEMA
##  El modelo incluye T modos de abastecimiento con los parámetros de cantidad Q y costo 
##  de envío R.
##  En la situación de contar con un único camión abastecedor, los modos se refieren a la
##  cantidad que carga el camión y en el costo de envío se puede incluir una penalización 
##  para cargas que se desee disminuir.
##  Por ejemplo, con T=3, el camión puede cargar Q1 = 10000, Q2 = 20000 ó Q3 = 40000 unidades
##  y si el costo es el mismo para cualquier cantidad pero es preferible recibir cargas chicas
##  podemos hacer, por ejemplo, R1 = 1000, R2 = 1000+500, R3 = 1000+1000.
##  Ahora bien, para incluir la opción de hacer dos entregas chicas se puede agregar un cuarto modo,
##  con el doble de cantidad y el doble de costo que el modo chico. De esta forma la restricción de 
##  hacer una sola entrega al día puede incluir entregas múltiples en un día.
##  Si no contamos las restricciones de no negatividad, para N días se tendrían 5N restricciones 
##  y (T+2)N variables (+ 5N holguras para las desigualdades).

set DIAS;
set MODOS;
set aux;

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

param costoEnvio  {MODOS};
param capunidades {MODOS};

param maximopipas {DIAS, MODOS};

var xP {i in DIAS, j in MODOS} >= 0 integer; # cantidad de pipas
var xV {i in DIAS}             >= 0 integer; # cantidad de litros vendidos
var d  {i in DIAS}             >= 0 integer; # litros vendidos con penalizacion 

#maximize Ganancia : sum {i in DIAS, j in MODOS}(costoEnvio[j] + capunidades[j]*costo[i]) * xP[i,j] + sum {i in DIAS}(costo[i] + E) * xV[i]  - sum {i in DIAS}(pron[i]*E  + d[i]*D);
maximize Ganancia : sum {i in DIAS}  ( W[i] * xP[i]  - sum {i in MODOS} ( pron[i]*E  + d[i]*D);

subject to capacidad {i in DIAS} : sum {j in MODOS} sum {k in aux: k <= i} xP[k,j]*capunidades[j] - sum {k in aux: k <= i-1} xV[k] <= M - F0;
subject to abasto {i in DIAS} : - sum {j in MODOS} sum {k in aux: k <= i} xP[k,j]*capunidades[j] + sum {k in aux: k <= i} xV[k] <= F0 - L;
subject to pipapordia {i in DIAS} : sum {j in MODOS} xP[i,j] <= sum {j in MODOS} maximopipas[i,j];
subject to reststock {i in DIAS} : - sum {j in MODOS} sum {k in aux: k <= i}(-xP[k,j]*capunidades[j]) + sum {k in aux: k <= i} xV[k] + xV[i] - d[i] <= F0 - nivelfin[i];
subject to ventasPron {i in DIAS} : xV[i] <= pron[i];




