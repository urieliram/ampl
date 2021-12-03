set BUS ordered;
set LOOPS;
set BRANCH within { BUS , BUS } ;
set LOOP within { LOOPS,  BRANCH };

param dem{ BUS } >= 0;
param aa{ BUS } >= 0;
param bb{ BUS } >= 0;
param cc{ BUS } >= 0;
param pmin{ BUS } >= 0;   
param pmax{ BUS } >= 0;
param loss{ BUS } >= 0;
param r{ BRANCH } >= 0;  
param x{ BRANCH } >= 0;     
param shunt{ BRANCH } >= 0;   
param fmin{ BRANCH } >= 0;   
param fmax{ BRANCH } >= 0;
param sense{LOOP};

var f{ BRANCH } >= 0;
var p{ BUS } >= 0;

minimize Total_Cost:
sum { i in BUS } ((1/2)*cc[i] * p[i] * p[i] + bb[i] * p[i] + aa[i]  );

subject to BalanceNode {k in BUS}:
-(sum{ m in BUS:(k,m) in BRANCH } f[k,m]) 
+(sum{ i in BUS:(i,k) in BRANCH } f[i,k]) 
+ p[k] - dem[k] = 0;

subject to CapacityMax {(i,j) in BRANCH}:
f[i,j]  <= fmax[i,j];

subject to CapacityMin {(i,j) in BRANCH}:
f[i,j]  >= fmin[i,j];

subject to PowerMax {i in BUS}:
p[i]  <= pmax[i];

subject to PowerMin {i in BUS}:
p[i]  >= pmin[i];

subject to Ciclo{k in 1..card(LOOPS)}:
sum{ (i,j) in BRANCH:(k,i,j) in LOOP} f[i,j]*x[i,j]*sense[k,i,j] = 0;