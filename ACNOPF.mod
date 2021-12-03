set BUS;
set LOOPS;
set BRANCH within { BUS , BUS };
set LOOP within { LOOPS,  BRANCH };

param dem_r{ BUS } ;
param dem_m{ BUS } ;
param S{ BUS } >= 0;
param aa{ BUS } >= 0;
param bb{ BUS } >= 0;
param cc{ BUS } >= 0;
param r{ BRANCH } >= 0;  
param x{ BRANCH } >= 0;     
param shunt{ BRANCH } >= 0; 
param e{ BUS } >= 0;     
param sense{ LOOP };

param ibus_r{ BUS }; #Bus injected current
param ibus_m{ BUS }; #Bus injected current

param igmin_r{ BUS } >= 0;  
param igmax_r{ BUS } >= 0;     
param igmin_m{ BUS } >= 0;    
param igmax_m{ BUS } >= 0; 

# The variables of ACNOPF formulation are branch currents (iline) 
# and generator injected currents (ig) that corresponds to power generation.
# where upper indices r and m are related to real and imaginary parts of variables, respectively.
var iline_r{ BRANCH } >= 0; # real component
var ig_r{ BUS } >= 0; 		# real component
var iline_m{ BRANCH } >= 0; # imaginary component
var ig_m{ BUS } >= 0; 		# imaginary component

minimize Total_Cost:
sum { i in BUS } ( (1/2) * cc[i]  * ig_r[i] * ig_r[i] + bb[i] * ig_r[i] + aa[i] 
                 + (1/2) * cc[i]  * ig_m[i] * ig_m[i] + bb[i] * ig_m[i] + aa[i]  );

subject to BalanceNode_r {k in BUS}:
-(sum{ m in BUS:(k,m) in BRANCH } iline_r[k,m]) 
+(sum{ i in BUS:(i,k) in BRANCH } iline_r[i,k]) 
+ ig_r[k] - ibus_r[k]  = 0;  

subject to BalanceNode_m {k in BUS}:
-(sum{ m in BUS:(k,m) in BRANCH } iline_m[k,m]) 
+(sum{ i in BUS:(i,k) in BRANCH } iline_m[i,k]) 
+ ig_m[k] - ibus_m[k] = 0; 

subject to Ciclo_r{k in 1..card(LOOPS)}:
sum{ (i,j) in BRANCH:(k,i,j) in LOOP} iline_r[i,j]*r[i,j]*sense[k,i,j] = 0;

subject to Ciclo_m{k in 1..card(LOOPS)}:
sum{ (i,j) in BRANCH:(k,i,j) in LOOP} iline_m[i,j]*x[i,j]*sense[k,i,j] = 0;

subject to CurrentMax_r {i in BUS}:
ig_r[i]  <= igmax_r[i];

subject to CurrentMin_r {i in BUS}:
ig_r[i]  >= igmin_r[i];

subject to CurrentMax_m {i in BUS}:
ig_m[i]  <= igmax_m[i];

subject to CurrentMin_m {i in BUS}:
ig_m[i]  >= igmin_m[i];