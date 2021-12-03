#https://vanderbei.princeton.edu/ampl/nlmodels/index.html

set BUS;    # set of buses
set BRANCH within {1..4000} cross BUS cross BUS; # set of branches

# bus parameters

param bus_type       {BUS};            # type SL = 3, PV = 2, PQ = 0
param bus_name       {BUS} symbolic;   # name
param bus_voltage    {BUS};            # voltage
param bus_angle0     {BUS};            # initial angle
param bus_p_gen      {BUS};            # active power generation
param bus_q_gen      {BUS};            # reactive power generation
param bus_q_min      {BUS};            # reactive power generation lower limit
param bus_q_max      {BUS};            # reactive power generation upper limit
param bus_p_load     {BUS};            # active power load
param bus_q_load     {BUS};            # reactive power load
param bus_g_shunt    {BUS};            # shunt condutance
param bus_b_shunt    {BUS};            # shunt susceptance
param bus_b_shunt_min{BUS};            # shunt susceptance lower limit
param bus_b_shunt_max{BUS};            # shunt susceptance upper limit
param bus_b_dispatch {BUS};            # flag indicator of a dispatchable inductor/capacitor bank
param bus_area       {BUS};            # area

# branch parameters

param branch_from   {BRANCH};          # "from" bus
param branch_to     {BRANCH};          # "to" bus
param branch_type   {BRANCH};          # type 0=transmission line 1=fixed ratio transformer 2=OLTC 4=phase shifter 
param branch_r      {BRANCH};          # branch resistance
param branch_x      {BRANCH};          # branch reactance
param branch_c      {BRANCH};          # branch capacitance
param branch_tap    {BRANCH};          # on phase transformer tap ratio 
param branch_tap_min{BRANCH};          # on phase transformer tap ratio lower limit 
param branch_tap_max{BRANCH};          # on phase transformer tap ratio upper limit
param branch_shf0   {BRANCH};          # phase shifter initial angle 
param branch_shf_min{BRANCH};          # phase shifter angle lower limit
param branch_shf_max{BRANCH};          # phase shifter angle upper limit


param p_gen_upper := 1.10;              # upper percentual limit on active power generation
param deg_2_rad   := 3.14159/180;       # conversion from degrees to radians
param bmva        := 100;               # base MVA rating

var bus_angle   {BUS};                 # bus angles
var branch_shf  {(l,k,m) in BRANCH} >= branch_shf_min[l,k,m], <= branch_shf_max[l,k,m]; # phase shifter angles

var branch_p_flow {BRANCH}; # final active direct flow, used to output data

# matrix YBUS

set YBUS := setof{i in BUS} (i,i)           union 
            setof {(l,k,m) in BRANCH} (k,m) union
            setof {(l,k,m) in BRANCH} (m,k);

param B{(k,m) in YBUS} := if(k == m)      then (sum{(l,k,i) in BRANCH} (0) 
                                               +sum{(l,i,k) in BRANCH} (0))
                          else if(k != m) then (sum{(l,k,m) in BRANCH} 1/branch_x[l,k,m]
                                               +sum{(l,m,k) in BRANCH} 1/branch_x[l,m,k]);


# active power generation

var p_gen {k in BUS} = bus_p_load[k] + sum{(k,m) in YBUS} (B[k,m]*(bus_angle[k] - bus_angle[m] + 
                                       sum{(l,k,m) in BRANCH} branch_shf[l,k,m]+sum{(l,m,k) in BRANCH} -branch_shf[l,m,k]));

# problem definition 

minimize active_power : sum {k in BUS : bus_type[k] == 2 || bus_type[k] == 3} 
         bus_p_load[k] + sum{(k,m) in YBUS} (B[k,m]*(bus_angle[k] - bus_angle[m] + 
         sum{(l,k,m) in BRANCH} branch_shf[l,k,m]+sum{(l,m,k) in BRANCH} -branch_shf[l,m,k]));
 
subject to p_load {k in BUS : bus_type[k] == 0}:
        bus_p_gen[k] - bus_p_load[k] - sum{(k,m) in YBUS} (B[k,m]*(bus_angle[k] - bus_angle[m] + 
        sum{(l,k,m) in BRANCH} branch_shf[l,k,m]+sum{(l,m,k) in BRANCH} -branch_shf[l,m,k])) = 0;

subject to p_generation {k in BUS : bus_type[k] == 2 || bus_type[k] == 3}:
  0 <=  bus_p_load[k] + sum{(k,m) in YBUS} (B[k,m]*(bus_angle[k] - bus_angle[m] + 
        sum{(l,k,m) in BRANCH} branch_shf[l,k,m]+sum{(l,m,k) in BRANCH} -branch_shf[l,m,k])) <= p_gen_upper*bus_p_gen[k];

data; # data section

# bus data input

param: BUS: bus_type bus_name bus_voltage bus_angle0 bus_p_gen bus_q_gen
            bus_q_min bus_q_max bus_p_load bus_q_load bus_g_shunt bus_b_shunt
            bus_b_shunt_min bus_b_shunt_max bus_b_dispatch bus_area := 
include IEEE014a.bus;

# branch data input

param: BRANCH: branch_type branch_r branch_x branch_c
               branch_tap branch_tap_min branch_tap_max branch_shf0 
               branch_shf_min branch_shf_max :=
include IEEE014a.branch;

# data initialization

for{i in BUS} {
   let bus_angle[i]   := bus_angle0[i]*deg_2_rad; 
   let bus_p_gen[i]   := bus_p_gen[i]/bmva;
   let bus_p_load[i]  := bus_p_load[i]/bmva;
  };

for{(l,k,m) in BRANCH} {
   let branch_shf[l,k,m]     := -branch_shf0[l,k,m]*deg_2_rad;
   let branch_shf_min[l,k,m] :=  branch_shf_min[l,k,m]*deg_2_rad;
   let branch_shf_max[l,k,m] :=  branch_shf_max[l,k,m]*deg_2_rad;  
};

# fixing variables

fix {i in BUS : bus_type[i] == 3} bus_angle[i];                     # slack angle fixed
fix {(l,k,m) in BRANCH : branch_type[l,k,m] !=4} branch_shf[l,k,m]; # phase shifters are branch_type 4

option minos_options "summary_file=6 summary_frequency=5 timing=1";
option loqo_options "verbose=2 timing=1";
option lancelot_options "timing=1";
option snopt_options "timing=1";

solve;

# calculates active branch flow 

for{(l,k,m) in BRANCH} 
   let branch_p_flow[l,k,m] := (bus_angle[k]-bus_angle[m]+branch_shf[l,k,m])/branch_x[l,k,m];

# generates output file

printf "  #      Name    Voltage  Angle     PGen    PLoad  To    PFlux   \n" > dcopf.stt;
printf "--------------------------------------------------------------\n" >> dcopf.stt; 
for{i in BUS} {
printf "%4d %s %6.4f %6.2f %8.2f %8.2f", i, bus_name[i], bus_voltage[i], bus_angle[i]/deg_2_rad,
p_gen[i]*bmva, bus_p_load[i]*bmva >> dcopf.stt;

printf " ------------\n" >> dcopf.stt;

for{(l,i,m) in BRANCH} 
printf "%48s %4d %8.2f \n", "", m, branch_p_flow[l,i,m]*bmva >> dcopf.stt;

for{(l,k,i) in BRANCH } 
printf "%48s %4d %8.2f \n", "", k, -branch_p_flow[l,k,i]*bmva >> dcopf.stt;
}
