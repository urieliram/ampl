
######################################################################

######################################################################
#  SET, PARAMETER AND VARIABLE
######################################################################

#reset;

#---- DECLARE SPACE FOR UP TO 4000 BUSES. - opf_var
set BUS;                                          # all buses
set BRANCH within {1..5000} cross BUS cross BUS;  # only normal lines
set TRANS within {1..5000} cross BUS cross BUS;   # including transformers and phase shifters
set FACTS within BRANCH;   # different FACTS devices
set LOAD within {1..4000} cross BUS;              # one bus may have multiple loads
set TGEN within {1..4000} cross BUS;              # thermal generator
set WGEN within {1..4000} cross BUS;              # wind generator
set SWSHUNT within {1..4000} cross BUS;
set IDCG;
set IDWG;

set SCENARIO;
set TIME ordered;

set CHG_CTRLS_GP;
set CHG_CTRLS_GV;

set  LOC_CHG_GP;
set  LOC_CHG_GV;
set  LOC_CHG_GTAP;
set  LOC_CHG_GSHUNT;
set  CRITICALBRANCHCONS;
set  CRITICALTRANSCONS;

set VCBRANCH within BRANCH union TRANS;           # critical branches in terms of thermal violation

#---- DEFINE A SET FOR THE BUS ADMITTANCE MATRIX (YBUS).
set YBUS := setof{i in BUS} (i,i) union 
              setof {(l,k,m) in BRANCH} (k,m) union
                setof {(l,k,m) in BRANCH} (m,k) union
                  setof {(l,k,m) in TRANS}  (k,m) union
                    setof {(l,k,m) in TRANS}  (m,k);

					
#---- DEFINE A SET FOR THE JACOBIAN MATRIX (POWER FLOW).
set JID := setof{i in BUS} (i,i) union 
              setof {(l,k,m) in BRANCH} (k,m) union
                setof {(l,k,m) in BRANCH} (m,k) union
                  setof {(l,k,m) in TRANS}  (k,m) union
                    setof {(l,k,m) in TRANS}  (m,k);

set BRANCHTRANS = BRANCH union TRANS;
					
#---- DECLARE PARAMETERS
param basemva;                      # in MVA

param Linef;          #the from - to of line
param Linet;

param STATUS_CHG_GP             {TGEN};
param STATUS_CHG_GP_RANK        {TGEN};
param STATUS_CHG_GV             {TGEN};
param STATUS_CHG_GV_RANK        {TGEN};

param STATUS_CHG_TAP            {TRANS};
param STATUS_CHG_TAP_RANK       {TRANS};

param STATUS_CHG_SHUNT          {SWSHUNT}; 
param STATUS_CHG_SHUNT_RANK     {SWSHUNT};  
#---- Operational Parameters
#param bus_name          {BUS} symbolic;
param bus_type          {BUS};      # 3-slack, 1-load, 2-generator, 4-isolated
param bus_pd            {BUS};
param bus_qd            {BUS};
param bus_shunt_g       {BUS};      # real component of shunt admittance to ground, in MW at unity voltage
param bus_shunt_b       {BUS};      # reactive component of shunt admittance to ground, in MVAR at unity voltage
param bus_area          {BUS};
param bus_vm0           {BUS};
param bus_va0           {BUS};      # in degree
param bus_basekv        {BUS};
param bus_zone          {BUS};
param bus_vmax          {BUS};
param bus_vmin          {BUS};

param branch_r          {BRANCH};   # branch resistance
param branch_x          {BRANCH};   # branch reactance
param branch_c          {BRANCH};   # total branch charging susceptance, p.u.
param branch_rate1      {BRANCH};   # 1st MVA rating
param branch_rate2      {BRANCH};   # 2nd MVA rating
param branch_rate3      {BRANCH};   # 3rd MVA rating
param branch_nontap     {BRANCH};   # 0-disconnected, 1-connected
param branch_nonshift    {BRANCH};
param branch_status     {BRANCH};   # 0-disconnected, 1-connected
param branch_angle0     {BRANCH};
param branch_angle2     {BRANCH};

param trans_r          {TRANS};   # branch resistance
param trans_x          {TRANS};   # branch reactance
param trans_c          {TRANS};   # total branch charging susceptance, p.u.
param trans_rate1      {TRANS};   # 1st MVA rating
param trans_rate2      {TRANS};   # 2nd MVA rating
param trans_rate3      {TRANS};   # 3rd MVA rating
param trans_tap0       {TRANS};   # 0-disconnected, 1-connected
param trans_shift0     {TRANS};
param trans_status     {TRANS};   # 0-disconnected, 1-connected
param trans_angle0     {TRANS};
param trans_angle2     {TRANS};
param trans_tap_min    {TRANS};
param trans_tap_max    {TRANS};
param trans_tap_step   {TRANS};
param trans_shift_min  {TRANS};
param trans_shift_max  {TRANS};

#-- TGEN -- Single period: default settings; Multi-period: settings at the hour 0
param  tgen_p0			{TGEN};
param	tgen_q0			{TGEN};
param	tgen_q_max		{TGEN};
param	tgen_q_min		{TGEN};
param	tgen_v0			{TGEN};
param	tgen_base		{TGEN};
param	tgen_status		{TGEN};
param	tgen_p_max		{TGEN};
param	tgen_p_min		{TGEN};
param	tgen_Pc1		{TGEN};
param	tgen_Pc2		{TGEN};
param	tgen_Qc1min		{TGEN};
param	tgen_Qc1max		{TGEN};
param	tgen_Qc2min		{TGEN};
param	tgen_Qc2max		{TGEN};
param	tgen_ramp_agc	{TGEN};
param	tgen_ramp_10	{TGEN};
param	tgen_ramp_30	{TGEN};
param	tgen_ramp_q		{TGEN};
param	tgen_apf		{TGEN};
param	tgen_fuel_a		{TGEN};
param	tgen_fuel_b		{TGEN};
param	tgen_fuel_c		{TGEN};

param shunt_type        {SWSHUNT};  # 0-fixed, 1-discreat + local, 2-continuous + local
param shunt_b0			{SWSHUNT};  # in MVAR at unity voltage
param shunt_b_min		{SWSHUNT};  # for continuous adjustment
param shunt_b_max		{SWSHUNT};
param shunt_step		{SWSHUNT};	# number of steps for block 1

param facts_type        {FACTS};    # 0-TCSC
param facts_status      {FACTS};    # 0-out-of-service, 1-in-service
param facts_min         {FACTS};
param facts_max         {FACTS};

#---- Branch Parameters
param branch_G {(l,k,m) in BRANCH}
    :=  branch_r[l,k,m] / (branch_r[l,k,m]^2 + branch_x[l,k,m]^2);
param branch_B {(l,k,m) in BRANCH}
    := -branch_x[l,k,m] / (branch_r[l,k,m]^2 + branch_x[l,k,m]^2);
param trans_G {(l,k,m) in TRANS}
    :=  trans_r[l,k,m] / (trans_r[l,k,m]^2 + trans_x[l,k,m]^2);
param trans_B {(l,k,m) in TRANS}
    := -trans_x[l,k,m] / (trans_r[l,k,m]^2 + trans_x[l,k,m]^2);  

#---- DECLARE VARIABLES, WITH UPPER AND LOWER BOUNDS.
#---- Base Case
var bus_vm  {i in BUS} 			>= bus_vmin[i],
                       			<= bus_vmax[i];            
var bus_va  {i in BUS} >= -3,
                        <= 3;
var shunt_b {(sw,i) in SWSHUNT} >= shunt_b_min[sw,i],
                                <= shunt_b_max[sw,i];
var trans_tap  {(l,k,m) in TRANS} >= trans_tap_min[l,k,m],
                                  <= trans_tap_max[l,k,m]; 
var trans_shi  {(l,k,m) in TRANS} >= trans_shift_min[l,k,m],
                                  <= trans_shift_max[l,k,m]; 
                                   
var tgen_q   {(g,i) in TGEN}	>= tgen_q_min[g,i],
                             	<= tgen_q_max[g,i];
var tgen_p   {(g,i) in TGEN} 	>= tgen_p_min[g,i],
                            	<= tgen_p_max[g,i];
								
var tgen_state {(g,i) in TGEN}  binary;            # 1 if unit is on

var tgen_statet {(g,i) in TGEN, t in TIME}  binary;            # 1 if unit is on
								

var sipg {(g,i) in TGEN} binary ;
var sipv {(g,i) in TGEN} binary ; 
var sitap {b in LOC_CHG_GTAP} binary ; 


##----------- cone variable  ------------------

var cii{ i in BUS} >= 0.81,#bus_vmin[i]*bus_vmin[i],
                   <= 1.21;#bus_vmax[i]*bus_vmax[i];
var cij{(l,k,m) in BRANCHTRANS} >= 0.7,
                            <= 1.1;
var sij{(l,k,m) in BRANCHTRANS} >= -0.7,
                           <= 0.71;
					   

							 
#---- COMPUTE ADMITTANCE VALUES FOR BASE CASE
var G{(k,m) in YBUS} =
  if k = m then
    (bus_shunt_g[k]
     + sum{(l,k,i) in BRANCH} branch_status[l,k,i] * branch_G[l,k,i]
     + sum{(l,i,k) in BRANCH} branch_status[l,i,k] * branch_G[l,i,k]
     + sum{(l,k,i) in TRANS} trans_status[l,k,i] * trans_G[l,k,i] / trans_tap[l,k,i]^2
     + sum{(l,i,k) in TRANS} trans_status[l,i,k] * trans_G[l,i,k])
  else if k != m then
  /*
    (sum{(l,k,m) in BRANCH} branch_status[l,k,m] * (- branch_G[l,k,m])
     + sum{(l,m,k) in BRANCH} branch_status[l,m,k] * (- branch_G[l,m,k])
     + sum{(l,k,m) in TRANS} trans_status[l,k,m] * (- trans_G[l,k,m] * cos(trans_shi[l,k,m])
                              + trans_B[l,k,m] * sin(trans_shi[l,k,m])) / trans_tap[l,k,m]
     + sum{(l,m,k) in TRANS} trans_status[l,m,k] *(- trans_G[l,m,k] * cos(trans_shi[l,m,k])
                              - trans_B[l,m,k] * sin(trans_shi[l,m,k])) / trans_tap[l,m,k]);
*/
							
	(sum{(l,k,m) in BRANCH} branch_status[l,k,m] * (- branch_G[l,k,m])
     + sum{(l,m,k) in BRANCH} branch_status[l,m,k] * (- branch_G[l,m,k])
     + sum{(l,k,m) in TRANS} trans_status[l,k,m] * (- trans_G[l,k,m] * cos(0)
                              + trans_B[l,k,m] * sin(0)) / trans_tap[l,k,m]
     + sum{(l,m,k) in TRANS} trans_status[l,m,k] *(- trans_G[l,m,k] * cos(0)
                              - trans_B[l,m,k] * sin(0)) / trans_tap[l,m,k]);

#var transa{(l,k,m) in TRANS} = 
#     cos(0);							  
							  
var B{(k,m) in YBUS} =
  if(k == m) then
    (sum{(sw,k) in SWSHUNT} shunt_b[sw,k] + bus_shunt_b[k]
	#(sum{(sw,k) in SWSHUNT} shunt_b[sw,k]
     + sum{(l,k,i) in BRANCH} branch_status[l,k,i] * ( branch_B[l,k,i] + branch_c[l,k,i]/2)
     + sum{(l,i,k) in BRANCH} branch_status[l,i,k] * ( branch_B[l,i,k] + branch_c[l,i,k]/2)
     + sum{(l,k,i) in TRANS} trans_status[l,k,i] * ( trans_B[l,k,i] / trans_tap[l,k,i]^2 + trans_c[l,k,i]/2 )
     + sum{(l,i,k) in TRANS} trans_status[l,i,k] * ( trans_B[l,i,k] + trans_c[l,i,k]/2 ))
  else if(k != m) then
  /*
    (sum{(l,k,m) in BRANCH} branch_status[l,k,m] * (- branch_B[l,k,m])
     + sum{(l,m,k) in BRANCH} branch_status[l,m,k] * (- branch_B[l,m,k])+
      sum{(l,k,m) in TRANS} trans_status[l,k,m] * ( - trans_G[l,k,m] * sin(trans_shi[l,k,m])
                              - trans_B[l,k,m] * cos(trans_shi[l,k,m])) / trans_tap[l,k,m]+
     sum{(l,m,k) in TRANS} trans_status[l,m,k] *( trans_G[l,m,k] * sin(trans_shi[l,m,k])
                              - trans_B[l,m,k] * cos(trans_shi[l,m,k])) / trans_tap[l,m,k]);
*/				
    (sum{(l,k,m) in BRANCH} branch_status[l,k,m] * (- branch_B[l,k,m])
     + sum{(l,m,k) in BRANCH} branch_status[l,m,k] * (- branch_B[l,m,k])+
     sum{(l,k,m) in TRANS} trans_status[l,k,m] * ( 
                              - trans_B[l,k,m] ) / trans_tap[l,k,m]+
     sum{(l,m,k) in TRANS} trans_status[l,m,k] *(
                              - trans_B[l,m,k] * 1) / trans_tap[l,m,k]);
							  
#---- COMPUTE JACOBIAN MATRIX FOR POWER FLOW
var JPA{(i,j) in JID} = 
   if i!=j then
		-bus_vm[i]*bus_vm[j]*(G[i,j]*sin(bus_va[i]-bus_va[j]) - B[i,j]*cos(bus_va[i]-bus_va[j]))
   else if i==j then
        sum{(i,k) in JID} bus_vm[i]*bus_vm[k]*(G[i,k]*sin(bus_va[i]-bus_va[k]) - B[i,k]*cos(bus_va[i]-bus_va[k]));
		
var JPU{(i,j) in JID} = 
   if i!=j then
		-bus_vm[i]*(G[i,j]*cos(bus_va[i]-bus_va[j]) + B[i,j]*sin(bus_va[i]-bus_va[j]))
   else if i==j then
        sum{(i,k) in JID} bus_vm[i]*(G[i,k]*sin(bus_va[i]-bus_va[k]) - B[i,k]*cos(bus_va[i]-bus_va[k]))
		+ 2*bus_vm[i]*G[i,j];
		
var JQA{(i,j) in JID} = 
   if i!=j then
		 bus_vm[i]*bus_vm[j]*(G[i,j]*cos(bus_va[i]-bus_va[j]) + B[i,j]*sin(bus_va[i]-bus_va[j]))
   else if i==j then
        sum{(i,k) in JID} -bus_vm[i]*bus_vm[k]*(G[i,k]*cos(bus_va[i]-bus_va[k]) + B[i,k]*sin(bus_va[i]-bus_va[k]));
		
var JQU{(i,j) in JID} = 
   if i!=j then
		-bus_vm[i]*(G[i,j]*sin(bus_va[i]-bus_va[j]) - B[i,j]*cos(bus_va[i]-bus_va[j]))
   else if i==j then
        sum{(i,k) in JID} bus_vm[i]*(G[i,k]*sin(bus_va[i]-bus_va[k]) - B[i,k]*cos(bus_va[i]-bus_va[k]))
		+ 2*bus_vm[i]*B[i,j]; 
							  
# var cosv{(k,m) in YBUS} =
#      sum{(l,k,m) in TRANS } sin(trans_shi[l,k,m])+1;
#---- base case branch flow
var branch_p_f {(l,k,m) in BRANCH: branch_rate1[l,k,m] <= 400}= # final active direct flow (k->m)
  branch_G[l,k,m] * bus_vm[k]^2 
  - branch_G[l,k,m] * bus_vm[k] * bus_vm[m] 
    * cos(bus_va[k] - bus_va[m])
  - branch_B[l,k,m] * bus_vm[k] * bus_vm[m] 
    * sin(bus_va[k] - bus_va[m]);
    
var branch_q_f {(l,k,m) in BRANCH: branch_rate1[l,k,m] <= 400}= # final reactive direct flow
  - (branch_B[l,k,m] + branch_c[l,k,m]/2) * bus_vm[k]^2 
  - branch_G[l,k,m] * bus_vm[k] * bus_vm[m]
    * sin(bus_va[k] - bus_va[m])
  + branch_B[l,k,m] * bus_vm[k] * bus_vm[m]
    * cos(bus_va[k] - bus_va[m]);

var branch_p_t {(l,k,m) in BRANCH: branch_rate1[l,k,m] <= 400}= # final active direct flow (k->m)
  branch_G[l,k,m] * bus_vm[m]^2 
  - branch_G[l,k,m] * bus_vm[m] * bus_vm[k] 
    * cos(bus_va[m] - bus_va[k])
  - branch_B[l,k,m] * bus_vm[m] * bus_vm[k] 
    * sin(bus_va[m] - bus_va[k]);
    
var branch_q_t {(l,k,m) in BRANCH: branch_rate1[l,k,m] <= 400}= # final reactive direct flow
  - (branch_B[l,k,m] + branch_c[l,k,m]/2) * bus_vm[m]^2 
  - branch_G[l,k,m] * bus_vm[m] * bus_vm[k]
    * sin(bus_va[m] - bus_va[k])
  + branch_B[l,k,m] * bus_vm[m] * bus_vm[k]
    * cos(bus_va[m] - bus_va[k]);

var branch_p_f_cone {(l,k,m) in BRANCHTRANS} = 
	cii[k]*G[k,m]-(G[k,m]*cij[l,k,m]-B[k,m]*sij[l,k,m]);
	
	
    
var trans_p {(l,k,m) in TRANS: trans_rate1[l,k,m] <= 400}= # final active direct flow
  trans_G[l,k,m] * bus_vm[k]^2 / trans_tap[l,k,m]^2
  - trans_G[l,k,m] * bus_vm[k] * bus_vm[m] / trans_tap[l,k,m]
    * cos(bus_va[k] - bus_va[m] - trans_shi[l,k,m])
  - trans_B[l,k,m] * bus_vm[k] * bus_vm[m] / trans_tap[l,k,m]
    * sin(bus_va[k] - bus_va[m] - trans_shi[l,k,m]);
    
var trans_q {(l,k,m) in TRANS: trans_rate1[l,k,m] <= 400}= # final reactive direct flow
  - (trans_B[l,k,m] + trans_c[l,k,m]/2) * bus_vm[k]^2 / trans_tap[l,k,m]^2
  - trans_G[l,k,m] * bus_vm[k] * bus_vm[m] / trans_tap[l,k,m]
    * sin(bus_va[k] - bus_va[m] + trans_shi[l,k,m])
  + trans_B[l,k,m] * bus_vm[k] * bus_vm[m] / trans_tap[l,k,m]
    * cos(bus_va[k] - bus_va[m] + trans_shi[l,k,m]);

var IJPinj{k in BUS} = 
    sum{(l,k,m) in BRANCHTRANS} (G[k,m]*cij[l,k,m] - B[k,m]*sij[l,k,m]) 
	+ sum{(l,n,k) in BRANCHTRANS} (G[n,k]*cij[l,n,k] - B[n,k]*(-sij[l,n,k]));						  

var IJQinj{k in BUS} = 
	sum{(l,k,m) in BRANCHTRANS} (-B[k,m]*cij[l,k,m] - G[k,m]*sij[l,k,m])
	+ sum{(l,n,k) in BRANCHTRANS} (-B[n,k]*cij[l,n,k] - G[n,k]*(-sij[l,n,k]));
	

	
#---- COMPUTE OTHER NECESSARY VARIABLES
#---- fuel cost of thermal generators
var fuel_cost{(g,i) in TGEN} =
    tgen_fuel_a[g,i]*(tgen_p[g,i]*100)^2+tgen_fuel_b[g,i]*(tgen_p[g,i]*100)+tgen_fuel_c[g,i]*tgen_state[g,i];

var pinjtion {k in BUS} =
    G[k,k]*cii[k] + IJPinj[k];

var qinjtion {k in BUS} =
    -B[k,k]*cii[k] + IJQinj[k];
######################################################################
#  CONSTRAINTS
######################################################################
#---- POWER BALANCE FOR ENTIRE SYSTEM (LOSS IGNORED)

subject to totalp_balance :
	sum{(u,i) in TGEN} tgen_p[u,i] = sum{ b in BUS} bus_pd[b] ;
	
 #subject to totalp_balancet {t in TIME}:
#	sum{(u,i) in TGEN} tgen_pt[u,i,t] -	sum{(l,i) in LOAD} load_fore_pt[l,i,t] = 0;

subject to p_balance_cone {k in BUS}:
    sum{(g,k) in TGEN} tgen_p[g,k] - bus_pd[k] = G[k,k]*cii[k] + IJPinj[k];

subject to q_balance_cone {k in BUS}:
    sum{(g,k) in TGEN} tgen_q[g,k] - bus_qd[k] = -B[k,k]*cii[k] + IJQinj[k];	
	
subject to sijineqsij{(l,k,m) in BRANCHTRANS}:
    cij[l,k,m]*cij[l,k,m] + sij[l,k,m]*sij[l,k,m] <= cii[k]*cii[m];	


#---- AC POWER FLOW BALANCE EQUATIONS (NONLINEAR CONSTRAINTS).
# Pre-contingency 
subject to p_balance {k in BUS}:
  sum{(g,k) in TGEN} tgen_p[g,k] 
  - bus_pd[k]
  - sum{(k,m) in YBUS} (bus_vm[k] * bus_vm[m]
                        * (  G[k,m] * cos(bus_va[k] - bus_va[m])
                           + B[k,m] * sin(bus_va[k] - bus_va[m]))) = 0;
  
subject to q_balance {k in BUS}:
  sum{(g,k) in TGEN} tgen_q[g,k] 
  - bus_qd[k]
  - sum{(k,m) in YBUS} (bus_vm[k] * bus_vm[m]
                        * (  G[k,m] * sin(bus_va[k] - bus_va[m])
                           - B[k,m] * cos(bus_va[k] - bus_va[m]))) = 0;

		 
######################################################################
# OBJECTIVE FUNCTIONS
######################################################################
#minimize fuel cost
minimize Total_fuel_cost :
      sum{(g,i) in TGEN} fuel_cost[g,i];

minimize Minimum_controls :
      sum{(g,i) in TGEN} sipg[g,i]+sum{(g,i) in TGEN} sipv[g,i];
  
maximize MaxlinePower :
      sum{(l,k,m) in BRANCHTRANS: k==Linef and m==Linet} sqrt(branch_p_f[l,k,m]^2);
	  
maximize MaxlinePowerCone :
      sum{(l,k,m) in BRANCHTRANS: k==Linef and m==Linet} sqrt(branch_p_f_cone[l,k,m]^2);   
	  
	  
	  
problem ConeOPF: tgen_p, tgen_q, cii,cij,sij,
        p_balance_cone, q_balance_cone,
		sijineqsij,
        Total_fuel_cost;	  
	  
problem ACOPF:tgen_p,tgen_q,bus_vm, bus_va,shunt_b, trans_tap, trans_shi,                             # variables
	    p_balance, q_balance,                                            # constraints: one of the two constraints is choosen 
	    #MaxlinePower;
        Total_fuel_cost;	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  