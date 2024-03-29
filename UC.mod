#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com    #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				                            #######
#######  (Modelo obtenido de Anjos y Conejo 2017   				                     #######
#######  #######  #######  #######  #######  #######  #######    #######

###########  SETS

  set T ;    # Periods
  set J ;    # Generation units
  set C ;    # Linear pieces in the cost function of unit j 

###########  PARAMETERS

  param Pmax {J} ; 	     ## The maximum power output unit j 
  param Pmin {J} ; 	     ## The minimum power output unit j
  param c    {J} ;       ## Cost of producing power generation j
  param cU   {J} ;       ## The cost of starting up unit j in period t
  param De   {T} ;       ## The hourly load forecast 
  param R    {T} ;       ## The spinning reserve requirements

  param alfa {J,C} ;     ## Fixed coefficients of cost function of unit j 
  param beta {J,C} ;     ## Fixed coefficients of cost function of unit j 
  param Ru   {J}   ;     ## Maximum ramp-up rate of unit j
  param RD   {J}   ;     ## Maximum ramp-down rate of unit j
  param RU   {J}   ;     ## Maximum ramp-up rate of unit j
  param SU   {J}   ;     ## Maximum startup rate of unit j   
  param SD   {J}   ;     ## Maximum shutdown rate of unit j
  param U    {J}   ;     ## Number of time periods that unit j is required to be on
  param D    {J}   ;     ## Number of time periods that unit j is required to be off
  param TU   {J}   ;     ## Number of time periods that j has to run for at least before it can be shut down.
  param TD   {J}   ;     ## Number of time periods that j has to off for at least before it can be start up.
    
  param v0   {J}   ;     ## Condiciones iniciales de operacion
  param p0   {J}   ;     ## Condiciones iniciales de generacion

###########  VARIABLES

 var p  {J,T} >= 0; 		## the quantity of electricity generated by unit j at time t
 var pb {J,T} >= 0; 		## the maximum available power at time t from unit j
 
 var v  {J,T} >= 0, binary; ## binary equals 1 if unit j is on in time period t, and 0 if it is off
 var y  {J,T} >= 0, binary; ## binary equals 1 if unit j starts up at the beginning of time period t, and 0 otherwise
 var z  {J,T} >= 0, binary; ## binary equals 1 if unit j shuts down at the beginning of time period t, and 0 otherwise
 
###########  OBJETIVE FUNCTION

  minimize Z:
  sum {t in T}sum{j in J}( c[ j ] * p[ j , t ] + cU[ j ] * y[ j , t ] ) ;
 
###########  S.T.
 
# Demand
  subject to Const_Demand{ t in T }: 
  sum { j in J } p[ j , t ] = De[ t ];

# Spinning Reserve + Demand
  subject to Const_Reserve{ t in T }: 
  sum { j in J } pb[ j , t ] >= De[ t ] + R[ t ];
  
# Piecewise cost
 # subject to Const_Piecewise{ j in J , s in C, t in T} : 
 # c[ j ] * p[ j , t ] >=  alfa[ j , s ] * p[ j , t ] + beta[ j , s ];
 
 # Unit states 0
  subject to Const_Unitstates0{ j in J , t in T: t=1} : 
  v0[ j ] - v[ j , t ] + y[ j , t ] - z[ j, t ] = 0;
 
# Unit states
  subject to Const_Unitstates{ j in J , t in T: t>1} : 
  v[ j , t - 1 ] - v[ j , t ] + y[ j , t ] - z[ j, t ] = 0;

# Ramping up 0
  subject to Const_RampingUp0{ j in J , t in T: t=1} : 
  p[ j , t ] - p0[ j ] <=  RU[ j ] * v0[ j ] + SU[ j ] * y[j , t];
  
# Ramping up 
  subject to Const_RampingUp{ j in J , t in T: t>1} : 
  p[ j , t ] - p[ j , t - 1 ] <=  RU[ j ] * v[ j , t - 1 ] + SU[ j ] * y[j , t];

# Ramping down 0
  subject to Const_RampingDown0{ j in J , t in T: t=1} : 
  p0[ j ] - p[ j , t ] <=  RD[ j ] * v[ j , t ] + SD[ j ] * z[j , t];

# Ramping down
  subject to Const_RampingDown{ j in J , t in T: t>1} : 
  p[ j , t - 1 ] - p[ j , t ] <=  RD[ j ] * v[ j , t ] + SD[ j ] * z[j , t];



# Uptime    * * * * * * * * * * * *
  subject to Const_Uptime{ j in J , t in (min(card(T),U[j])+1)..(card(T)) } : 
  sum{ k in ( t-TU[j]+1)..t:k>=1 } y[ j , k ] <=  v[ j , t ];  
  
# Downtime  * * * * * * * * * * * *
  subject to Const_Downtime{j in J , t in (min(card(T),D[j])+1)..(card(T)) } : 
  v[ j , t ] + sum{ k in ( t-TD[j]+1)..t:k>=1 } z[ j , k ] <= 1 ;

# Generation Limits
  subject to Const_GenerationLimits{ j in J , t in T} : 
  Pmin[ j ] * v[ j , t ] <= p[ j , t ];
  
# Generation Limits 2
  subject to Const_GenerationLimits2{ j in J , t in T} : 
  p[ j , t ] <= pb[ j , t ] ;  
  
# Generation Limits 3
  subject to Const_GenerationLimits3{ j in J , t in T} : 
  pb[ j , t ] <= Pmax[ j ] * v[ j , t ];
    
# Generation Limits 4_0 +++++++
  subject to Const_GenerationLimits4_0{ j in J , t in T:t=1} : 
  pb[ j , t ] <= p0[ j ] + RU[ j ] * v0[ j ] + SU[ j ] * y[ j , t ];
    
# Generation Limits 4
  subject to Const_GenerationLimits4{ j in J , t in T:t>1} : 
  pb[ j , t ] <= p[ j , t - 1 ] + RU[ j ] * v[ j , t - 1 ] + SU[ j ] * y[ j , t ];
     
# Generation Limits 5
  subject to Const_GenerationLimits5{ j in J , t in T:(t<(card(T)-1))} : 
  pb[ j , t ] <= Pmax[ j ] * ( v[j,t] - z[j,t+1] ) + z[j,t+1] * SD[ j ];