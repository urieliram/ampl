
#Lagrangian heuristics based on disaggregated bundle methods for hydrothermal unit commitment
#Borghetti A, Frangioni A, Lacalandra F, Nucci CA
#IEEE TRANSACTIONS ON POWER SYSTEMS 18 (2): 974-974 MAY 2003
#http://people.brunel.ac.uk/~mastjjb/jeb/orlib/unitinfo.html

#Set of indexes of available thermal units ( |I| number of thermal units; i thermal unit index).
set I;

#Set of indexes of river basins ( |B|: number of river basins; b: basin unit index).
set B;

set H;  # Set of indexes of all available hydro units.
        # |H|  number of hydro units, 
        
set Hb; # Set of indexes of all available hydro units in river basin b  
        # |Hb| number of hydro units in river basin b ;
        # h hydro unit index.

set Hh; # Set of indexes of available upstream hydro units in river basin b directly above hydro unit 
        # h(|Hh|: number of hydro units; h: upstream hydro unit index).

set T;  #Set of time periods in the optimization horizon 
        #( |T|: number of time periods; t:time period index).
        
param D[T]; # |T|-dimensional vector of load demands Dt in each period t.

param R[T]; # |T|-dimensional vector of the required operating spinning reserves Rt in each period t.

var u[I,T]; # |I|-rows |T|-columns matrix, whose rows are the 
            # |T|-dimensional arrays ui of the 0–1 variables u[i,t]
            # indicating the commitment state of thermal unit i during period t.
           
var pI[I,T]; # |I|-rows |T|-columns matrix, whose rows are the |T|-dimensional arrays of
             # p[i] of production levels p[i,t] of thermal unit i during each period t.      
            
var pH[I,T]; # |H|-rows |T|-columns matrix, whose rows are the -dimensional arrays 
             # p[h] of production levels p[h,t] of hydro unit h during each period t.      
            
param pm[I]; # Minimum/maximum outputs of the units.    
param mx[I];                 
param pmh[H];      
            
param taou[I];   # Minimum up and downtimes of thermal units
param taod[I];       
            
param c[I,T];    # Per hour operating cost of committed thermal unit i at period t,
                 # as a quadratic function of production level p[i,t].
             
param r[I,T];    # Operating reserve contributions that can be supplied by the units during 
param r[H,T];    # period t as a function of their production levels.
            
param su[I,T] ;  # Start-up cost which is charged whenever thermal unit is committed at the beginning
                 # of period . It can depend on the number of periods that the unit has been down.            

param w[H,T];    # Water discharge rate, net inflow rate and spillage of reservoir 
param a[H,T];    # during period.
param s[H,T];              
       
param n[T] ;     # Length of period t. Summation is equal to the optimization horizon.      
             
param tao[H,H];  # Water transport delay from hydro unit to reservoir .          
             
param V[H,T];    # Storage volume of the reservoir of hydro unit h at the end of period t
                 # limited between a maximum VMX[H] and a minimum value VMN[H].      
param VMX[H];  
param VMN[H];     

param Vin[H];    # Storage volumes of reservoir at the beginning
param Vend[H];   # and at the end of the optimization horizon,
                 # as given by a long-term hydroscheduling.
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             