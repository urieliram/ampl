#######  #######  #######  #######  #######  #######  #######  #######
#######  Multiperiod Coal Blending and Distribution Problem    #######
#######  Programador: Uriel Iram Lezama Lope				   #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				   #######
#######  (Omision Intencional de Acentos)    				   #######
#######  #######  #######  #######  #######  #######  #######  #######

#######  CONJUNTOS

#  Nombre de Conjunto  # {indice usado en la ecuacion} Descripcion del conjunto.

set MINAS;           # {i}   MINAS de carbón con almacenamiento M.
set SILOS;           # {j}   SILOS con o sin benefactor para reducir ceniza y sulfuro.
set CLIENTES;        # {k}   CLIENTES con demanda de requerimientos específicos de ceniza y sulfuro.
set T;               # {t}   Periodos de produccion t.

#set TAO;			 # {tao} period dissipation limit.
#set DELTA;          # {t}   Periodos de almacenamiento del.

set A1 within  {MINAS , SILOS};     #  A1 denote the permissible flow transfer arcs (i,j) from mine i to silo j.
set A2 within  {SILOS , CLIENTES};  #  A2 denote the permissible flow transfer arcs (j,k) from silo j to customer k.

#######  PARAMETROS 

param Tmax,  		integer, >=0 ;  # Maximo de tiempo del problema      
param TaoMax,  		integer, >=0;   # Maximo tiempo de entrega al cliente      
		          					# Maximum shipment lag permitted between the coal production at 
			                      	# Any mine and its ultimate shipment to customers through any silo

#param i   >=0,  	 integer ;      #  Indice de Minas de salida.
#param j   >=0,  	 integer ;      #  Indice de Minas de llegada.
#param t   >=Tmax,   integer ;      #  Indice de tiempo del proceso, Tmax.
#param tao >=TaoMax, integer ;   	#  Indice de tiempo de entrega a cliente, 1..TaoMax.
#param del >=Tmax,   integer ;   	#  Indice de tiempo de entrega a cliente, 1..TaoMax.

param p     { MINAS,T } >=0 ;       #  {i,t} Production (in tons of coal) at mine i during period t.
param a     { MINAS,T } >=0 ;       #  {i,t} Ash percentage content in the coal produced at mine / during period t.
param s     { MINAS,T } >=0 ;       #  {i,t} Sulfur percentage content in the coal produced at mine / during period t.
param cM    { MINAS   } >=0 ;       #  {i} Cost per ton at mine i in a period storage.
param M     { MINAS   } >=0 ;       #  {i} Capacity of the storage facility

param c1    { MINAS, SILOS } >=0;  	#  The transportation cost per ton from mine i to silo j

param S     { SILOS } >=0;         	#  storage capacity of silo j
param cS    { SILOS } >=0;         	#  per-ton storage cost of j

param q0    { SILOS } >=0;;			#  initial amount of qj tons of coal stored at silo j
param s0    { SILOS } >=0;		   	#  sulfur percentage content in t0
param a0    { SILOS } >=0;		   	#  ash percentage content in t0

param cB    { MINAS, SILOS } >=0;      #  cleaned cost of i to j per ton
param beta  { MINAS, SILOS } >=0 ,<=1;  #  ash content being respectively attenuated by a factor (0,1]
param gamma { MINAS, SILOS } >=0 ,<=1;  #  sulfur content being respectively attenuated by a factor (0,1]
param alfa  { MINAS, SILOS } >=0 ,<=1;  #  total weight being thereby attenuated by a factor (0,1]


param c2    { SILOS, CLIENTES } >=0;   #  The transportation cost per ton from silo j to customer k

param d      { CLIENTES, T };     	#  The demand placed (in tons of coal) by customer k during period t

param la     { CLIENTES };	  	#  lower limits ash percentage contents in tons of coal
param ua     { CLIENTES };     	#  upper limits ash percentage contents in tons of coal
param ls     { CLIENTES };      	#  lower limits sulfur percentage contents in tons of coal
param us     { CLIENTES };      	#  upper limits sulfur percentage contents in tons of coal
param r      { CLIENTES };      	#  per-ton per-percentage point that falls below the maximum specified percentage

#######  VARIABLES

var y { MINAS, SILOS, T, CLIENTES , T } >=0;# {i,j,k,t,tao} amount (tons) of coal shipped from mine i to silo j in period t, 
                    						# with continued shipment to customer k in period tao (where tao = t, t + 1, t + 2,
                       						# based on the three-period shipment lag restriction)
                    						
var y0 { SILOS, CLIENTES, T } >=0 ;	        # Amount (tons) of coal that is in initial storage at silo/, which is shipped to 
                    						# customer k in period (where tao = 1,2,3, based on a three period dissipation limit)	
                    						                    						
var xM { MINAS, T } >=0;					# slack variable that represents the amount (tons) of coal 
                    						# remaining in storage at mine i during period delta.                    						
var xS { SILOS, T } >=0;					# accumulated storage amount (tons) of coal in silo j during period delta;

var za { CLIENTES } >=0;					# percentage ash content in the blended coal that is ultimately delivered to customer k in period tao

var zs { CLIENTES } >=0;					# percentage sulfur content in the blended coal that is ultimately delivered to customer k in period tao
                    						

#######  FUNCION OBJETIVO #, t in T, tao in T, tao=t in min(t+2,T) #sum{tao t:=min(t+2,card(T))} 

minimize Costo_Total : #t..(min(t+2,card(T)))
sum{ j in SILOS} sum{ i in MINAS:(i,j) in A1} sum{k in CLIENTES:(j,k) in A2} sum{t in T} sum{tao in t..min(t+2,card(T))} 
 ( (c1[i,j] + cB[i,j] + c2[j,k] + (tao-t+1) * cS[j] ) * y[i,j,t,k,tao] ) + 
sum{ i in MINAS} sum{del in 1..card(T)} (cM[i] + xM[i,del] ) +
sum { j in SILOS } sum {k in CLIENTES:(j,k) in A2 } sum {tao in 1..3} (c2[j,k] * y0[j,k,tao]) +
sum { j in SILOS} sum {t in 1..3 } cS[j] * ( q0[j]  - sum { tao in T : tao<t} sum {k in CLIENTES:(j,k) in A2 } y0[j,k,tao] ) - 
sum { k in CLIENTES, tao in T } ( ( ua[k]-za[k]) * d[k,tao] * r[k] );

###################  RESTRICCIONES

##  Balance de almacenamiento en la mina i en el Periodo del
subject to Balance_Nodo {i in MINAS, del in T}: 
(sum { t in 1..del } p[i,t] - 
sum { j in SILOS:(i,j) in A1} sum {t in 1..del} sum {k in CLIENTES:(j,k) in A2} sum{tao in t..min((t+2),card(T))} y[i,j,t,k,tao] )  #SE RECORTA TAO A LA CARDINALIDAD DE T
= xM[i,del];

##  Limite superior de almacenamiento en la mina i en el periodo delta
subject to Almacenamiento_Nodo {i in MINAS, del in T}:
0 <= xM[i,del] <= M[i];

##  Balance de almacenamiento en el Silo j en el Periodo del
subject to Balance_Silo {j in SILOS, del in T}:
sum { i in MINAS:(i,j) in A1} sum { k in CLIENTES:(j,k) in A2} sum {t in max(1,del-2)..del} sum { tao in del..min((t+2),card(T)) } 
(alfa[i,j] * y[i,j,t,k,tao] )
+ ( q0[j] - sum{k in CLIENTES:(j,k) in A2 } sum { tao in 1..min(del-1,3)} y0[j,k,tao]) = xS[j,del] ;

##  Limite superior de almacenamiento en el Silo j en el Periodo del
subject to Almacenamiento_Silo {j in SILOS, del in T}:
0 <= xS[j,del] <= S[j];

##  Envio de la cantidad q0 almacenada inicial de los SILOS al CLIENTE j
subject to Desalojo0_Silo {j in SILOS}:
sum{ k in CLIENTES:(j,k) in A2 } sum { tao in 1..3 } y0[j,k,tao] = q0[j];

## Satisfacción de la demanda  d de los clientes k en el tiempo tao.
subject to Satisfacer_Demanda {k in CLIENTES, tao in T}:
sum{ j in SILOS:(j,k) in A2 } sum { i in MINAS:(i,j) in A1} sum { t in max(1,tao-2)..tao } alfa[i,j]*y[i,j,t,k,tao] +
sum{ j in SILOS:(j,k) in A2 } y0[j,k,tao] = d[k,tao];

## Balance de ceniza
subject to Nivel_Ceniza {k in CLIENTES, tao in T}:
za[k] * d[k,tao] = sum{ j in SILOS:(j,k) in A2 } sum {i in MINAS:(i,j) in A1} sum {t in max(1,tao-2)..tao} 
alfa[i,j]*beta[i,j]*y[i,j,t,k,tao] + 
sum{ j in SILOS:(j,k) in A2 } a0[j]*y0[j,k,tao];

## Estandares de nivel de ceniza alto
subject to Nivel_Ceniza_alto {k in CLIENTES}:
za[k] <= ua[k];

## Estandares de nivel de ceniza bajo
subject to Nivel_Ceniza_bajo {k in CLIENTES}:
za[k] >= la[k];

## Balance de sulfuro
subject to Nivel_sulfuro {k in CLIENTES, tao in T}:
zs[k] * d[k,tao] = sum{ j in SILOS:(j,k) in A2 } sum{i in MINAS:(i,j) in A1} sum{ t in max(1,tao-2)..tao} 
s[i,t]*gamma[i,j]*y[i,j,t,k,tao] + 
sum{j in SILOS:(j,k) in A2 } s0[j]*y0[j,k,tao];

## Estandares de nivel de sulfuro alto
subject to Nivel_sulfuro_alto {k in CLIENTES}:
zs[k] <= us[k];

## Estandares de nivel de sulfuro bajo
subject to Nivel_sulfuro_bajo {k in CLIENTES}:
zs[k] >= ls[k];

## Links utiles
## https://www.lix.polytechnique.fr/~liberti/teaching/isic/isc612-07/
## https://www.lix.polytechnique.fr/~liberti/teaching/isic/isc612-07/exercises-solutions.pdf