# CONJUNTOS

set J ;              # Las unidades generadoras que suministran potencia a los nodos conectados a los nodos i.
set N ;              # Los nodos eléctricos o buses, físicamente son subestaciones eléctricas.
set E  within {N,N}; # Las ramas eléctricas o líneas de transmisión que se conectan entre los nodos i,j y transmiten energía en MW:
set JN within {J,N}; # Conjunto de unidades u asociadas a nodos i 

# PARAMETROS

param Pmax   {J};    # La potencia máxima del generador j en MW 
param Pmin   {J};    # La potencia mínima del generador j en MW
param c      {J};    # Costo de la unidad generadora j

param De{N};         # La demanda en el nodo i en MW
param B {N,N};       # La susceptancia de la rama E en (1/ohms) 
param Y {N,N};       # La capacidad de flujo máximo de la rama E en MW

# VARIABLES

var p   {J}   >= 0;  # Generación de la unidad u en MW
var y   {N,N}     ;  # Flujo en la rama y en MW
var del {N};         # Angulo de voltaje en el nodo N en rad

# FUNCION OBJETIVO
minimize Z : sum {j in J} c[j] * p[j] ;

# Restricciones de Operacion
subject to Balance_en_los_nodos {n in N}: 
sum {j in J : (j,n) in JN} p[j] = De[n] + sum{m in N:(n,m) in E}(y[n,m]) ;

subject to Calculo_de_flujo_en_ramas {n in N, m in N : (n,m) in E}:
y[n,m] = B[ n , m ] * ( del[n] - del[m] );

subject to Limite_de_flujo_en_ramas {n in N, m in N : (n,m) in E}:
y[n,m] <= Y[ n , m ];

subject to Limite_operativo_de_los_generadores_min {j in J}: 
p[j] >= Pmin[j]  ;

subject to Limite_operativo_de_los_generadores_max {j in J}: 
p[j] <= Pmax[j];

subject to NodoSlack: 
del[3] = 0;