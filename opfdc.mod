# CONJUNTOS
set U ;              # Las unidades generadoras que suministran potencia a los nodos conectados a los nodos i.
set V ;              # Los nodos eléctricos o buses, físicamente son subestaciones eléctricas.
set E  within {V,V}; # Las ramas eléctricas o líneas de transmisión que se conectan entre los nodos i,j y transmiten energía en MW:
set UV within {U,V}; # Conjuto de unidades u asociadas a nodos i 

# PARAMETROS

param gmax   {U};    # La potencia máxima del generador u en MW 
param gmin   {U};    # La potencia mínima del generador u en MW
param c      {U};    # Costo de la unidad generadora

param d{V};          # La demanda en el nodo i en MW
param B{V,V};        # La susceptancia de la rama E en (1/ohms) 
param fpmax{V,V};    # La capacidad de flujo máximo de la rama E en MW

# VARIABLES
var x   {U}   >= 0;  # Generación de la unidad u en MW
var y   {V,V}     ;  # Flujo en la rama E en MW
var del {V};         # Ángulo de voltaje en el nodo i en rad

# FUNCION OBJETIVO
minimize Z : sum {u in U} c[u] * x[u] ;

# Restricciones de Operacion
subject to Balance_en_los_nodos {i in V}: 
+ sum {u in U : (u,i) in UV} x[u]   = d[i] + sum{j in V:(i,j) in E}( y[i,j]) ;

subject to Calculo_de_flujo_en_ramas {i in V, j in V : (i,j) in E}:
y[i,j] = B[ i , j ] * ( del[i] - del[j] );

subject to Limite_de_flujo_en_ramas {i in V, j in V : (i,j) in E}:
y[i,j] <= fpmax[ i , j ];

subject to Limite_operativo_de_los_generadores_min {u in U}: 
x[u] >= gmin[u]  ;

subject to Limite_operativo_de_los_generadores_max {u in U}: 
x[u] <= gmax[u];

subject to NodoSlack: 
del[3] = 0;