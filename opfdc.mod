# CONJUNTOS
set U ;              # Las unidades generadoras que suministran potencia a los nodos conectados a los nodos i.
set V ;              # Los nodos el�ctricos o buses, f�sicamente son subestaciones el�ctricas.
set E  within {V,V}; # Las ramas el�ctricas o l�neas de transmisi�n que se conectan entre los nodos i,j y transmiten energ�a en MW:
set UV within {U,V}; # Conjuto de unidades u asociadas a nodos i 

# PARAMETROS

param gmax   {U};    # La potencia m�xima del generador u en MW 
param gmin   {U};    # La potencia m�nima del generador u en MW
param c      {U};    # Costo de la unidad generadora

param d{V};          # La demanda en el nodo i en MW
param B{V,V};        # La susceptancia de la rama E en (1/ohms) 
param fpmax{V,V};    # La capacidad de flujo m�ximo de la rama E en MW

# VARIABLES
var x   {U}   >= 0;  # Generaci�n de la unidad u en MW
var y   {V,V}     ;  # Flujo en la rama E en MW
var del {V};         # �ngulo de voltaje en el nodo i en rad

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