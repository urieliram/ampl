#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######
# Ejercicio numero 14
# Una industria vinícola produce vino y vinagre. El doble de la producción de vino es siempre
# menor o igual que la producción de vinagre más cuatro unidades. Por otra parte, el triple de la producción
# de vinagre sumado con cuatro veces la producción de vino se mantiene siempre menor o igual a 18
# unidades.
# Hallar el número de unidades de cada producto que se deben producir para alcanzar un beneficio máximo,
# sabiendo que cada unidad de vino deja un beneficio de 800 u.m. y cada unidad de vinagre de 200 u.m.

# INFORMACIÓN
# Dos veces la producción de vino es menor o igual que la producción de vinagre más cuatro unidades
# El triple de la producción de vinagre más cuatro (producción de vino) es siempre menor que 18 unidades

# BENEFICIOS
# VINO         800 u.m. por unidad
# VINAGRE      200 u.m. por unidad

set PRODUCTOS;
set BALANCES;

param beneficio  {PRODUCTOS};
param produccion {PRODUCTOS,BALANCES};
param total      {BALANCES};

 var  X{PRODUCTOS} >= 0; # Cantidad en botellas de vino y vinagre a producir
 
 maximize Z:
 sum{i in PRODUCTOS}  beneficio[i] * X[i];
 
 # Para todas las BALANCES en unidades volumetricas
 subject to balances {j in BALANCES}:
 sum{i in PRODUCTOS} produccion[i,j] * X[i] <= total[j];