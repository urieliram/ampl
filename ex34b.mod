# Ejercicio 34 inciso b

# VARIABLES:
# X1 : Cantidad de producto A (g)
# X2 : Cantidad de producto B (g)

# Z : Funcion de minimizar calorias 

# Modelo:
# Min = 4.5 X1 + 1.5 X2

# Sujeta a:
# (1) LIMITE DE CONTENIDO
# X1	+	X2	<	150
# -X1	-	X2	<	50
# -X1	+	X2	<	0
# X1			<	100


# X1	, 	X2	>	0

 set PRODUCTO;   # tipo de producto en la mezcla
 set RECURSOS;   # recursos       
 
 param B{PRODUCTO};           # Aporte del producto a la mezcla.
  
 param U{PRODUCTO,RECURSOS};  # Restricciones por producto.
 
 param T{RECURSOS};           # Restricciones. 
  
 var X {PRODUCTO} >= 0, integer;             # Variables del Problema. 
 
  # Funcion objetivo
 minimize Z:
 sum{i in PRODUCTO} B[i] * X[i];
 
subject to R{j in RECURSOS}:
sum{i in PRODUCTO} U[i,j] * X[i] <= T[j];
