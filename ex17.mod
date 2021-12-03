#######  #######  #######  #######  #######  #######  #######    #######
#######  Programador: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				     #######
#######  (Omision Intencional de Acentos)    				     #######
#######  #######  #######  #######  #######  #######  #######    #######

## Una persona tiene 500000 u.m. para invertir en dos tipos de acciones A y B. El tipo A tiene
## bastante riesgo con un interés anual del 10% y el tipo B es bastante seguro con un interés anual del
## 7%. Decide invertir como máximo 300000 u.m. en A y como mínimo 100000 u.m. enB, e invertir en A
## por lo menos tanto como en B, ¿Cómo deberá invertir sus 500000 u.m. para maximizar sus intereses
## anuales?
## ACCIONES INTERES
## ANUAL
## CONDICIONES DE INVERSION
## A 10% No más de 300000 u.m. Por lo menos tanto como en B
## B 7% No menos de 100000 u.m.
  
 set ACCIONES ;                 # A1, A2 
 set RECURSOS  ;                # 
 
 param A {ACCIONES,RECURSOS};  # Cantidad de recurso necesario para el producto
 
 param B {RECURSOS};            # Disponibilidad del Recurso 
 
 param C {ACCIONES};           # Beneficio por producto
 
 var   X {ACCIONES} >= 0, integer;
 
 maximize Z:
 sum{i in ACCIONES} C[i] * X[i];
 
 #para todo RECURSO
 
 subject to Recursos {j in RECURSOS}:
 sum{i in ACCIONES} A[i,j] * X[i] <= B[j];