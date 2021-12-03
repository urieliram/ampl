#######  #######  #######  #######  #######  #######  #######      #######
#######  Programadores: Uriel Iram Lezama Lope urieliram@yahoo.com #######
#######  Programadores: Julio César  #######
#######  Programadores: Gabriela  Renata   #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				       #######
#######  #######  #######  #######  #######  #######  #######      #######

###########  CONJUNTOS

  set I ;    # INVERSIONES

###########  PARAMETROS

  param M ; 	      ## Capital de inversión máximo
  param C {I} ; 	  ## Costo de la inversión i
  param R {I} ;       ## Rédito de la inversión i
    
###########  VARIABLES

 var x{I} >= 0, binary; ## "1" si se realiza la inversion i, "0" si no se realiza.
 var y    >= 0  ;       ## total de la inversion
 
  ## variables auxiliares
 var d >= 0,  binary;
 var e <= 1,  binary; 
 var f <= 1,  binary; 
 var g >= 0,  binary; 
 var h >= 0,  binary;  
 
###########  FUNCION OBJETIVO

  maximize Z:
  sum { i in I } ( (R[ i ] ) * x[ i ] ) ;
 
###########  RESTRICCIONES
 
# La suma de las inversiones i no debe superar el total M
  subject to Limite_inversion: 
  y <= M;
  
# Se cuantifica en una variable el total de la inversion
  subject to inversion_total:   
  y = sum { i in I } C[ i ] * x[ i ];

# La inversion 2 se realiza solo si la inversion 1 se realiza.
   subject to 2_solo_si_1: 
   x[ 2 ] <= x[ 1 ];

# La inversion 3 se realiza solo si la inversion 2 se realiza.
   subject to 3_solo_si_2: 
   x[ 3 ] <= x[ 2 ];
     
# La inversion 4 se realiza si se realizan las inversiones 1 y 2.
 subject to igualar_x4_d:
  x[ 4 ] = d;
  
  subject to aux_d_x1:
  d <= x[ 1 ];
  
  subject to aux_d_x2:
  d <= x[ 2 ];
 
  subject to aux_d_sumxi:
  d >= x[ 1 ] + x[ 2 ] - (2 - 1);
  
  # La inversión 5 no se realiza si se realizan las inversiones 1 o 2. (NOR)
 subject to negar_x5:
  x[ 5 ] = 1 - e;
  
  subject to e_mayor_x1:
  e >= x[ 1 ];
  
  subject to e_mayor_x2:
  e >= x[ 2 ];
 
  subject to e__menor_sumxi:
  e <= x[ 1 ] + x[ 2 ];
  
# La inversión 6 no se realiza si se realizan las inversiones 2 y 3. (NAND)

 subject to negar_x6:
  x[ 6 ] = 1 - f;
  
  subject to f_menor_x2:
  f <= x[ 2 ];
  
  subject to f_menor_x3:
  f <= x[ 3 ];
 
  subject to f_mmayor_sumxi:
  f >= x[ 2 ] + x[ 3 ] - (2 - 1);;
    
# La inversión 7 se realiza solo si realiza la inversion 2 y no la 3.
  subject to niega_x3:
  g = 1 - x[ 3 ];

  subject to x7_iguala_h:
  x[ 7 ] = h;
  
  subject to h_menor_x2:
  h <= x[ 2 ] ;
  
  subject to h_menor_g:
  h <= g ;
  
  subject to suma_h:
  h >= x[ 2 ] + g - (2 - 1);
  
  
  
 