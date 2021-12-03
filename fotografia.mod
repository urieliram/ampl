#######  #######  #######  #######  #######  #######  #######  #######
#######  Programadores: Gabriela                               #######
#######  Programadores: Julio César                            #######
#######  Programadores: Uriel Iram Lezama Lope                 #######
#######  Posgrado en Ing Sistemas, PISIS, UANL				   #######
#######  #######  #######  #######  #######  #######  #######  #######
#######  (Omision Intencional de Acentos)

###########  CONJUNTOS

  set P;    # Conjunto de fotografías candidatas, indice p

###########  PARAMETROS

  param M ; 	      ## Capacidad 
  param g {P} ; 	  ## Beneficio asociado a cada fotografía pi
  param R {P} ;       ## Tamanio asociado a cada fotografía pi.
    
###########  VARIABLES

 var x{I} >= 0 , binary ; ## "1" si se realiza la inversion i, "0" si no se realiza.
 var y    >= 0 ;          ## total de la inversion
 

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
 subject to se_hara_si_1_y_2:
  2 * x[ 4 ] <= x[ 1 ] + x[ 2 ];
  
  # La inversión 5 no se realiza si se realizan las inversiones 1 o 2. (NOR)
 subject to no_si_1_o_2:
  2 - ( x[ 1 ] + x[ 2 ] ) >= 2 * x[ 5 ];
 
# La inversión 6 no se realiza si se realizan las inversiones 2 y 3. (NOR)
 subject to no_si_2_y_3:
  x[ 6 ] <= 2 - ( x[ 2 ] + x[ 3 ] );
     
# La inversión 7 se realiza solo si realiza la inversion 2 y no la 3.
  subject to solo_si_2_y_no_3:
  2 * x[ 7 ] <= x[ 2 ] - x[ 3 ] + 1 ; 