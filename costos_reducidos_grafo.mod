var pi1, <=0;
var pi2, <=0;
var pi3, <=0;
var pi4, <=0;
var pi5, <=0;

var x12, >=0;
var x13, >=0;
var x32, >=0;
var x24, >=0;
var x35, >=0;
var x54, >=0;

###########  FUNCION OBJETIVO

maximize W:
    20*pi1*pi1 - 15*pi2 + 5*pi3 - 10*pi4 + 0*pi5 ;
     
subject to dua1: - pi1 + pi2 <=   5  ;
subject to dua2: - pi1 + pi3 <=   3  ;
subject to dua3: - pi3 + pi2 <=  -1  ;
subject to dua4: - pi2 + pi4 <=  -2  ;
subject to dua5: - pi3 + pi5 <=  10  ;
subject to dua6: - pi5 + pi4 <=   2  ;


#minimize Z:
#    5*x1 + 3*x2  -1*x3 - 2*x4 + 10*x5 + 2*x6 ;
   
#subject to dua1:  x1 - x2 <=   ;