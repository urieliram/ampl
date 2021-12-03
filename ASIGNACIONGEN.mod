var x11, >=0,<=1;
var x12, >=0,<=1;
var x21, >=0,<=1;
var x22, >=0,<=1;
var x31, >=0,<=1;
var x32, >=0,<=1;

#PRIMAL
minimize Z:  9 * x11 + 2* x12 + 1 * x21 + 2 * x22 + 3 * x31 + 8 * x32 ;
subject to dual1:  x11 + x12 = 1 ;
subject to dual2:  x21 + x22 = 1 ;
subject to dual3:  x31 + x32 = 1 ;
subject to dual4: 6*x11 + 7*x21  + 9*x31 <= 13 ;
subject to dual5: 8*x12 + 5*x22  + 6*x32 <= 11 ;
