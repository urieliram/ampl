var x1, >=0, binary;
var x2, >=0, binary;
var x3, >=0, binary;
var x4, >=0, binary;
param u =32/17;

#PRIMAL
#minimize Z:  40 * x1 + 60 * x2 + 70 * x3 + 160 * x4 ;
#subject to res1: 16 * x1 + 35 * x2 + 45 * x3 + 85 * x4 >= 81 ;


#LAGRANGE
minimize Z:  40 * x1 + 60 * x2 + 70 * x3 + 160 * x4+ u*(-16 * x1 - 35 * x2 - 45 * x3 - 85 * x4 + 80) ;
