var x1, >=0;
var x2, >=0;

var u1, >=0;
var u2, >=0;
var u3, >=0;
var u4, <=0;
var u5, <=0;

#PRIMAL
#maximize Z: 0.1 * x1 + x2 ;

#subject to res1:                 x2  <= 3 ;
#subject to res2:  - 1 * x1 + 1 * x2  <= 1 ;
#subject to res3:  + 1 * x1 + 1 * x2  <= 5 ;
#subject to res4:  + 1 * x1 + 1 * x2  >= 1 ;
#subject to res5:  - 1 * x1 + 1 * x2  >= -2 ;

#DUAL
minimize G: 3 * u1 + u2 + 5 * u3 + u4 -2 * u5;

subject to dua1:           -1 * u2 + 1 * u3 + 1 * u4 -1 * u5 >= 0.1  ;
subject to dua2:   1 * u1 + 1 * u2 + 1 * u3 + 1 * u4 +1 * u5 >= 1 ;

#DUAL
#minimize G:            u2 + 5 * u3 + 1 * u4 - 2 * u5;

#subject to dua1: - 1 * u2 + 1 * u3 + 1 * u4 - 1 * u5 >= 0.1  ;
#subject to dua2: + 1 * u2 + 1 * u3 + 1 * u4 + 1 * u5 >= 1 ;