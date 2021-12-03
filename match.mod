var x1, >=0;
var x2, >=0;
var x3, >=0;
var x4, >=0;
var x5, >=0;
var x6, >=0;
var x7, >=0;
var x8, >=0;
var x9, >=0;
var x10, >=0;
var x11, >=0;


#PRIMAL
maximize Z: 1*x1 + 4*x2 + 7*x3 + 3* x4 + 5*x5 + 6*x6 + 3*x7 + 1*x8 + 2*x9 + 9*x10 +6*x11 ;

#maximize Z: x1 +x2 + x3+ x4+ x5 +x6+x7+x8+x9+x10+x11 ;
subject to dua1:    x1+x2        <= 1 ;
subject to dua2:    x3+x4+x5     <= 1 ;
subject to dua3:    x6+x7        <= 1 ;
subject to dua4:    x8           <= 1 ;
subject to dua5:    x9           <= 1 ;
subject to dua6:    x10+x11      <= 1 ;
subject to dua7:    x6           <= 1 ;
subject to dua8:    x3           <= 1 ;
subject to dua9:    x1+x8        <= 1 ;
subject to dua10:   x4           <= 1 ;
subject to dua11:   x2+x5+x9+x10 <= 1 ;
subject to dua12:   x7+x11       <= 1 ;


#DUAL
#minimize G:            u2 + 5 * u3 + 1 * u4 - 2 * u5;

#subject to res1:                 x2  <= 3 ;
#subject to res2:  - 1 * x1 + 1 * x2  <= 1 ;
#subject to res3:  + 1 * x1 + 1 * x2  <= 5 ;
#subject to res4:  + 1 * x1 + 1 * x2  >= 1 ;
#subject to res5:  - 1 * x1 + 1 * x2  >= -2 ;

#DUAL
