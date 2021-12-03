var x1, >=0;
var x2, >=0;
var x3, >=0;

#PRIMAL
maximize Z: 4 * x1 + 2 * x2 + 1 * x3 ;

subject to n1:   +     x1               <= 5   ;
subject to n2:   + 4 * x1 +     x2      <= 25  ;
subject to n3:   + 8 * x1 + 4 * x2 + x3 <= 125 ;

