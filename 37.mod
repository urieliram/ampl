var X1, >=0 integer;
var X2, >=0 integer;
var X3, >=0 integer;
var X4, >=0 integer;
var X5, >=0 integer;

maximize Z : - 40* X1 + 40* X2 + 60* X3 + 130 *X4 + 90* X5;

subject to res1: 0.5*X1  -X2    -X4           = 0;
subject to res2: 0.5*X1      -X3        -X5   = 0;
subject to res3:     X1                      <= 20;
subject to res4:             X4     +.75*X5  <= 8;



#subject to res1: 2 * x1 + 3 * x2 + 1 * x3 <= 5  ;
#subject to res2: 4 * x1 + 1 * x2 + 2 * x3 <= 11 ;
#subject to res3: 3 * x1 + 4 * x2 + 2 * x3 <= 8  ;




