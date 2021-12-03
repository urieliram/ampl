var x1, >=0;
var x2, >=0;
var x3, >=0;
var x4, >=0;
var x5, >=0;
var x6, >=0;
var x7, >=0;
var x8, >=0;
var y1, integer, >=0,<=1;
var y2, integer, >=0,<=1;

#PRIMAL
maximize Z:  x1 + 8 * x2 + 2 * x3 + 3 * x4 +5* x5 + x6 + 2 * x7 + x8 + 3 * y1 + 20 * y2 ;

subject to res1:   3 *  x1  +     x2  +        x3 +  2*y1  +      y2    <= 6  ;
subject to res2:         x4   +    x5  +   4 * x6 +     y1  + 2 * y2    <= 10  ;
subject to res3:   2 *  x7  + 3*x8  +                     y1  +  y2        <= 5  ;

#subject to res2:  1 * x1 + 2 * x2  <= 8 ;

#DUAL
#minimize G: 2 * u1 + 8 * u2 ;

#subject to dua1: -2 * u1 + 1 * u2  >= 3  ;
#subject to dua2:  1 * u1 + 2 * u2  >= 2 ;

#param L := 0;
#param M := -99999999999;

#PRIMAL
#maximize Z: x1 + 3 * L * x2 ;

#subject to res1:   - M  * x1 - x2  <= -4 ;
#subject to res2:  2     * x1 - x2  <=  2 ;
#subject to res3:  3 * M * x1 - x2  <=  8 ;

#DUAL
#minimize G: -4 * u1 + 2 * u2 + 8 *** u3;

#subject to dua1: -M * u1 + 2 * u2  + 3 * M * u3 <= 1  ;
#subject to dua2: -    u1 -     u2  -     u3     <= 3 * L ;

#DUAL
#minimize G: 2 * u1 + 8 * u2 ;

#subject to dua1: -2 * u1 + 1 * u2  >= 3  ;
#subject to dua2:  1 * u1 + 2 * u2  >= 2 ;