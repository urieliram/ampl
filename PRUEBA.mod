var x1, >=0;
var x2, >=0;

#minimize G: 4u1 +  u2 ;
#subject to dua1 : 2 * u1 - 1 * u2           >= 1   ;
#subject to dua2 :        + 1 * u2 + 1 * u3  >= 3   ;

#PRIMAL
minimize Z: 18 * x1 + 28 * x2+8*(4.9) ;

subject to res1: 4.76923 * ( 6 - 2 * x1 ) +3.69231  * ( 10 - x1 )  <= x2  ;
#subject to res2: 2 * x1 + 5 * x2  <= 28 ;

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
#minimize G: -4 * u1 + 2 * u2 + 8 * u3;

#subject to dua1: -M * u1 + 2 * u2  + 3 * M * u3 <= 1  ;
#subject to dua2: -    u1 -     u2  -     u3     <= 3 * L ;

#DUAL
#minimize G: 2 * u1 + 8 * u2 ;

#subject to dua1: -2 * u1 + 1 * u2  >= 3  ;
#subject to dua2:  1 * u1 + 2 * u2  >= 2 ;