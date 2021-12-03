var u1, >=0;
var u2, >=0;
var u3, >=0;
param y1 =0;
param y2 =1;

#PRIMAL
minimize Z: (6-2*y1-y2)*u1+(10-y1-2*y2)*u2+(5-y1-y2)*u3+3*y1+20*y2;

subject to res1: 3 *   u1  >= 1 ;
subject to res2:         u1  >= 8 ;
subject to res3:         u1  >= 2 ;
subject to res4:         u2  >= 3 ;
subject to res5:         u2  >= 5 ;
subject to res6:         u2  >= 1 ;
subject to res7: 2 *   u3  >= 2 ;
subject to res8: 3 *   u3  >= 1 ;

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