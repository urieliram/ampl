var x1, >=0,binary;
var x2, >=0,binary;
var x3, >=0,binary;
var x4, >=0,binary;
#PRIMAL
minimize Z:  40 * x1 + 60 * x2 + 70 * x3 + 160 * x4 ;
subject to res1: 16 * x1 + 35 * x2 + 45 * x3 + 85 * x4 >= 81 ;
subject to res2: x1 <= 1 ;
subject to res3: x2 <= 1 ;
subject to res4: x3 <= 1 ;
subject to res5: x4 <= 1 ;

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