var y1, >=0 ;
var y2, >=0 ;
var y3, >=0 ;
var y4, >=0 ;
var y5, >=0 ;
var y6, >=0 ;
var y7, >=0 ;

minimize Z : 15*y1 - 9*y2 - 0.875*y3 + 1.8*y4 +	y5 - y6	+ 1.578*y7;

subject to res1:
5*y1 - 8*y2	- y3 + y4 >= 2;

subject to res2:
-2*y1 -	3*y2 + y5 >= 1;

subject to res3:
8*y1 + y2 - y6 + y7 >= -1;

#Z = 7.5
#y1 = 0.5
#y2 = 0
#y3 = 0
#y4 = 0
#y5 = 0
#y6 = 0
#y7 = 0
