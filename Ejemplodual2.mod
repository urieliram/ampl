var y1, >=0 ;
var y2, >=0 ;
var y3, >=0 ;
var y4, >=0 ;
var y5, >=0 ;
var y6, >=0 ;

minimize Z : 17*y1 - 6*y2 - 0.875*y3 + 1.8*y4 -	y5 + 1.578*y6;

subject to res1:
5*y1 - 8*y2	- y3 + y4 >= 2;

subject to res2:
8*y1 +   y2            - y5 + y6 >= -1;

subject to res3:
y1 +   y2            = -1;


#Z = 2.6
#y1 = 0
#y2 = 0
#y3 = 0
#y4 = 2
#y5 = 1
#y6 = 0
#res1 = 1.8
#res2 = 1