var y1, integer, >=0,<=1;
var y2, integer, >=0,<=1;
var zb;
param u1 =8;
param u2 =5;
param u3 =1;

#PRIMAL
maximize Z:  zb;

subject to res1: zb <= (6-2*y1-y2)*u1 + (10-y1-2*y2)*u2 + (5-y1-y2)*u3  +3*y1 +20*y2;
