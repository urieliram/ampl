var X1, >=0 ;
var X2, >=0 ;
var X3, >=0 ;

maximize Z : 2 * X1 + 1 * X2 - 1 * X3 ;

subject to res1: 5*X1  -  2*X2  +  8*X3  <=  15;
subject to res2: 8*X1  +  3*X2  -  1*X3  >=   9;
subject to res4: 7/8 <= X1 <= 9/5;
subject to res5: 0   <= X2 <= 1;
subject to res6: 1   <= X3 <= 101/64;

# Z = 3.6
# X1 = 1.8
# X2 = 1
# X3 = 1
