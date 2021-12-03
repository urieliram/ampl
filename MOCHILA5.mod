var x1,  >=0, integer;
var x2,  >=0, integer;

#PRIMAL
maximize Z:  42-2*x2-5*x1;
subject to dual1: x1 + 4 * x2 <= 28 ;
#subject to dual2: 1 * x1 + 4 * x2 <= 28 ;
#subject to dual3: 1 * x1 + 1 * x2 <= 6 ;