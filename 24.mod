
reset;

set C; #COLONIA
set E; #EXTRACTO

#param a {j in P};
#param b; #
param c {i in C};
param d {j in C};
param e {i in C, j in E};

var x {i in C};

maximize profit : sum {i in C} c[i]*x[i];


subject to disponible {j in E}: (sum {i in C} e[j,i])*x[i] <= d[j]; #disponible

