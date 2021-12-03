# museum.mod
# file:///C:/Users/uriel/Downloads/Liberti_Easy_modelling_problems_solutions_exercises-solutions.pdf
# pagina 80

param n >= 0, integer;
set V := 1..n;
set E within {V,V};
var x{E} binary;
minimize cost : sum{(i,j) in E} x[i,j];
subject to vertexcover {i in V} :
sum{j in V : (i,j) in E} x[i,j] + sum{j in V : (j,i) in E} x[j,i] >= 1;
