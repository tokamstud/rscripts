myfunc <- function (a,b,c){

  x <- (-b)+sqrt((-b^2)+4*a*c)/2*a;
  y <- (-b)-sqrt((-b^2)+4*a*c)/2*a;

  return(c(x,y));
}

print(myfunc(2,2,2))
