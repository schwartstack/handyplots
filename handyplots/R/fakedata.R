fakedata <-
function(formula,s=.25){
  y=formula
  error=rnorm(length(y),0,s*sd(y))
  return(y+error)
}
