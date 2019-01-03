quickplot <-
function(x,y){
  
  error=""
  
  if((class(x)!="numeric"&&class(x)!="integer")||(length(x)<4)){
    error=paste(error,"Error: x must be a numeric vector of length > 3",sep="\n")
  }
  
  if((class(y)!="numeric"&&class(y)!="integer")||(length(y)<4)){
    error=paste(error,"Error: y must be a numeric vector of length > 3",sep="\n")
  }
  
  if(length(y)!=length(x)){
    error=paste(error,"Error: x and y must be numeric vectors of equal length",sep="\n")
  }
  
  if(error!=""){
    return(cat(error,"\n"))
  }
  
  default=par("mfrow")
  par(mfrow=c(2,2))
  plot(x,y,main="Scatter Plot",xlab=deparse(substitute(x)),ylab=deparse(substitute(y)))
  abline(lm(y~x))
  qqnorm(rstudent(lm(y~x)))
  qqline(rstudent(lm(y~x)))
  resplot(lm(y~x))
  boxplot(x,y,names=(c(deparse(substitute(x)),deparse(substitute(y)))),main="Box Plots")
  par(mfrow=default)
}
