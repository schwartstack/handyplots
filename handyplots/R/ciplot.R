ciplot <-
function(x, y, x0 = NULL, int = c("p","c"), level = 0.95, relationship = c("linear","quadratic","cubic","sqrt","exponential","reciprocal","log"), show.range = TRUE, user.xlim = NULL, user.ylim = NULL){
  
  if(all(int==c("p","c"))){
    int="p" #default 'int' value
  }
  if(all(relationship==c("linear","quadratic","cubic","sqrt","exponential","reciprocal","log"))){
    relationship="linear" #default 'relationship' value
  }
  
  error=""
  
  if((class(x)!="numeric"&&class(x)!="integer")||(length(x)<4)){
    error=paste(error,"Error: x must be a numeric vector of length > 3",sep="\n")
  }
  
  if((class(y)!="numeric"&&class(y)!="integer")||(length(y)<4)){
    error=paste(error,"Error: y must be a numeric vector of length > 3",sep="\n")
  }
  
  if(length(x)!=length(y)){
    error=paste(error,"Error: x and y must be of equal length",sep="\n")
  }
  
  xlab=deparse(substitute(x)) #ensures the x label matches the input variable name
  ylab=deparse(substitute(y)) #ensures the y label matches the input variable name
  y=y[order(x,decreasing = T)]
  x=sort(x,decreasing = T) #if I don't sort x, weird things happen when I try to draw the regression line for non-linear relationships
  
  if(length(x0)>1||(!is.null(x0)&&class(x0)!="numeric")){
    error=paste(error,"Error: 'x0' must be a length-1 numeric vector or left blank",sep="\n")
  }
  
  if(length(int)!=1 || class(int)!="character" || (!startsWith("prediction",int)&&!startsWith("confidence",int))){
    error=paste(error,"Error: invalid interval type. Try 'prediction' or 'confidence'",sep="\n")
  }
  
  if(length(level)>1||class(level)!="numeric"||level<=0||level>=1){
    error=paste(error,"Error: 'level' must be a number greater than 0 and less than 1",sep="\n")
  }
  
  if(class(show.range)!="logical"||length(show.range)!=1){
    error=paste(error,"Error: 'show.range' must be a length-1 logical vector",sep="\n")
  }
  
  if(is.null(user.xlim)){
  }else if(length(user.xlim)!=2||class(user.xlim)!="numeric"){
    error=paste(error,"Error: 'user.xlim' must be a length-2 numeric vector or left blank",sep="\n")
  }else if(any(is.na(user.xlim))||any(is.infinite(user.xlim))||any(is.nan(user.xlim))){
    error=paste(error,"Error: need finite 'user.xlim' values",sep="\n")
  }
  
  if(is.null(user.ylim)){
  }else if(length(user.ylim)!=2||class(user.ylim)!="numeric"){
    error=paste(error,"Error: 'user.ylim' must be a length-2 numeric vector or left blank",sep="\n")
  }else if(any(is.na(user.ylim))||any(is.infinite(user.ylim))||any(is.nan(user.ylim))){
    error=paste(error,"Error: need finite 'user.ylim' values",sep="\n")
  }
  
  model=switch(relationship,
               "lin"=lm(y~x),"linear"=lm(y~x),
               "squared"=lm(y~poly(x,degree=2,raw=T)),"quadratic"=lm(y~poly(x,degree=2,raw=T)),"quad"=lm(y~poly(x,degree=2,raw=T)),
               "cube"=lm(y~poly(x,degree=3,raw=T)),"cubic"=lm(y~poly(x,degree=3,raw=T)),
               "sqrt"=lm(y~sqrt(x)),"squareroot"=lm(y~sqrt(x)),
               "e"=lm(y~exp(x)),"exp"=lm(y~exp(x)),"exponential"=lm(y~exp(x)),
               "recip"=lm(y~I(1/x)),"reciprocal"=lm(y~I(1/x)),
               "log"=lm(y~log(x)),"ln"=lm(y~log(x)),"logarithmic"=lm(y~log(x)),"logarithm"=lm(y~log(x)))
  if(is.null(model)){
    error=paste(error,"Error: invalid relationship type. Try 'linear', 'quadratic','sqrt','exp','reciprocal', or 'log'",sep="\n")
  }
  
  if(error!=""){
    return(cat(error,"\n"))
  }
  
  prediction=NULL
  if(!is.null(x0)){
    prediction=predict(model,data.frame(x=x0),int=int,level=level) #get prediction for yhat if x0 is set
  }
  if(is.null(user.xlim)){
    user.xlim=c(min(c(x,x0)),max(c(x,x0))) #default x limits
  }
  if(is.null(user.ylim)){
    user.ylim=c(min(c(y,prediction)),max(c(y,prediction))) #default y limits
  }
  title="" #if x0 is NULL and show.range is FALSE, the plot will have no title
  if(!is.null(x0)||show.range){
    title="Interval"
    if(startsWith("prediction",int)){
      title=paste("Prediction",title)
    }else{
      title=paste("Confidence",title)
    }
    if(!is.null(x0)){
      title=paste(title,", x0 = ",x0,sep="")
    }
    title=paste(title,", level = ",level,sep="")
    if(!is.null(x0)){
      title=paste(title,"\n","yhat = ",signif(prediction[1])," \U00B1 ",signif(prediction[1]-prediction[2]),sep="") #putting the prediction range of yhat in the title of the plot
    }
  }
  plot(x,y,xlim=user.xlim,ylim=user.ylim,pch=16,cex=.5,main=title,xlab=xlab,ylab=ylab) #main plot
  lines(x,predict(model)) #plot regression line (or curve)
  if(!is.null(x0)){  
    points(x0,prediction[1]) #plot yhat (if x0 is set)
    if(x0<min(x)){ #if x0 is outside the domain of x, this will draw a dotted line to extrapolate the data to the point (x0,yhat)
      s=seq(min(c(user.xlim,x0)),min(x),length.out = 100)
      lines(s,predict(model,data.frame(x=s)),lty="dotted")
    }else if(x0>min(x)){
      s=seq(max(x),max(c(user.xlim,x0)),length.out = 100)
      lines(s,predict(model,data.frame(x=s)),lty="dotted")
    }
    lines(c(x0,x0),c(prediction[2],prediction[3]),lwd=3,col="darkred") #draw vertical line which is the prediction/confidence interval of yhat (if x0 is set)
  }
  if(show.range){ #if show.range is TRUE, draw red dashed lines to show the confidence/prediction interval for the entire plot, not just at x0
    range=seq(min(c(x,x0,user.xlim)),max(c(x,x0,user.xlim)),length.out=100)
    lines(range,predict(model,data.frame(x=range),int=int,level=level)[,2],lty="dashed",col="red")
    lines(range,predict(model,data.frame(x=range),int=int,level=level)[,3],lty="dashed",col="red")
  }
}
