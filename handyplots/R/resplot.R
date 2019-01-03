resplot <-
function(model, zoom = NULL, highlight.outliers = FALSE, residuals = c("student","standard")){
  
  if(all(residuals==c("student","standard"))){
    residuals="student"
  }
  
  error=""
  
  if(class(model)!="lm"&&class(model)!="glm"){
    error=paste(error,"Error: 'model' must be object of class lm or glm",sep="\n")
  }
  
  if(class(highlight.outliers)!="logical"||length(highlight.outliers)!=1){
    error=paste(error,"Error: 'highlight.outliers' must be a length-1 logical vector",sep="\n")
  }
  
  y=switch(residuals,"student"=rstudent(model),"standard"=rstandard(model),"studentized"=rstudent(model),"standardized"=rstandard(model),"rstudent"=rstudent(model),"rstandard"=rstandard(model))
  if(is.null(y)){
    y=NA #change y from NULL to NA to remove warning message when trying to set lim
    error=paste(error,"Error: not a valid residual type. Try: 'student' or 'standard'",sep="\n")
  }
  
  if(is.null(zoom)){
    lim=c(min(y),max(y)) #default
  }else if(class(zoom)=="numeric"&length(zoom)==1){
      lim=c(-zoom,zoom)
    }else{
      error=paste(error,"Error: 'zoom' must be a single numeric value or left blank",sep="\n")
    }
  
  if(error!=""){
    return(cat(error,"\n"))
  }
  
  x=predict(model)
  lab=switch(residuals,"student"="Studentized Residuals","standard"="Standardized Residuals","studentized"="Studentized Residuals","standardized"="Standardized Residuals","rstudent"="Studentized Residuals","rstandard"="Standardized Residuals")
  plot(x,y,main="Residual Plot",xlab="Predicted Values",ylab=lab,pch=16,cex=.4,ylim=lim)
  abline(0,0)
  abline(1,0,lty=2)
  abline(-1,0,lty=2)
  abline(3,0,lty=3)
  abline(-3,0,lty=3)
  if(highlight.outliers){
    points(predict(model)[which(rstudent(model)>3)],rstudent(model)[which(rstudent(model)>3)],col="red",cex=.5)
    points(predict(model)[which(rstudent(model)<(-3))],rstudent(model)[which(rstudent(model)<(-3))],col="red",cex=.5)
  }
}
