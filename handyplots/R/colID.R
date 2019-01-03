colID <-
function(df){
  if(class(df)!="data.frame"){
    return("Error: input must be a data frame.")
  }
  mat=data.frame(row.names = 1:ncol(df))
  mat[,1]=colnames(df)
  mat[,2]=sapply(df,class)
  colnames(mat)=c("colname","class")
  return(mat)
}
