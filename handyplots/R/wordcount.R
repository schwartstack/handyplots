wordcount <-
function(file="",n,decreasing=T,text){
  if((n<1)||(round(n)!=n)){
    return("Error: n must be a positive integer")
  }

  punctuation=c("`","~","!","@","#","$","%","^","&","*","(",")","_","+","=","{","[","}","]","|","\\",":",";","\"","<",",",">",".","?","/","'s")
  if(file!=""){
  wordlist=scan(file,what=character())
  }else{
    wordlist=strsplit(text," ")[[1]]
  }
  for(i in 1:length(punctuation)){
    wordlist=gsub(punctuation[i],"",wordlist,fixed=T)
  }
  wordlist=unlist(strsplit(wordlist,"-"))
  wordlist=tolower(wordlist)
  if(length(which(wordlist==""))>0)
  {
    wordlist=wordlist[-c(which(wordlist==""))]
  } 
  
  if(n>length(unique(wordlist))){
    n=length(unique(wordlist))
  }
  
  df=data.frame("Word"=sort(unique(wordlist)),"Count"=rep(0,length(unique(wordlist))))
  for(i in 1:length(unique(wordlist))){
    df[i,2]=length(which(wordlist==df[i,1]))
  }
  df=df[order(df[,2],decreasing = T),]
  df=df[1:n,]
  if(!decreasing){
  df=df[order(df[,2],decreasing = F),]
  }
  row.names(df)=1:nrow(df)
  
  barplot(df[,2],names.arg=df[,1])
}
