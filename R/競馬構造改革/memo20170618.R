
sub<-function(x){
  selected<-subset(UmahukuMat,
    eval(parse(text=x ))  
  )
 return(selected)
}

sumBFkin<-function(x){
 return(sum(x["”n•¡‹àŠz"]))
}
len<-function(x){
 return(length(x[,1]))
}

#------------------------------
BFKekkaNeraiStart <- 14
BFKekkaNeraiEnd <- 29
t<-proc.time()
sub1<-lapply(joken,sub)
checked <- lapply(sub1,kekkaCheck2,BFKekkaNeraiStart ,BFKekkaNeraiEnd )
sums<- lapply(checked,sumBFkin)
lens <- lapply(checked,len)
riekiritsu<-sums /(lens * (BFKekkaNeraiEnd -BFKekkaNeraiStart -1)*100)
proc.time()-t