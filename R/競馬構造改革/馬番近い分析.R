
mokuteki<-cbind(UmahukuMat[,1:3] ,matrix(50,length(UmahukuMat[,1]),1))

for(i in 1:length(UmahukuMat[,1])){
  for(j in 1:30){
	if(UmahukuMat[i,"馬複馬番1"] == UmahukuMat[i,3 * j + 85] &&
	  UmahukuMat[i,"馬複馬番2"] == UmahukuMat[i,3 * j + 86]){
		mokuteki[i,4] <- j
	}
  }
}

plot(mokuteki[,4],UmahukuMat[,"X1着賞金"]/1000000)

hist(mokuteki[,4], breaks = "Scott", freq = FALSE)
lines(density(mokuteki[,4]), col = "orange")

table(mokuteki[,4]>30)
table(UmahukuMat[,"X1着賞金"])